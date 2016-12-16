if(!isGeneric("apply.fun")) {
  setGeneric("apply.fun",function(x,f,...){
    standardGeneric("apply.fun")
  })
}

.appendUserDefinedRequiredPackages = function(commands,packages,logfile) {
  package.list = as.list(packages)
  # package.require.statement = paste("require(",package.list,")",sep="")
  # commands = append(commands,package.require.statement)

  if (!missing(logfile)){
    commands = append(commands, paste(lapply(package.list,.requireInstallPackage,logfile=logfile),sep=""))
  } else {
    commands = append(commands, paste(lapply(package.list,.requireInstallPackage),sep=""))
  }

  return(commands)
}

.appendUserDefinedFunctionCode = function(commands,f) {
  #TODO if function is defined in a package, then we must remove some information that are not part of the function
  # e.g. <bytecode: ...> or <environment: ...>; also check whether or not the function is S3 or S4 (generic)
  # in principle we should allow just those functions that are defined in the script and are not part of a package

  f.character = paste(capture.output(f))
  commands = append(commands,paste("f <- ",f.character[1],sep=""))
  f.end = length(f.character)
  if (f.end > 1) {
    commands = append(commands,f.character[2:f.end])
  }
  return(commands)
}

# commands: vector of string
# attributes: vector of string with attribute names
.appendChunkDataFrameDefinition = function(commands,attributes) {
  #use attribute names to create a statement for creating a dataframe
  df.attributes = paste(attributes,"=",attributes,collapse=",",sep="")
  commands = append(commands,paste("df = data.frame(",df.attributes,")",sep=""))
  return(commands)
}

.appendOutputTypeConversionForR = function(commands,output) {
  if (is.list(output)) {
    attr.names = names(output)
    attr.types = unlist(output)
    names(attr.types) = c()
    commands = append(commands,paste("list(",paste(attr.names,"=as(func.result$",attr.names,",\"double\")",sep="",collapse=","),")",sep=""))
    return(commands)

  } else {
    stop("Output of the processed chunk was not defined.")
  }
}

.log = function(s, logfile,quote=TRUE) {
  if (quote) {
    return(sprintf("cat(\"%s\n\", file=\"%s\",append=TRUE)",s,logfile))
  } else {
    return(sprintf("cat(%s, file=\"%s\",append=TRUE)",s,logfile))
  }

}

.requireInstallPackage = function(lib,logfile) {
  statement = gsub("%s", lib, "if (!require(%s)) {\n %o install.packages(\"%s\",repos=\"https://cloud.r-project.org/\")\n library(%s)\n}")
  if (!missing(logfile)) {
    statement = gsub("%o", paste(.log(sprintf("installing package %s on instance",lib),logfile),"\n",sep=""),statement)
  } else {
    statement = gsub("%o ", "", statement)
  }

  return (statement)
}

.rexec.query = function(x,f,array,packages,parallel=FALSE,cores=1,aggregates,output, logfile, ...) {
  logging = !missing(logfile)
  noPackages = missing(packages) || is.null(packages) || length(packages) == 0

  commands=c()
  attr = scidb::scidb_attributes(x)

  if (!all(aggregates %in% attr)) {
    stop("Cannot find the attributes to aggregate over in the functions data input.")
  }

  # load required packages
  if (!noPackages) {
    commands = .appendUserDefinedRequiredPackages(commands,packages)
  }

  # correct negative amount of cores
  if (cores < 1) cores = 1

  # register package doParallel if parallel == TRUE
  if (parallel && cores > 1) {
    # append parallel setting
    if (!missing(packages)) {
      if (!all("doParallel" %in% packages)) {
        commands = append(commands,.requireInstallPackage("doParallel"))
      }
    } else {
      commands = append(commands,.requireInstallPackage("doParallel"))
    }

    commands = append(commands,paste("registerDoParallel(cores=",cores,")",sep=""))
  }

  # get the code of the function as text
  if (!missing(f) && is.function(f)) {
    commands = .appendUserDefinedFunctionCode(commands,f)
  } else {
    stop("No function was defined to operate on a chunk.")
  }


  # create the data.frame
  commands = .appendChunkDataFrameDefinition(commands,attr)
  # TODO option to calculate coordinates or timestamps to create or work with spatial or spatio-temporal objects


  # use packages plyr and foreach as mandatory packages
  if (!noPackages) {
    if (!all("plyr" %in% packages)) {
      if (logging) {
        commands = append(commands,.requireInstallPackage("plyr",logfile = logfile))
      } else {
        commands = append(commands,.requireInstallPackage("plyr"))
      }
    }
    if (!all("foreach" %in% packages)) {
      if(logging) {
        commands = append(commands,.requireInstallPackage("foreach",logfile = logfile))
      } else {
        commands = append(commands,.requireInstallPackage("foreach"))
      }

    }
  } else {
    if (logging) {
      commands = append(commands,.requireInstallPackage("plyr",logfile = logfile))
      commands = append(commands,.requireInstallPackage("foreach",logfile = logfile))
    }
    else {
      commands = append(commands,.requireInstallPackage("plyr"))
      commands = append(commands,.requireInstallPackage("foreach"))
    }

  }

  # set aggregates statement
  if (missing(aggregates)) {
    aggregates = c()
  }

  # write aggregate expression for the rexec_script
  aggregates.string = paste("c(",paste("\"",aggregates,"\"",collapse=",",sep=""),")",sep="")

  # dot.params = list(...)
  if (logging) {
    commands = append(commands, .log("processing a chunk",logfile))
  }

  #TODO you can pass additional arguments to f after .fun
  ddply_cmd = paste("func.result = ddply(.data=df, .variables=",aggregates.string,", .fun=f ,.parallel=",parallel,")",sep="")

  tc_exec = sprintf("tryCatch({
                            %s
        },error = function(err) {
                            %s
                            df = df[,which(names(df) %%in%% names(output))]
                            var = names(output)[!names(output) %%in%% names(df)]
                            default_values = as.list(rep(0,length(var)))
                            names(default_values) = var

                            .GlobalEnv$func.result = cbind(df,default_values)
        })",
                    ddply_cmd,
                    if (logging) {
                      .log("error in a chunk",logfile)
                    } else { "" })

    commands = append(commands,tc_exec)

    #R_EXEC probably allows in scidb just double values
    #https://github.com/Paradigm4/r_exec/blob/master/LogicalRExec.cpp
    #line 54
    commands = .appendOutputTypeConversionForR(commands,output)



    output.attr.count = length(output)

    # create the afl command
    query.R = sprintf("store(unpack(r_exec(%s,'output_attrs=%i','expr=%s'),i),%s)",
                      x@name,
                      output.attr.count,
                      paste(commands,collapse="\n",sep=""),
                      array)

    return(query.R)
}

# x: scidbst array
# f: user defined function
# array: the array name to store the output of the function under
# parallel: optional parameter whether or not to use one or more cores of an instance
# cores: the amount of cores to use for processing at a single instance
# aggregate: vector of string with the names of the dimensions
# output: a named list of output types, the name of an element corresponds to the attributes used
# dim: a named list with the dimension names of the output, the values are are used if the dimensions are renamed during "redimension",
#      dimension values need to be int64 values user should be aware!
# dim.spec: named list of the specification from dimensions
# method: a string defining the processing mechanism used. possible = "rexec" or "stream"

# TODO ... theoretically this can be passed on to .fun in ddply, but not implemented yet
.apply.scidbst.fun = function(x,f,array,packages,parallel=FALSE,cores=1,aggregates,output,logfile,dim,dim.spec, method="rexec",...) {
    dimMissing = missing(dim)

    # checking the parameter for correctness
    if (!is.list(dim)) {
      if (is.character(dim)) {
      l = length(dim)
      n = dim
      dim = vector("list",l)
      names(dim) = n
      } else {
        stop("Parameter 'dim' is no character vector or named list.")
      }
    }
    if (is.null(names(dim))) {
      stop("Cannot infer dimension names, please state the names by assigning 'names(dim)'")
    }
    # if dim is unlisted later, then null values disappear
    dim = .nullToNA(dim)

    # compare if all dimensions are in the output
    if (!all(names(dim) %in% names(output))) {
      stop("Cannot find dimensions in the output.")
    }
    ###

    attr = scidb_attributes(x)

    if (!all(aggregates %in% attr)) {
      stop("Cannot find the attributes to aggregate over in the functions data input.")
    }

  # create the afl command
  temp_name = .getTempNames(x,1)

  # call this function for scidb array and simply execute the r_exec command
  if (missing(packages)) packages = NULL

  .query = .rexec.query(x=x@proxy,
                     f=f,
                     array=temp_name,
                     packages=packages,
                     parallel=FALSE,
                     cores=cores,
                     aggregates=aggregates,
                     output=output,
                     logfile=logfile,...)

  x@temps = append(x@temps,temp_name)
  iquery(.query)

  out = scidb(temp_name)

  #execute the transform statement
  outlist = .renameAndConvertArray(out,dim,output)
  renamed = outlist$arr
  output = outlist$output

  if (!missing(dim.spec)) {
    redim = .redimensionArray(renamed,dim, dim.spec,output)
  } else {
    # check if dimension names are already in the source array, if yes then pick schema of them

    # create a complete vector of dimension names ( how they are called in the end)
    newDims = unlist(dim)
    newDims[is.na(newDims)] = names(dim)[is.na(newDims)]
    names(newDims) = c()

    if (all(newDims %in% dimensions(x))) {
      # if all potentially renamed dimensions are already used in scidbst array "x", then take the schema from there
      dim.spec = vector("list",length(newDims))
      names(dim.spec) = newDims

      oldDimsSpec = cbind(min=scidb_coordinate_start(x),
                          max=scidb_coordinate_end(x),
                          chunk = scidb_coordinate_chunksize(x),
                          overlap = scidb_coordinate_overlap(x))
      rownames(oldDimsSpec) = dimensions(x)
      for (dimname in newDims) {
        dim.spec[[dimname]] = oldDimsSpec[dimname,]
      }
      redim = .redimensionArray(renamed,dim, dim.spec,output)
    } else {
      #don't redimension, because we don't know what the new dimensions are...
      warning("Cannot redimension the array processed by r_exec. Returned the renamed and projected array.")

      return(project(renamed),names(output))
    }
  }


    #Now we should patch up the spatial and temporal references
    # if the dimensions are named after the original dimension from the source array, then we assume they are the same
    if (x@isSpatial) {
      sdims = srs(x)@dimnames
      if (all(sdims %in% scidb::dimensions(redim))) {

      } else {
        x@isSpatial = FALSE
      }
    }

    if (x@isTemporal) {
      tdim = tdim(x)
      if (all(tdim %in% scidb::dimensions(redim))) {

      } else {
        x@isTemporal = FALSE
        # handle like completely aggregated and leave the extent -> meaning this is the theoretical temporal extent of the array
      }
    }
    x@proxy = redim

    out = scidbsteval(x,array)

    return(out)
    #output values are set to "expr_value_X" with X the poisition in the array
    # https://github.com/Paradigm4/r_exec/blob/master/LogicalRExec.cpp
    # line 76
}

# transform call
.renameAndConvertArray = function(arr, dim, output) {
  # requires dim to be contained completely in output
  # find dimensions in output
  dim.pos = which(names(dim) %in% names(output))
  new.dim.names = unlist(dim[dim.pos])
  renamingDims = new.dim.names[!is.na(new.dim.names)]
  n = names(output)
  n[which(n %in% names(renamingDims))] = renamingDims

  names(output) = n
  #make sure dimension are forced to be int64 values
  output[dim.pos] = "int64"

  expr.attr.name = paste("expr_value_",0:(length(output)-1),sep="")
  transforms = paste(output[],"(",expr.attr.name,")",sep="")
  names(transforms) = names(output)

  # create an argument list for the do.call
  calls = unlist(list(`_data`=arr,transforms))

  renamed = do.call(transform,args=calls)


  return(list(arr=renamed,output=output))
}

.redimensionArray = function(arr,dim,dim.spec,output) {
  dim.pos = which(dim[] %in% names(output) | names(dim) %in% names(output))
  dimensions = output[dim.pos]
  attributes = output[-dim.pos]
  attr.schema = sprintf("<%s>",paste(names(attributes),": ",attributes,sep="",collapse=", "))
  #TODO check if the dimension specification is sufficient
  if (all(names(dim.spec) %in% names(dimensions))) {
    n = names(dim.spec)
    df = sapply(dim.spec,cbind)
    #TODO requires dim.spec list entries to follow the same structure and order
    rownames(df) = names(dim.spec[[1]])
    dim.schema = sprintf("[%s]",paste(colnames(df),"=",df["min",],":",df["max",],",",df["chunk",],",",df["overlap",],sep="",collapse=", "))
  } else {
    stop("Specified dimension do not match the dimension specification")
  }
  schema = sprintf("%s%s",attr.schema,dim.schema)
  redimensioned = redimension(arr, schema = schema)

  return(redimensioned)
}

# finding the dimension after rename:
# dim[] %in% names(output) | names(dim) %in% names(output)

.nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' Applys a custom function on chunks of an array
#'
#' This function applies a custom function on a scidbst array chunk using r_exec.
#'
#' @details The function that can be stated has the following description "function(x,...) {}". The x parameter is a
#' data.frame of the attributes stored in one chunk. In most cases you are advised to transform the array to have the
#' dimension values as attributes if you need those to perform calculations. The function will be passed on to the \link[plyr]{ddply}
#' function.
#'
#' @param x scidbst array
#' @param f function
#' @return scidbst array
#' @export
setMethod("apply.fun",signature(x="scidbst",f="function"), .apply.scidbst.fun)


if(!isGeneric("rexec.scidb")) {
  setGeneric("rexec.scidb",function(x,f,...){
    standardGeneric("rexec.scidb")
  })
}
setMethod("rexec.scidb",signature(x="scidb",f="function"), function(x,f,array,packages,parallel=FALSE,cores=1,aggregates=c(),output, logfile, ...) {
  logging = !missing(logfile)
  noPackages = missing(packages) || is.null(packages) || length(packages) == 0

  if (noPackages) packages = NULL
  if (missing(output)) output = NULL

  if(logging) {
    .script = .rexec.query(x=x@proxy,
                                  f=f,
                                  array=array,
                                  packages=packages,
                                  parallel=FALSE,
                                  cores=cores,
                                  aggregates=aggregates,
                                  output=output,
                                  logfile=logfile,...)
  } else {
    .query = .rexec.query(x=x@proxy,
                                  f=f,
                                  array=array,
                                  packages=packages,
                                  parallel=FALSE,
                                  cores=cores,
                                  aggregates=aggregates,
                                  output=output,...)
  }

  x@temps = append(x@temps,array)
  iquery(.query)

  out = scidb(array)
  return(out)
})
