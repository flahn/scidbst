if(!isGeneric("r.apply")) {
  setGeneric("r.apply",function(x,f,...){
    standardGeneric("r.apply")
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
  func.def = append(c(),paste("f <- ",f.character[1],sep=""))
  f.end = length(f.character)
  if (f.end > 1) {
    func.def = append(func.def,f.character[2:f.end])
  }
  commands = append(commands,paste(func.def,sep="",collapse="; "))
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

# this helper function transfers the spatial reference as constants to the r-script
.appendSpatialReferenceDeclaration = function(commands, obj) {
  if (class(obj) != "scidbst" && !is.spatial(obj)) {
    warning("Stated array has no or incomplete spatial reference. Skipping reference transfer.",call. = FALSE,immediate. = TRUE)
    return(commands)
  }
  # affine transformation
  m = affine(obj)
  affine = sprintf("affine <- matrix(c(%f,%f,%f,%f,%f,%f),nc=3,nr=2)",m[1,1],m[2,1],m[1,2],m[2,2],m[1,3],m[2,3])
  commands = append(commands,affine)

  #crs
  crs.str= crs(obj)@projargs
  crs = sprintf("crs <- CRS(\"%s\")",crs.str)
  commands = append(commands,crs)

  #extent
  ex = extent(obj)
  extent = sprintf("extent <- extent(%f,%f,%f,%f)",ex@xmin,ex@xmax,ex@ymin,ex@ymax)
  commands = append(commands,extent)

  return(commands)
}

.appendTemporalReferenceDeclaration = function(commands, obj) {
  if (class(obj) != "scidbst" && !is.temporal(obj)) {
    warning("Stated array has no or incomplete temporal reference. Skipping reference transfer.",call. = FALSE,immediate. = TRUE)
    return(commands)
  }

  #temporal Extent
  tmin = sprintf("tmin <- as.POSIXlt(\"%s\")",as.character(tmin(obj)))
  tmax = sprintf("tmax <- as.POSIXlt(\"%s\")",as.character(tmax(obj)))
  commands = append(commands,tmin)
  commands = append(commands,tmax)

  #trs
  t0 = sprintf("t0 <- as.POSIXlt(\"%s\")",as.character(t0(obj)))
  tunit = sprintf("tunit <- \"%s\"",tunit(obj))
  tres = sprintf("tres <- %f",tres(obj))
  commands = append(commands,t0)
  commands = append(commands,tunit)
  commands = append(commands,tres)

  return(commands)


}

.log = function(s, logfile,quote=TRUE) {
  if (missing(logfile)) {
    return(sprintf("cat(\"%s\n\")",s))
  }
  if (quote) {
    return(sprintf("cat(\"%s\n\", file=\"%s\",append=TRUE)",s,logfile))
  } else {
    return(sprintf("cat(%s, file=\"%s\",append=TRUE)",s,logfile))
  }

}

# aggregates: the names of the columns in the data.frame to aggregate by
# parallel: ddply option to use parallel processing when aggregating
# df.name: the name of the data.frame used to create the data chunk data.frame
# logging: boolean, whether or not to log
# logfile: character string, where to write the log information into
# output: the key-value list with the output attributes and their type
# f.params: a vector of strings stating the names of the variables defined in the RScript
.createDDPLYCommand = function(aggregates,parallel,df.name,logging,logfile,output,f.params) {
  # write aggregate expression for the rexec_script
  if (length(aggregates > 0)) {
    aggregates.string = paste("c(",paste("\"",aggregates,"\"",collapse=",",sep=""),")",sep="")
  } else {
    aggregates.string = "c()"
  }

  if (missing(f.params) || !is.character(f.params)) {
    f.params = c()
  }

  #add additional parameter to the ddply command
  if (length(f.params) == 0) {
    f.insert = ""
  } else {
    f.insert = paste(paste(f.params,collapse=", ",sep=""),",",sep="")
  }


  #you can pass additional arguments to f after .fun -> f.insert
  ddply_cmd = paste("func.result = ddply(.data=",df.name,", .variables=",aggregates.string,", .fun=f,",f.insert,".parallel=",parallel,")",sep="")

  #.GlobalEnv$func.result = cbind(df,default_values)
  tc_exec = sprintf("tryCatch({ %s },error = function(err) { %s$df$ = $df$[,which(names($df$) %%in%% names(output))]; var = names(output)[!names(output) %%in%% names($df$)]; default_values = as.list(rep(0,length(var))); names(default_values) = var; func.result <<- cbind($df$,default_values); })",
                    sprintf("%s%s%s",
                            if (logging) {
                              paste(.log("executing function on chunk",logfile),"; ",sep="")
                            }else {""},
                            ddply_cmd,
                            if (logging) {
                              paste(.log("function executed successfully",logfile=logfile),";",sep="; ")
                            }else {""}),
                    if (logging) {
                      p1 = paste(.log("error in a chunk",logfile),"; ",sep="")
                      p2= paste(p1,.log("err$message",logfile=logfile,quote=FALSE),"; ",sep="")
                      p2
                    } else { "" })
  tc_exec = gsub(pattern="\\$df\\$",replacement = df.name, tc_exec)

  out.list.def = paste("output = list(",
                       paste(names(output),"=\"",output[],"\"",sep="",collapse=","),
                 "); ",sep="")
  out = append(out.list.def,tc_exec)
  out = append("func.result <- NULL; ",out)
  return (out)
}

# creates the command stack for the RScript
.createRScript = function(x,f,array,packages,parallel=FALSE,cores=1,aggregates,output, logfile, ...) {
  logging = !missing(logfile)
  noPackages = missing(packages) || is.null(packages) || length(packages) == 0

  commands=c()
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

  # transfer spatial and temporal references before using the scidb array
  f.params = c()
  if (class(x) == "scidbst") {
    # using raster functions to describe the spatial reference -> need to include that
    if (!missing(packages)) {
      if (!all("raster" %in% packages)) {
        commands = append(commands,.requireInstallPackage("raster"))
      }
    } else {
      commands = append(commands,.requireInstallPackage("raster"))
    }

    if (is.spatial(x)) {
      commands = .appendSpatialReferenceDeclaration(commands, x)
      f.params = append(f.params,c("affine=affine","crs=crs","extent=extent"))
    }

    if (is.temporal(x)) {
      commands = .appendTemporalReferenceDeclaration(commands, x)
      f.params = append(f.params, c("tmin=tmin","tmax=tmax","t0=t0","tunit=tunit","tres=tres"))
    }

    x = x@proxy
  }


  attr = scidb::scidb_attributes(x)

  if (!all(aggregates %in% attr)) {
    stop("Cannot find the attributes to aggregate over in the functions data input.")
  }

  # create the data.frame
  commands = .appendChunkDataFrameDefinition(commands,attr)

  # get the code of the function as text
  if (!missing(f) && is.function(f)) {
    commands = .appendUserDefinedFunctionCode(commands,f)
  } else {
    stop("No function was defined to operate on a chunk.")
  }

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

  if (logging) {
    commands = append(commands, .log("processing a chunk",logfile))
  }

  tc_exec = .createDDPLYCommand(aggregates = aggregates,
                                parallel = parallel,
                                df.name = "df",
                                logging=logging,
                                logfile = logfile,
                                output = output,
                                f.params = f.params)

  commands = append(commands,tc_exec)

  #R_EXEC probably allows in scidb only double values
  #https://github.com/Paradigm4/r_exec/blob/master/LogicalRExec.cpp
  #line 54
  commands = .appendOutputTypeConversionForR(commands,output)

  return(commands)
}

.createAFLCommand = function(x,output,commands,array) {
  output.attr.count = length(output)


  # create the afl command
  query.R = sprintf("store(unpack(r_exec(%s,'output_attrs=%i','expr=%s'),i),%s)",
                    x@name,
                    output.attr.count,
                    paste(commands,collapse="; ",sep=""),
                    array)



  # clean up! there might be accidentially a semicolon between ) and {
  query.R = gsub(pattern="\\)\\s*(;)\\s*\\{",replacement=") {",x=query.R)
  query.R = gsub(pattern="(;;)",replacement=";",x=query.R)
  query.R = gsub(pattern="(\\{\\s*;)",replacement="{",x=query.R)
  query.R = gsub(pattern="(;\\s*;)",replacement="; ",x=query.R)

  return(query.R)
}

.requireInstallPackage = function(lib,logfile) {
  statement = gsub("%s", lib, "if (! require(%s)) { %o install.packages(\"%s\",repos=\"https://cloud.r-project.org/\"); library(%s)};")
  if (!missing(logfile)) {
    statement = gsub("%o", paste(.log(sprintf("installing package %s on instance",lib),logfile),";",sep=""),statement)
  } else {
    statement = gsub("%o ", "", statement)
  }

  return (statement)
}

# simply creates the RScript and the AFL query
.rexec.query = function(x,f,array,packages,parallel=FALSE,cores=1,aggregates,output, logfile, ...) {

    commands = .createRScript(x=x,
                              f=f,
                              array=array,
                              packages=packages,
                              parallel=parallel,
                              cores=cores,
                              aggregates=aggregates,
                              output=output,
                              logfile=logfile, ...)


    query.R = .createAFLCommand(x=x,
                                output=output,
                                commands=commands,
                                array=array)


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
.apply.scidbst.fun = function(x,f,array,packages,parallel=FALSE,cores=1,aggregates,output,logfile,dim,dim.spec, method="rexec",...) {

    dimMissing = missing(dim)
    dots = list(...)
    if (any("eval" %in% names(dots))) {
      eval = dots$eval
      if (!is.logical(eval)) {
        warning("Eval is not of type logical. Setting eval to FALSE", immediate. = TRUE)
        eval = FALSE
      }
    } else {
      eval = TRUE
    }

    stopAtRScript = FALSE
    stopAtAFLCommand = FALSE
    if (any("result" %in% names(dots))) {
      stopAtRScript = if (toupper(dots$result) == "RSCRIPT") TRUE else FALSE
      stopAtAFLCommand = if (toupper(dots$result) == "AFL") TRUE else FALSE
    }

    if (!dimMissing) {
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



  commands = .createRScript(x=x,
                            f=f,
                            array=temp_name,
                            packages=packages,
                            parallel=parallel,
                            cores=cores,
                            aggregates=aggregates,
                            output=output,
                            logfile=logfile, ...)
  if (stopAtRScript) {
    return(paste(commands,collapse="\n",sep=""))
  }

  #store the calculation under the temporary array name
  .query = .createAFLCommand(x=as(x,"scidb"),
                              output=output,
                              commands=commands,
                              array=temp_name)


  #now the .query has ; to delimit lines. Due to problems of R_EXEC handling this, we need to replace ; with \n
  .query = gsub(";","\n",.query)

  if (stopAtAFLCommand) {
    return(.query)
  }

  x@temps = append(x@temps,temp_name)

  #run the r_exec script
  if (eval) {
    iquery(.query)
  } else {
    return(.query)
  }

  out = scidb(temp_name)


  #execute the transform statement
  if (dimMissing) {
    outlist = .renameAndConvertArray(out,dim=NULL,output)
  } else {
    outlist = .renameAndConvertArray(out,dim,output)
  }

  renamed = outlist$arr
  output = outlist$output

  if (!dimMissing && !missing(dim.spec)) {
    redim = .redimensionArray(renamed,dim, dim.spec,output)
  } else {
    if (missing(dim)) {

      projected = scidbeval(scidb::project(renamed,names(output)),name=array)
      scidbrm(temp_name,force=T)
      return(projected)
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

        return(scidb::project(renamed,names(output)))
      }
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
    if (!is.null(dim)) {
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
    }
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

#' Apply custom R functions on scidbst array chunks
#'
#' This function applies a custom r function on each individual scidbst array chunk using the r_exec interface with
#' SciDB.
#'
#' @aliases r.apply r_exec
#'
#' @note The function that can be stated has the following description "function(x,...) {}". The x parameter is a
#' data.frame of the attributes stored in one chunk. In most cases you are advised to transform the array to have the
#' dimension values as attributes if you need those to perform calculations. The function will be passed on to the \link[plyr]{ddply}
#' function.
#'
#' @note parameter option "stream" for 'method' currently not supported.
#'
#' @param x scidbst array or scidb array
#' @param f r function of form \code{function(x) { ... }} expecting parameter x, which is a subset of the incoming data based on the aggregate statement
#' @param array string with the name of the output array
#' @param packages a vector of string of the packages required for the function f
#' @param parallel (optional) boolean whether or not the chunk is processed in parallel at an instance
#' @param cores (optional) if using parallel this specifies the number of cores to use at an instance
#' @param aggregates (optional) a vector of attribute names to group by
#' @param output a named list of output attributes and its scidb type (if using rexec method it will be 'double' regardless)
#' @param logfile (optional) the file path used to log during the processing if required
#' @param dim (optional) a named list with attribute name = output attribute name e.g. \code{list(dimy="y",dimx="x")}
#' @param dim.spec (optional) a named list with the dimension specification using the output dimension name as a identifier and a named numeric vector with min, max, overlap and chunk to specify the dimensionality
#' @param method The method to use, either "rexec" or "stream"; not utilized currently
#' @param ... see Details
#' @return scidbst array or scidb array depending on the input
#'
#' @details The script that is created during this function will handle the installation of required R-packages on
#' each of the instances. Then it combines the incoming attribute vectors to a data.frame object, which is passed
#' on to the '\code{\link[plyr]{ddply}}' function of the package 'plyr'. Depending on the stated aggregates parameter
#' the function 'f' is applied on that grouped sub data.frame object (parameter x of function f). Using the output
#' list the array will be projected on to the selected attributes. When specifying 'dim' and 'dim.spec' the stated
#' columns of the data.frame will be used as dimension in a perceeding redimension call.
#'
#' The \code{...} operator can contain the parameter \code{eval}, which is set to TRUE as default.
#' Also \code{...} can contain a developer parameter called \code{result} with the allowed values "afl" and
#' "rscript". \code{r.apply} then returns the submitted R-Script or the resulting AFL query. To
#' prevent the function from being executed use result in combination with "eval=FALSE".
#'
#' The following variable names are reserved if the spatial and temporal references exists and are transferred to ddply
#' function:
#' \describe{
#'  \item{\code{affine}}{a 2x3 matrix for spatial coordinate transformation}
#'  \item{\code{crs}}{a CRS object stating the used coordinate reference system}
#'  \item{\code{extent}}{a extent object stating the spatial extent}
#'  \item{\code{tmin} / \code{tmax}}{POSIXlt objects stating the minimum and maximum temporal boundary}
#'  \item{\code{t0}}{POSIXlt object marking the datum (time at value 0)}
#'  \item{\code{tunit}}{character describing the temporal measurement unit}
#'  \item{\code{tres}}{a number describing the temporal resolution}
#' }
#'
#'
#'
#' @examples
#' \dontrun{
#'  input.arr = scidbst("some_scidbst_array")
#'
#'  # make sure to have the dimensions as attributes if you plan to use them in calculations
#'  input.arr = transform(input.arr, dimx="double(x)",dimy="double(y)", dimt="double(t)")
#'  f <- function(x,...) {
#'      # parse the parameter passed as ... into the function and assign them to the functions
#'      # environment
#'      dot.input = list(...)
#'      i <- 1
#'      lapply(dot.input, function(x,y) {
#'          assign(x=y[i],value=x,envir=parent.env(environment()))
#'          i <<- i+1
#'          x
#'        },
#'        names(dot.input)
#'      )
#'      rm(i)
#'
#'      if (is.null(x)) {
#'        return(c(nt=0,var=0,median=0,mean=0))
#'      }
#'      t = x$dimt
#'      n = x$val
#'      return(c(nt=length(t),var=var(n),median=median(n),mean=mean(n)))
#'    }
#'  rexec.arr = r.apply(x=input.arr,
#'      f=f,
#'      array="output_array",
#'      parallel=FALSE,
#'      cores=1,
#'      aggregates=c("dimy","dimx"),
#'      output=list(dimy="double",dimx="double",nt="double",var="double",median="double",mean="double"),
#'      dim=list(dimy="y",dimx="x"),
#'      dim.spec=list(y=c(min=0,max=99,chunk=20,overlap=0),x=c(min=0,max=99,chunk=20,overlap=0)),
#'      logfile="/tmp/logfile.log")
#' }
#' @name r.apply,scidbst
#' @rdname r-apply-scidbst-method
#' @export
setMethod("r.apply",signature(x="scidbst",f="function"), .apply.scidbst.fun)

#' @name r.apply,scidb
#' @rdname r-apply-scidbst-method
#' @export
setMethod("r.apply",signature(x="scidb",f="function"), function(x,f,array,packages,parallel=FALSE,cores=1,aggregates=c(),output, logfile, ...) {
  logging = !missing(logfile)
  noPackages = missing(packages) || is.null(packages) || length(packages) == 0

  dots = list(...)
  if (any("eval" %in% names(dots))) {
    eval = dots$eval
    if (!is.logical(eval)) {
      warning("Eval is not of type logical. Setting eval to FALSE", immediate. = TRUE)
      eval = FALSE
    }
  } else {
    eval = TRUE
  }

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
  .query = gsub(";","\n",.query)
  # x@temps = append(x@temps,array)

  if (eval) {
    iquery(.query)
    # in .query we will have the store command which makes the array
    # available under the name from parameter 'array'
    out = scidb(array)
    return(out)
  } else {
    return(.query)
  }


})
