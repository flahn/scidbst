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
  commands = append(commands,f.character[2:f.end])
  return(commands)
}

# commands: vector of string, attributes: vector of string with attribute names
.appendChunkDataFrameDefinition = function(commands,attributes) {
  #use attribute names to create a statement for creating a dataframe
  # ndvi.df = data.frame(ndvi=ndvi,dimy=dimy,dimx=dimx,dimt=dimt)
  df.attributes = paste(attributes,"=",attributes,collapse=",",sep="")
  commands = append(commands,paste("df = data.frame(",df.attributes,")",sep=""))
  return(commands)
}

.appendOutputTypeConversion = function(commands,output) {
  if (is.list(output)) {
    attr.names = names(output)
    attr.types = unlist(output)
    names(attr.types) = c()
    commands = append(commands,paste("list(",paste(attr.names,"=as(func.result$",attr.names,",\"",attr.types,"\")",sep="",collapse=","),")",sep=""))
    return(commands)
    #list(as.double(ndvi.change$dimy), dimx =    as.double(ndvi.change$dimx),    as.double(ndvi.change$nt),as.double(ndvi.change$breakpoint),    as.double(ndvi.change$magnitude) )
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

.apply.scidbst.fun = function(x,f,array,packages,parallel=FALSE,cores=1,aggregates,output,logfile,...) {
  commands=c()
  attr = scidb_attributes(x)

  if (!all(aggregates %in% attr)) {
    stop("Cannot find the attributes to aggregate over in the functions data input.")
  }

  if (!missing(packages)) {
    commands = .appendUserDefinedRequiredPackages(commands,packages)
  }

  if (cores < 1) cores = 1

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

  # load required packages


  # get the code of the function as text
  if (!missing(f) && is.function(f)) {
    commands = .appendUserDefinedFunctionCode(commands,f)
  } else {
    stop("No function was defined to operate on a chunk.")
  }


  # create the data.frame
  commands = .appendChunkDataFrameDefinition(commands,attr)
  # TODO option to calculate coordinates or timestamps to create or work with spatial or spatio-temporal objects

  if (!missing(aggregates)) {
    if (!missing(packages)) {
      if (!all("plyr" %in% packages)) {
        commands = append(commands,.requireInstallPackage("plyr",logfile = logfile))
      }
      if (!all("foreach" %in% packages)) {
        commands = append(commands,.requireInstallPackage("foreach",logfile = logfile))
      }
    } else {
      commands = append(commands,.requireInstallPackage("plyr",logfile = logfile))
      commands = append(commands,.requireInstallPackage("foreach",logfile = logfile))
    }

    #ndvi.change = ddply(ndvi.df, c(\"dimy\",\"dimx\"), f, .parallel=TRUE)
    aggregates.string = paste("c(",paste("\"",aggregates,"\"",collapse=",",sep=""),")",sep="")
    #TODO you can pass additional arguments to f after .fun
    # dot.params = list(...)
    commands = append(commands, .log("processing a chunk",logfile))
    # commands = append(commands, .log("colnames(df)",logfile,quote=FALSE))

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
    })",ddply_cmd,.log("error in a chunk",logfile))

    # commands = append(commands,ddply_cmd)
    commands = append(commands,tc_exec)
  } else {
    #TODO handle missing aggregates statement
  }

  commands = .appendOutputTypeConversion(commands,output) #R_EXEC probably allows in scidb just double values
  #https://github.com/Paradigm4/r_exec/blob/master/LogicalRExec.cpp
  #line 54


  output.attr.count = length(output)

  query.R = sprintf("store(unpack(r_exec(%s,'output_attrs=%i','expr=%s'),i),%s)",
                    x@proxy@name,
                    output.attr.count,
                    paste(commands,collapse="\n",sep=""),
                    array)

  iquery(query.R)
  out = scidb(array)

  #rename
  # expr_attr = append(list("_data"=out),as.list(paste("expr_value_",0:(output.attr.count-1),sep="")))
  # new_attr = c("_data",names(output))
  #
  # names(expr_attr) = new_attr
  # renamed = do.call(transform, expr_attr)
  expr_attr = paste("expr_value_",0:(output.attr.count-1),sep="")
  renamed = scidb::attribute_rename(out,expr_attr,names(output))

  return(renamed)

  #output values are set to "expr_value_X" with X the poisition in the array
  # https://github.com/Paradigm4/r_exec/blob/master/LogicalRExec.cpp
  # line 76
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
