if(!isGeneric("apply.fun")) {
  setGeneric("apply.fun",function(x,f,...){
    standardGeneric("apply.fun")
  })
}

.appendUserDefinedRequiredPackages = function(commands,packages) {
  package.list = as.list(packages)
  package.require.statement = paste("require(",package.list,")",sep="")
  commands = append(commands,package.require.statement)
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

.apply.scidbst.fun = function(x,array,packages,parallel=FALSE,cores=1,aggregates,output,f,...) {
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
      if (!all("doParallel" %in% packages)) {
        commands = append(commands,"require(doParallel)")
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
  # TODO option to calculate coordinates or timestamps

  if (!missing(aggregates)) {
    if (!all("plyr" %in% packages)) {
      commands = append(commands,"require(plyr)")
    }
    #ndvi.change = ddply(ndvi.df, c(\"dimy\",\"dimx\"), f, .parallel=TRUE)
    aggregates.string = paste("c(",paste("\"",aggregates,"\"",collapse=","),")",sep="")
    #TODO you can pass additional arguments to f after .fun
    # dot.params = list(...)


    commands = append(commands,paste("func.result = ddply(.data=df, .variables=",aggregates.string,", .fun=f ,.parallel=",parallel,")",sep=""))
  }

  commands = .appendOutputTypeConversion(commands,output) #R_EXEC probably allows in scidb just double values
  #https://github.com/Paradigm4/r_exec/blob/master/LogicalRExec.cpp
  #line 54


  output.attr.count = length(output)

  query.R = sprintf("store(unpack(r_exec(%s,'output_attrs=%i','expr=%s'),i),%s)",x@title,output.attr.count,paste(commands,collapse="\n",sep=""),array)
  # query.R = paste("store(unpack(r_exec(", "L7_SW_ETHOPIA_TCHUNK_SMALL",
  #                 ",'output_attrs=5','expr=",
  #                         "'),i),
  #                         L7_SW_CHANGES_TEST_SMALL)", sep="")
  return(query.R)
  # return(iquery(query.R))

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
setMethod("apply.fun",signature(x="scidbst",f="function"), .apply.scidbst.fun)
