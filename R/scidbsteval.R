#' @include scidbst-class.R
NULL

if (!isGeneric("scidbsteval")) {
  setGeneric("scidbsteval",function(expr, name, ...) standardGeneric("scidbsteval"))
}

.scidbeval.scidbst = function(expr, name, eval = TRUE, gc = TRUE, temp = FALSE) {
  if (missing(name)) {
    stop("No target array name specified. Please use parameter 'name' to state the target arrays name.")
  }
  # first store / evaluate array
  if (inherits(expr,"scidb")) {
    scidb.obj = .toScidb(expr)
    scidbeval(scidb.obj,eval,name, gc, temp)
  }

  # then set spatial and temporal references if applicable
  if (expr@isSpatial) {
    #eo_setsrs:  {name,xdim,ydim,authname,authsrid,affine_str}
    cmd = paste("eo_setsrs(",name,",'",getXDim(expr),"','",getYDim(expr),"','",expr@sref$auth_name,"',",expr@sref$auth_srid,",'","x0=",expr@affine[1,1]," y0=",expr@affine[2,1]," a11=",expr@affine[1,2]," a22=",expr@affine[2,3]," a12=",expr@affine[1,3]," a21=",expr@affine[2,2],"'",")",sep="")
    iquery(cmd)
  }

  if (expr@isTemporal) {
    cmd = paste("eo_settrs(",name,",'",getTDim(expr),"','",as.character(expr@startTime),"','",.getRefPeriod(expr),"'",")",sep="")
    iquery(cmd)
  }

  expr@name = name
  return(expr)
}

#' Executes cascaded operations and stores data in scidb under a given name
#'
#' The function works in the similar way as \link{scidbeval}. As an expression this function requires a 'scidbst' object,
#' which can be modified by various scidb operations. By calling this function the actual commands are executed in the
#' SciDB cluster. The result will be stored under the given 'name' parameter. In addition to the original function, the
#' evaluation of a scidbst object will also set the current spatial and/or temporal reference.
#' @rdname scidbsteval
#' @param expr The scidbst object
#' @param name The name of the target array in which the data is stored
#' @param eval A flag whether or not the commands shall be executed in scidb
#' @param gc A flag whether or not the result should be tied to the R garbage collector
#' @param temp A flag wheter or not the resulting scidb array is temporary
#'
#' @return The modified scidbst object
#'
#' @export
setMethod("scidbsteval", signature(expr="scidbst", name="character"), .scidbeval.scidbst )
