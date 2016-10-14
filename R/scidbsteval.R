#' @include scidbst-class.R
NULL

if (!isGeneric("scidbsteval")) {
  setGeneric("scidbsteval",function(expr, name, ...) standardGeneric("scidbsteval"))
}

.scidbeval.scidbst = function(expr, name, eval = TRUE, gc = TRUE, temp = FALSE, drop=TRUE) {
  if (missing(name)) {
    stop("No target array name specified. Please use parameter 'name' to state the target arrays name.")
  }
  if (inherits(expr,"scidbst")) {
    scidb.obj = as(expr,"scidb")
  } else if (is.character(expr)){
    # if this is an expression (string), then we try to create a scidbst object from that
    expr = scidbst(expr)
    scidb.obj = as(expr,"scidb")
  } else {
    stop(paste("Cannot invoke scidb with parameter 'expr' of type ",class(expr),sep=""))
  }

  if (!drop) {
    temp_name = paste(name,"_temp",sep="")
    #store evaluated array temporary
    scidb.obj = scidbeval(scidb.obj,eval=TRUE,name=temp_name,gc=TRUE,temp=TRUE) #probably takes time
    expr@proxy = scidb.obj
    bounds=iquery(sprintf("dimensions(%s)",temp_name),return=T) # required to find the true

    # there are problems with reshape and unbounded coordinates -> scidbeval runs infinitely, so replace
    # infinites with highs and lows
    # if references are kept (not dropped)
    starts = scidb_coordinate_start(expr)
    starts[starts=="*"] = bounds$low[starts=="*"]
    starts = as.double(starts)

    lengths = scidb_coordinate_bounds(expr)$length
    lengths = as.double(lengths)
    lengths[is.infinite(lengths)] = bounds[is.infinite(lengths),"high"] -bounds[is.infinite(lengths),"low"]+1

    ends = scidb_coordinate_end(expr)
    ends[ends=="*"] = bounds$high[ends=="*"]
    ends = as.double(ends)

    dimnames = dimensions(expr)
    chunks = as.double(scidb_coordinate_chunksize(expr))

    #do subarray before reshape
    scidb.obj = subarray(scidb.obj,c(starts,ends))

    # 1. check if there is some sort of spatial reference left (isSpatial is not enough)
    # hint: normally the spatial dimensions are not unbounded
    if (!expr@isSpatial) { #not spatial in scidb, but in R
      if (length(expr@srs@dimnames) >= 2) {
        # yes: merge old spatial dimensions back to the array,
        # set values to 0 and
        # set new spatial reference (adapted resolution for example)
        starts = c(starts, 0,0)
        ends = c(ends, 0,0)
        lengths = c(lengths, 1,1)
        dimnames = c(dimnames,ydim(expr),xdim(expr))
        chunks = c(chunks,1,1)
        expr@isSpatial = TRUE
      } else {
        stop("Error: Cannot set spatial reference due to missing spatial dimension.")
      }
    }

    # 2. check if there is some sort of temporal reference left (isTemporal just refers to the scidb array)
    if (!expr@isTemporal) {
      if (length(tdim(expr)) > 0) {
        # yes: merge the old temporal dimension back to array,
        # set values to 0 and
        starts = c(starts, 0)
        ends = c(ends,0)
        lengths = c(lengths, 1)
        dimnames = c(dimnames,tdim(expr))
        chunks = c(chunks,1)
        expr@isTemporal = TRUE
        # set adapt temporal reference
      }
    }

    D = paste(scidb:::build_attr_schema(scidb.obj),scidb:::build_dim_schema(scidb.obj, newstart=starts, newnames=dimnames, newlen=lengths, newchunk=chunks),sep="")
    scidb.obj = reshape_scidb(x=scidb.obj,schema=D)

    #second store to adapt none dropping changes
    scidb.obj = scidbeval(expr=scidb.obj,eval=eval,name=name, gc=gc, temp=temp)
    expr@proxy = scidb.obj


    #clean up
    scidbrm(temp_name,force=TRUE)
  } else {
    # store / evaluate array
    scidb.obj = scidbeval(scidb.obj,eval,name, gc, temp)
    expr@proxy = scidb.obj
    # no need to copy elements, just use the expr object that was passed to this function and change name later
  }


  # set spatial and temporal references if applicable
  if (expr@isSpatial) {
    setSRS(scidb.obj,srs(expr),affine(expr))
    #eo_setsrs:  {name,xdim,ydim,authname,authsrid,affine_str}
    # cmd = paste("eo_setsrs(",name,",'",xdim(expr),"','",ydim(expr),"','",expr@srs@authority,"',",expr@srs@srid,",'","x0=",affine(expr)[1,1]," y0=",affine(expr)[2,1]," a11=",affine(expr)[1,2]," a22=",affine(expr)[2,3]," a12=",affine(expr)[1,3]," a21=",affine(expr)[2,2],"'",")",sep="")
    # iquery(cmd)
  }

  if (expr@isTemporal) {
    # cmd = paste("eo_settrs(",name,",'",tdim(expr),"','",as.character(t0(expr)),"','",getRefPeriod(expr),"'",")",sep="")
    # iquery(cmd)
    setTRS(scidb.obj,trs(expr))
  }

  # rename the array, since the name was changed due to store
  expr@proxy@name = name
  expr@title = name

  if (temp) {
    if(is.null(expr@temps)) {
      expr@temps=c(name)
    } else {
      expr@temps=c(name,expr@temps)
    }
  } else {
    if(!is.null(expr@temps)) {
      # TODO remove temporary arrays
      list = scidbls()
      existingArrays = expr@temps[expr@temps %in% list]
      scidbrm(existingArrays,force=TRUE)
      expr@temps = NULL
    }
  }

  return(expr)
}

#' Executes cascaded operations and stores data in scidb under a given name
#'
#' The function works in the similar way as \link{scidbeval}. As an expression this function requires a 'scidbst' object,
#' which can be modified by various scidb operations. By calling this function the actual commands are executed in the
#' SciDB cluster. The result will be stored under the given 'name' parameter. In addition to the original function, the
#' evaluation of a scidbst object will also set the current spatial and/or temporal reference.
#'
#' @rdname scidbsteval
#' @aliases scidbsteval
#' @param expr The scidbst object
#' @param name The name of the target array in which the data is stored
#' @param eval A flag whether or not the commands shall be executed in scidb
#' @param gc A flag whether or not the result should be tied to the R garbage collector
#' @param temp A flag wheter or not the resulting scidb array is temporary
#' @param drop Whether or not to drop spatial or temporal references, when dimension is removed from array
#'
#' @return The modified scidbst object
#'
#' @note Using the similar function \code{scidbeval} function will also perform the storing operation, but it will not transfer
#' the dimension references for space and/or time. Also, unbounded dimensions that are not dropped will be created
#' as bounded dimensions by its minimum/maximum dimension value.
#' @seealso \code{\link{scidbeval}}
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' scidbst.obj = scidbst(array_name) # array with spatial and temporal dimensions
#'
#' #array renaming
#' scidbsteval(expr=scidbst.obj,name=new_name)
#'
#' # slicing and storing
#' sliced = slice(scidbst.obj,"t","2016-05-03")
#' scidbsteval(sliced,new_name)
#'
#' # aggregation over space and storing
#' agg.t = aggregate(x=scidbst.obj,by=list("t"),FUN="avg(attribute1)")
#' scidbsteval(agg.t,name=new_name)
#' }
#' @export
setMethod("scidbsteval", signature(expr="scidbst", name="character"), .scidbeval.scidbst )
