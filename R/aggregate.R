#' @include scidbst-class.R
NULL

setGeneric("aggregate.t", function(x, ...) standardGeneric("aggregate.t"))

.aggregate.t.scidbst = function(x, by, ...) {
  selection = list(ydim(x),xdim(x))

  if (x@isTemporal) {
    x@isTemporal = FALSE
    sobj = as(x,"scidb")
    agg = aggregate(sobj, by=selection,...) #delegate operation to scidb package
    x@proxy = agg
    # x@data@names = scidb_attributes(x)
    x@trs@tResolution = as.numeric(difftime(tmax(x),tmin(x),units=tunit(x)))+1
    return(x)
  } else {
    stop("Cannot aggregate over time with no temporal reference on the object")
  }
}

#' @rdname aggregate-scidbst-methods
#' @export
setMethod("aggregate.t", signature(x="scidbst"), .aggregate.t.scidbst)

setGeneric("aggregate.sp", function(x, ...) standardGeneric("aggregate.sp"))

.aggregate.sp.scidbst = function(x, by, ...) {
  dots = list(...)

  selection = as.list(tdim(x))
  if (!missing(by)) {
    dots["by"] <- NULL
  }

  if (x@isSpatial) {

    x@isSpatial = FALSE
    old_nrow = nrow(x)
    old_ncol = ncol(x)
    sobj = as(x,"scidb")
    agg = aggregate(sobj, by=selection, dots) # delegate operation to scidb package
    x@proxy = agg

    x@affine = affine(x) %*% matrix(c(1,0,0,0,old_ncol,0,0,0,old_nrow),ncol=3,nrow=3)

    return(x)
  } else {
    stop("Cannot aggregate over space with no spatial reference on the object")
  }
}

#' @rdname aggregate-scidbst-methods
#' @export
setMethod("aggregate.sp", signature(x="scidbst"), .aggregate.sp.scidbst)


.aggregate.scidbst = function(x, by, ...) {

  if (!missing(by)) {
    if (!is.list(by)) {
      if (is.character(by) && length(by) > 0) {
        by = as.list(by)
      }
    } else {
      stop("Cannot use the stated parameter 'by'. Type incompatible. Please use 'list' instead.")
    }
  }

  dots = list(...)
  usesWindow = any(c("window","variable_window") %in% names(dots))

  if (missing(by) && !usesWindow) {
    by = "" # aggregate over all dimensions -> result will contain one value
  }

  if (!missing(by) && usesWindow) {
    by = dimensions(x) #uses all dimensions -> no aggregation, just applying the window operation
    warning("Ignoring parameter 'by', because 'window' operator is used. To prevent conflicts 'by' is set to all dimensions, meaning the array schema is not changed.")
  }

  aggregate_by_time = FALSE
  aggregate_by_space = FALSE

  if (!x@isTemporal && !x@isSpatial) {
    stop("No spatial or temporal reference. Coerce object to scidb object.")
  }

  if (x@isTemporal) {
    aggregate_by_time = all(x@srs@dimnames %in% by)
  }
  if (x@isSpatial) {
    aggregate_by_space = all(tdim(x) %in% by)
  }

  if (aggregate_by_time && !aggregate_by_space) { #all spatial dimensions are present
    x = aggregate.t(x, by, ...)
    return(x)
  }

  if (aggregate_by_space && !aggregate_by_time) { # all temporal dimensions are present
    x = aggregate.sp(x, by, ...)
    return(x)
  }

  if (aggregate_by_time && aggregate_by_space) { #aggregation over space and time
    # aggregate as scidb
    old_ncol = ncol(x)
    old_nrow = nrow(x)

    .scidb = as(x,"scidb")
    if (length(by) != length(dimensions(x))) {
      x@proxy = aggregate(.scidb,by,...)
    } else {
      x@proxy = aggregate(.scidb, by="", ...)
    }


    if (is.null(x@proxy)) {
      # this should basically never happen, because dimension 'i' will be used if every other dimension
      # is gone.
      stop("No dimensions left.")
    }

    # set tResolution to complete temporal extent
    x@trs@tResolution = as.numeric(difftime(tmax(x),tmin(x),units=tunit(x)))+1

    # set space Resolution in affine transformation to total spatial extent
    x@affine = affine(x) %*% matrix(c(1,0,0,0,old_ncol,0,0,0,old_nrow),ncol=3,nrow=3)
    x@isSpatial = FALSE
    x@isTemporal = FALSE
    return(x)
  }

  # at this point either no or just one spatial dimension is present

  if (!aggregate_by_time && !aggregate_by_space && !any(x@srs@dimnames %in% by)) { # no referenced dimension involved
    #aggregate as scidb object
    x@proxy = aggregate(as(x,"scidb"),by,...)

    return(x)
  } else {
    stop("Aggregation over one spatial dimension currently not allowed")
  }

}

#' Aggregates a scidbst object over given dimensions and/or attributes
#'
#' Due to the fact that scidbst arrays can have dimensions that are referenced in space and/or time, this function
#' uses 'scidb's aggregate function for performing the aggregation itself and it manages the metadata information
#' about the references / extents and alike.
#'
#' The scidbst package also provides functions to directly aggregate over space or time without the need to
#' specify the dimensions that need to be aggregated by.
#'
#' \strong{Aggregate over space}
#'
#' This function aggregates over space leaving a scidbst array without spatial dimensions. The spatial information will remain
#' on the R object. The spatial resolution will be increased to the whole spatial dimensions (one cell captures the whole image). And
#' the spatial extent will remain the same.
#'
#' \strong{Aggregate over time}
#'
#' This function aggregates a scidbst array for the temporal dimension. This means the resulting array will stripped from
#' the temporal dimension and the values on the temporal dimension will be aggregated.
#'
#'
#' @name aggregate
#' @rdname aggregate-scidbst-methods
#' @aliases aggreate.sp,scidbst aggregate.t,scidbst aggregate,scidbst
#' @usage
#' aggregate(x, by, FUN, window, variable_window)
#'
#' @param x A \code{scidbst} object.
#' @param by optional single character string or a list of array dimension and/or attribute names to group by;
#' or a \code{scidb} object to group by. Not required for \code{window} and grand aggregates.
#' @param FUN a character string representing a SciDB aggregation expression or a reduction function.
#' @param window optional, if specified, perform a moving window aggregate along the specified coordinate windows.
#' @param variable_window optional, if specified, perform a moving window aggregate over successive data values along the
#' coordinate dimension axis specified by \code{by}.
#'
#' @note The additional parameter like window and variable_window are passed to the original aggregate function from the
#' scidb package. However, the variable_window parameter was not tested.
#' @examples
#' \dontrun{
#' # Using the pure aggregate method
#' scidbconnect(...)
#' scidbst.obj = scidbst("array1") #array with spatial and temporal dimension (x,y,t)
#' agg.1 = aggregate(x=scidbst.obj,by=list("y","x"),FUN="avg(band1)") # aggreagtes over time, result something similar to a raster
#' agg.2 = aggregate(x=scidbst.obj,by=list("t"),FUN="avg(band1)") # aggregate over space, result something like a time series
#' agg.3 = aggregate(x=scidbst.obj,by=list("y","x","t"),FUN=count) # count cells that are aggregated = total number of cells over all dimensions
#' }
#'
#' \dontrun{
#' # Using aggregate over space
#' scidbst.obj = scidbst("array2")
#' aggt = aggregate.sp(scidbst.obj,FUN="avg(attribute1)") # returns something similar to a timeseries with aggregated values over the spatial dimension
#' }
#'
#' \dontrun{
#' # Using aggregate over time
#' scidbst.obj = scidbst("array3")
#' aggt = aggregate.t(scidbst.obj,FUN="avg(attribute1)") # returns something similar to a raster with aggregated values over the temporal dimension
#' }
#' @export
#'
setMethod("aggregate",signature(x="scidbst"), .aggregate.scidbst)
