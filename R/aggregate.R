#' @include scidbst-class.R
NULL

setGeneric("aggregate.t", function(x, ...) standardGeneric("aggregate.t"))

.aggregate.t.scidbst = function(x, ...) {
  browser()
  selection = x@spatial_dims

  if (x@isTemporal) {
    x@isTemporal = FALSE
    sobj = scidb(x@name)
    agg = aggregate(sobj, by=selection,...) #delegate operation to scidb package
    out = .scidbst_class(agg)

    #manage metadata
    out = .cpMetadata(x,out)
    out@data@names = scidb_attributes(out)
    out@tResolution = as.numeric(difftime(x@tExtent[["max"]],x@tExtent[["min"]],x@tUnit))+1
    out@temporal_dim = ""
    return(out)
  } else {
    stop("Cannot aggregate over time with no temporal reference on the object")
  }
}

#' aggregates over time
#'
#' This function aggregates a scidbst array for the dimension time. This means the resulting array will stripped from
#' the temporal dimension.
#'
#' @return scidbst array with a temporal resolution of the whole time series
#'
#' @export
setMethod("aggregate.t", signature(x="scidbst"), .aggregate.t.scidbst)

setGeneric("aggregate.sp", function(x, ...) standardGeneric("aggregate.sp"))

.aggregate.sp.scidbst = function(x, attributes, ...) {
  selection = as.list(x@temporal_dim)
  if (x@isSpatial) {

    x@isSpatial = FALSE
    old_nrow = nrow(x)
    old_ncol = ncol(x)
    sobj = scidb(x@name)
    agg = aggregate(sobj, by=selection,...) # delegate operation to scidb package
    out = .scidbst_class(agg)
    out = .cpMetadata(x,out)
    out@data@names = scidb_attributes(out)
    out@spatial_dims = list()

    out@affine = out@affine %*% matrix(c(1,0,0,0,old_ncol,0,0,0,old_nrow),ncol=3,nrow=3)

    return(out)
  } else {
    stop("Cannot aggregate over space with no spatial reference on the object")
  }
}

#' aggregates over space
#'
#' This function aggregates over space leaving a scidbst array that has a spatial resolution of the
#' prior spatial extent of the array.
#'
#' @return scidbst object with the spatial resolution extended to the whole image extent
#'
#' @export
setMethod("aggregate.sp", signature(x="scidbst"), .aggregate.sp.scidbst)


.aggregate.scidbst = function(x, by, ...) {
  aggregate_by_time = FALSE
  aggregate_by_space = FALSE

  if (!x@isTemporal && !x@isSpatial) {
    stop("No spatial or temporal reference. Coerce object to scidb object.")
  }

  if (x@isTemporal) {
    aggregate_by_time = all(by %in% x@spatial_dims)
  }
  if (x@isSpatial) {
    aggregate_by_space = all(by %in% x@temporal_dim)
  }
  browser()
  if (aggregate_by_time && !aggregate_by_space) {
    out = aggregate.t(x, by, ...)
    return(out)
  }
  if (aggregate_by_space && !aggregate_by_time) {
    out = aggregate.sp(x, by, ...)
    return(out)
  }

  if (!aggregate_by_time && !aggregate_by_space) {
    #check if one of the reference axis is involved in the by statement, aggregate and copy metadata
    dims = list()
    if (x@isSpatial) {
      dims = append(dims,x@spatial_dims)
    }
    if (x@isTemporal) {
      dims = append(dims, x@temporal_dim)
    }

    has_any_ref_axis = any(dims %in% by)
    if (!has_any_ref_axis) {
      #aggregate as scidb object
      arr = aggregate(scidb(x@name),by,...)
      #and then transform to scidbst
      arr = .scidbst_class(arr)
      arr = .cpMetadata(x,arr)
      return(arr)
    } else if (any(x@temporal_dim %in% by)) {
      #for now this should not happen, since length of temporal_dim is 1
      return(aggregate.t(x, by, ...))
    } else {
      #then it is one spatial dimension
      #which means we loose the spatial reference
      stop("Aggregating over just one spatial dimension currently not allowed.")
    }
  } else {
    #aggregation over space and time
    # aggregate as scidb
    arr = aggregate(scidb(x@name),by,...)
    # copy metadata
    arr = .cpMetadata(x,arr)
    out@data@names = scidb_attributes(out)
    # set tResolution to complete temporal extent

    out@tResolution = as.numeric(difftime(x@tExtent[["max"]],x@tExtent[["min"]],x@tUnit))+1
    out@temporal_dim = ""
    # set space Resolution in affine transformation to total spatial extent
    out@spatial_dims = list()

    out@affine = out@affine %*% matrix(c(1,0,0,0,old_ncol,0,0,0,old_nrow),ncol=3,nrow=3)
    return(out)
  }
}

#' Aggregates a SciDBST object over the given dimensions and/or attributes
#'
#' @usage
#' aggregate(x, by, FUN, window, variable_window)
#'
#' @param x A \code{scidbst} object.
#' @param by optional single character string or a list of array dimension and/or attribute names to group by;
#' or a \code{scidb} object to group by. Not required for \code{windowed} and grand aggregates.
#' @param FUN a character string representing a SciDB aggregation expression or a reduction function.
#' @param window optional, if specified, perform a moving window aggregate along the specified coordinate windows.
#' @param variable_window optional, if specified, perform a moving window aggregate over successive data values along the
#' coordinate dimension axis specified by \code{by}.
#'
#' @export
#' @seealso \link{aggregate,scidb-method}

setMethod("aggregate",signature(x="scidbst"), .aggregate.scidbst)
