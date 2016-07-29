#' @include scidbst-class.R
NULL

setGeneric("aggregate.t", function(x, ...) standardGeneric("aggregate.t"))

.aggregate.t.scidbst = function(x, by, ...) {
  selection = x@spatial_dims

  if (x@isTemporal) {
    x@isTemporal = FALSE
    sobj = .toScidb(x)
    agg = aggregate(sobj, by=selection,...) #delegate operation to scidb package
    out = .scidbst_class(agg)

    #manage metadata
    out = .cpMetadata(x,out)
    out@data@names = scidb_attributes(out)
    out@tResolution = as.numeric(difftime(x@tExtent[["max"]],x@tExtent[["min"]],x@tUnit))+1
    # out@temporal_dim = ""
    return(out)
  } else {
    stop("Cannot aggregate over time with no temporal reference on the object")
  }
}

#' Aggregate over time
#'
#' This function aggregates a scidbst array for the temporal dimension. This means the resulting array will stripped from
#' the temporal dimension and the values on the temporal dimension will be aggregated.
#'
#' @note The aggregated scidbst object will loose its temporal reference after aggregation, due to the fact that the spatial
#' dimensions are removed
#' @aliases aggregate.t
#' @usage
#' aggregate(x, by, FUN, window, variable_window)
#'
#' @param x A \code{scidbst} object.
#' @param by optional single character string or a list of array dimension and/or attribute names to group by additionally to
#' the temporal dimension
#' @param FUN a character string representing a SciDB aggregation expression or a reduction function.
#' @param window optional, if specified, perform a moving window aggregate along the specified coordinate windows.
#' @param variable_window optional, if specified, perform a moving window aggregate over successive data values along the
#' coordinate dimension axis specified by \code{by}.
#'
#' @return scidbst array with aggregated values on the temporal dimension (no temporal dimension)
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' scidbst.obj = scidbst(array_name)
#' aggt = aggregate.t(scidbst.obj,FUN="avg(attribute1)") # returns something similar to a raster with aggregated values over the temporal dimension
#' }
#' @export
setMethod("aggregate.t", signature(x="scidbst"), .aggregate.t.scidbst)

setGeneric("aggregate.sp", function(x, ...) standardGeneric("aggregate.sp"))

.aggregate.sp.scidbst = function(x, by, ...) {
  dots = list(...)

  selection = as.list(x@temporal_dim)
  if (!missing(by)) {
    dots["by"] <- NULL
  }

  if (x@isSpatial) {

    x@isSpatial = FALSE
    old_nrow = nrow(x)
    old_ncol = ncol(x)
    sobj = .toScidb(x)
    agg = aggregate(sobj, by=selection, dots) # delegate operation to scidb package
    out = .scidbst_class(agg)
    out = .cpMetadata(x,out)
    out@data@names = scidb_attributes(out)
    # out@spatial_dims = list()

    out@affine = out@affine %*% matrix(c(1,0,0,0,old_ncol,0,0,0,old_nrow),ncol=3,nrow=3)

    return(out)
  } else {
    stop("Cannot aggregate over space with no spatial reference on the object")
  }
}

#' Aggregate over space
#'
#' This function aggregates over space leaving a scidbst array without spatial dimensions. The spatial information will remain
#' on the R object. The spatial resolution will be increased to the whole spatial dimensions (one cell captures the whole image). And
#' the spatial extent will remain the same.
#'
#' @note the aggregate scidbst object will loose its spatial reference after evaluation, due to the fact that the spatial
#' dimensions are removed
#' @aliases aggregate.sp
#' @usage
#' aggregate(x, by, FUN, window, variable_window)
#'
#' @param x A \code{scidbst} object.
#' @param by optional single character string or a list of array dimension and/or attribute names to group by additionally to
#' the spatial dimensions
#' @param FUN a character string representing a SciDB aggregation expression or a reduction function.
#' @param window optional, if specified, perform a moving window aggregate along the specified coordinate windows.
#' @param variable_window optional, if specified, perform a moving window aggregate over successive data values along the
#' coordinate dimension axis specified by \code{by}.
#'
#' @return scidbst object with the spatial resolution extended to the whole image extent (spatial dimension is removed), often times
#' something similar to a time series will remain
#'
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' scidbst.obj = scidbst(array_name)
#' aggt = aggregate.sp(scidbst.obj,FUN="avg(attribute1)") # returns something similar to a timeseries with aggregated values over the spatial dimension
#' }
#' @export
setMethod("aggregate.sp", signature(x="scidbst"), .aggregate.sp.scidbst)


.aggregate.scidbst = function(x, by, ...) {
  aggregate_by_time = FALSE
  aggregate_by_space = FALSE

  if (!x@isTemporal && !x@isSpatial) {
    stop("No spatial or temporal reference. Coerce object to scidb object.")
  }

  if (x@isTemporal) {
    aggregate_by_time = all(x@spatial_dims %in% by)
  }
  if (x@isSpatial) {
    aggregate_by_space = all(x@temporal_dim %in% by)
  }

  if (aggregate_by_time && !aggregate_by_space) { #all spatial dimensions are present
    out = aggregate.t(x, by, ...)
    return(out)
  }

  if (aggregate_by_space && !aggregate_by_time) { # all temporal dimensions are present
    out = aggregate.sp(x, by, ...)
    return(out)
  }

  if (aggregate_by_time && aggregate_by_space) { #aggregation over space and time
    # aggregate as scidb
    old_ncol = ncol(x)
    old_nrow = nrow(x)

    if (length(by) != length(dimensions(x))) {
      out = aggregate(.toScidb(x),by,...)
    } else {
      out = aggregate(.toScidb(x), by="", ...)
    }


    if (is.null(out)) {
      # this should basically never happen, because dimension 'i' will be used if every other dimension
      # is gone.
      stop("No dimensions left.")
    }
    # copy metadata
    out = .scidbst_class(out)
    out = .cpMetadata(x,out)
    out@data@names = scidb_attributes(out)

    # set tResolution to complete temporal extent

    out@tResolution = as.numeric(difftime(x@tExtent[["max"]],x@tExtent[["min"]],x@tUnit))+1
    out@temporal_dim = ""
    # set space Resolution in affine transformation to total spatial extent
    out@spatial_dims = list()

    out@affine = out@affine %*% matrix(c(1,0,0,0,old_ncol,0,0,0,old_nrow),ncol=3,nrow=3)
    out@isSpatial = FALSE
    out@isTemporal = FALSE
    return(out)
  }

  # at this point either no or just one spatial dimension is present

  if (!aggregate_by_time && !aggregate_by_space && !any(x@spatial_dims %in% by)) { # no referenced dimension involved
    #aggregate as scidb object
    arr = aggregate(.toScidb(x),by,...)
    #and then transform to scidbst
    arr = .scidbst_class(arr)
    arr = .cpMetadata(x,arr)
    return(arr)
  } else {
    stop("Aggregation over one spatial dimension currently not allowed")
  }

}

#' Aggregates a 'scidbst' object over the given dimensions and/or attributes
#'
#' Due to the fact that scidbst arrays can have dimensions that are referenced in space and/or time, this function
#' uses 'scidb's aggregate function for performing the aggregation itself and it manages the metadata information
#' about the references / extents and alike.
#'
#' @details The scidbst package also provides functions to directly aggregate over space or time without the need to
#' specify the dimensions that need to be aggregated by. For more information see: \code{\link{aggregate.t}} and
#' \code{\link{aggregate.sp}}
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
#' @seealso \code{\link{aggregate,scidb-method}}, \code{\link{aggregate.t}}, \code{\link{aggregate.sp}}
#' @examples
#' scidbconnect(...)
#' scidbst.obj = scidbst(array_name) #array with spatial and temporal dimension (x,y,t)
#' agg.1 = aggregate(x=scidbst.obj,by=list("y","x"),FUN="avg(band1)") # aggreagtes over time, result something similar to a raster
#' agg.2 = aggregate(x=scidbst.obj,by=list("t"),FUN="avg(band1)") # aggregate over space, result something like a time series
#' agg.3 = aggregate(x=scidbst.obj,by=list("y","x","t"),FUN=count) # count cells that are aggregated = total number of cells over all dimensions
#' @export
#'
setMethod("aggregate",signature(x="scidbst"), .aggregate.scidbst)
