setGeneric("getXDim",function(x) standardGeneric("getXDim"))

#' Get spatial x dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a West-East dimension. If the object is not
#' spatially referenced it will return the second dimension name.
#'
#' @param x scidbst object
#' @return The name of the spatia West-East dimension or the first dimension name
#' @export
setMethod("getXDim",signature(x="scidbst"),function(x){
  if (length(x@spatial_dims) > 0 ) {
    return(x@spatial_dims$xdim)
  } else {
    return(dimensions(x)[2])
  }
})

setGeneric("getYDim",function(x) standardGeneric("getYDim"))

#' Get spatial y dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a North-South dimension. If the object is not
#' spatially referenced it will return the first dimension name.
#'
#' @param x scidbst object
#' @return The name of the spatia North-South dimension or the first dimension name
#' @export
setMethod("getYDim",signature(x="scidbst"),function(x){
  if (length(x@isSpatial) > 0) {
    return(x@spatial_dims$ydim)
  } else {
    return(dimensions(x)[1])
  }
})

setGeneric("getTDim",function(x) standardGeneric("getTDim"))

#' Get temporal dimension name
#'
#' Returns the name of the temporal dimension of a scidbst array. If the object is not temporal it will
#' return the first dimension name.
#'
#' @param x scidbst object
#' @return The temporal dimension name or the first dimension name
#' @export
setMethod("getTDim",signature(x="scidbst"),function(x){
  if (length(x@temporal_dim) > 0) {
    return(x@temporal_dim)
  } else {
    return(dimensions(x)[1])
  }
})


setGeneric("getLayer",function(x,layer,...) standardGeneric("getLayer"))

#' Get Layer
#'
#' If the data is loaded as a raster brick (e.g. with \code{readAll}), this function will replace the raster subset function, since scidb
#' uses a function with the same name to subset directly from the scidb database. The scidbst object, however, is required to be limited
#' to the two spatial dimensions, otherwise retrieving the data will fail. This function will return the selected layer of the raster part,
#' which is equal to an attribute in scidb.
#'
#' @param x scidbst object with raster data
#' @param layer the name or index of the attribute
#' @param ... additional parameters that are passed to \link{raster::subset}
#'
#' @return A RasterLayer object
#' @seealso \code{\link{raster::subset}}
#' @export
setMethod("getLayer",signature(x="scidbst",layer="ANY"), function(x,layer,...) {
  if(!hasValues(x)) {
    stop("No data loaded. Use 'subset' to filter for the correct attributes or use 'readAll' first")
  }
  if (is.character(layer) && !(layer %in% scidb_attributes(x) )) {
    stop("Cannot find layer name")
  }

  b = brick(c2)
  b@data = x@data
  return(subset(b,layer,...))

})
