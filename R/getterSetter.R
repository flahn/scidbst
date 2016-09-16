
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
#' @seealso \code{\link[raster]{subset}}
#' @export
setMethod("getLayer",signature(x="scidbst",layer="ANY"), function(x,layer,...) {
  if(!hasValues(x)) {
    stop("No data loaded. Use 'subset' to filter for the correct attributes or use 'readAll' first")
  }
  if (is.character(layer) && !(layer %in% scidb_attributes(x) )) {
    stop("Cannot find layer name")
  }

  b = brick(x)
  b@data = x@data
  return(subset(b,layer,...))

})

setGeneric("affine", function(x) standardGeneric("affine"))

#' @export
setMethod("affine",signature(x="scidbst"),function(x) {
  return(x@affine)
})
