#' @include scidbst-class.R
#' @include sampleRegular.R
NULL

# .spplot.raster.scidbst.direct = function (obj, maxpixels=50000, as.table=TRUE, zlim,...) {
#   if (length(dimnames(obj)) > 2 ) {
#     stop("Too many dimensions detected. Try 'slice' to make a 2D subset of the image.")
#   }
#   if (!hasValues(obj)) {
#     obj = readAll(obj)
#   }
#   attr_names = scidb_attributes(obj)
#   #following: code from raster::spplot
#   obj <- sampleRegular(obj, maxpixels, asRaster=TRUE, useGDAL=TRUE)
#   if (!missing(zlim)) {
#     if (length(zlim) != 2) {
#       warning('zlim should be a vector of two elements')
#     }
#     if (length(zlim) >= 2) {
#       obj[obj < zlim[1] | obj > zlim[2]] <- NA
#     }
#   }
#   names(obj) = attr_names
#   obj <- as(obj, 'SpatialGridDataFrame')
#   spplot(obj,... , as.table=as.table)
# }

.spplot.brick.coerce = function(obj, ...) {
  if (obj@isSpatial) {
    if (obj@isTemporal) {
      if (tmin(obj) == tmax(obj)) {
        obj = slice(obj,tdim(obj),as.character(tmin(obj)))
      } else {
        stop("Cannot plot image with more than two dimensions, please use the 'slice' method to reduce one dimensioniality.")
      }
    }
  } else {
    stop("No spatial reference to use spplot with.")
  }
  brick = as(obj,"RasterBrick")
  spplot(brick, ...)
}


#' spplot for scidbst objects
#'
#' Like the raster version for spplot, this function plots a spatially referenced scidb array. To make this work on a
#' multidimensional array, the number of dimensions must be reduced to two spatial dimensions. This can
#' be done by using the 'slice' operation of scidb.
#'
#' @param obj scidbst object
#' @param ... Any argument that can be passed to spplot and levelplot
#' @param maxpixels Integer. Number of pixels to sample from each layer of large Raster objects
#'
#' @seealso \code{\link[raster]{spplot,Raster-method}}
#' @export
setMethod("spplot", signature(obj="scidbst"),.spplot.brick.coerce)
