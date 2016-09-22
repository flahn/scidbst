#' @include scidbst-class.R

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
