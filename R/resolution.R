#' @include scidbst-class-decl.R
NULL

#' Spatial resolution
#'
#' Returns information about the spatial resolution as numeric values, e.g.(xres,yres), xres or yres or in terms of number of
#' cells (ncol, nrow).
#'
#' The spatial resolution is calculated with the spatial extent and the number of rows or columns. The number of rows and columns
#' can be derived by transforming the spatial extent with affine transformation into the dimensional index values and calculating
#' the differences between the minimum and maximum value for each dimension.
#' Therefore the spatial resolution describes the distances between the regular parallel lines throughout the image.
#'
#' @name resolution,scidbst
#' @aliases res,scidbst ncol,scidbst nrow,scidbst
#' @rdname resolution-scidbst-methods
#' @param x scidbst object
#' @return numeric or numeric vector
#'
#' @examples
#' \dontrun{
#' arr = scidbst("sp_array")
#' res(arr)
#' xres(arr)
#' yres(arr)
#' nrow(arr)
#' ncol(arr)
#' }
#' @include extent.R
#' @include nrow.R
#' @include ncol.R
#' @export
setMethod("res", signature(x="scidbst"), function(x) {
  if (x@isSpatial) {
    return(c(xres(x),yres(x)))
  }
})

#' @rdname resolution-scidbst-methods
#' @export
setMethod("xres", signature(x="scidbst"), function(x) {
  e = extent(x)
  dx = xmax(e)-xmin(e)
  ncol = .ncol(x)
  return(dx/ncol)
})

#' @rdname resolution-scidbst-methods
#' @export
setMethod("yres", signature(x="scidbst"), function(x) {
  e = extent(x)
  dy = ymax(e)-ymin(e)
  nrow = .nrow(x)
  return(dy/nrow)
})





