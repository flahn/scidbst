#' @export
setMethod("xres", signature(x="scidbst"), function(x) {
  e = extent(x)
  dx = xmax(x)-xmin(x)
  return(dx/ncol(x))
})

#' @export
setMethod("yres", signature(x="scidbst"), function(x) {
  e = extent(x)
  dy = ymax(x)-ymin(x)
  return(dy/nrow(x))
})

#' @export
setMethod("res", signature(x="scidbst"), function(x) {
  return(c(xres(x),yres(x)))
})
