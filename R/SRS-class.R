#' @include SRS-class-decl.R
#' @include scidbst-class-decl.R
NULL

#' SRS class constructor
#'
#' Creates a new SRS object with projection arguments and dimension names (y,x)-order
#'
#' @param projargs character - The projection arguments as defined by PROJ4
#' @param dimnames character - The dimension names
#' @rdname SRS-class
#' @export
SRS = function(projargs, dimnames) {
  .srs = .srs_class()
  .srs@projargs = projargs
  .srs@dimnames = dimnames
  return(.srs)
}

#' @export
setMethod("crs",signature(x="scidbst"),function(x) {
  return(CRS(x@srs@projargs))
})

if (!isGeneric("xdim")) {
  setGeneric("xdim",function(x) standardGeneric("xdim"))
}

#' Get spatial x dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a West-East dimension. If the object is not
#' spatially referenced it will return the second dimension name.
#'
#' @name xdim
#' @rdname xdim-method
#' @param x scidbst object
#' @return The name of the spatia West-East dimension or the first dimension name
#' @export
setMethod("xdim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(xdim(x@srs))
  } else {
    warning("No explicit spatial reference found. Using second dimension instead.")
    return(dimensions(x)[2])
  }
})

#' @rdname xdim-method
#' @export
setMethod("xdim",signature(x="SRS"),function(x){
  return(x@dimnames[2])
})

if (!isGeneric("ydim")){
  setGeneric("ydim",function(x) standardGeneric("ydim"))
}

#' Get spatial y dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a North-South dimension. If the object is not
#' spatially referenced it will return the first dimension name.
#'
#' @name ydim
#' @rdname ydim-method
#' @param x scidbst object
#' @return The name of the spatia North-South dimension or the first dimension name
#' @export
setMethod("ydim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(ydim(x@srs))
  } else {
    warning("No explicit spatial reference found. Using first dimension instead.")
    return(dimensions(x)[1])
  }
})

#' @rdname ydim-method
#' @export
setMethod("ydim",signature(x="SRS"),function(x){
  return(x@dimnames[1])
})

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
