#' @include SRS-class.R
NULL
##############################
# dimensions
##############################

if (!isGeneric("dimensions")) {
  setGeneric("dimensions",function(obj){
    standardGeneric("dimensions")
  })
}

#' Dimension operations
#'
#' The listed functions are used to find the correct naming for the dimensions in order to address them correctly in other operations.
#' The x dimension relates to a West-East dimension, whereas the y dimension relates to the North-South dimension. Also the function tdim
#' returns the name of the temporal dimension used in SciDB.
#'
#' @name dimensions,scidbst
#' @rdname dimension-scidbst-methods
#' @param obj scidbst object
#' @param x scidbst object
#' @return character refering to the name of the selected dimension(s).
#'
#' @importFrom sp dimensions
#' @seealso \code{\link[scidb]{dimensions}}
#' @examples
#' \dontrun{
#' starr = scidbst("st_array")
#' dimensions(starr)
#' xdim(starr)
#' ydim(starr)
#' tdim(starr)
#' }
#' @export
setMethod("dimensions",signature(obj="scidbst"), function(obj) {
  .scidb = as(obj,"scidb")
  dims = scidb::dimensions(.scidb)
  return(dims)
})

#' @name dimensions,scidb
#' @rdname dimension-scidbst-methods
#' @export
setMethod("dimensions",signature(obj="scidb"), function(obj) {
  return(scidb::dimensions(x=obj))
})


if (!isGeneric("xdim")) {
  setGeneric("xdim",function(x) standardGeneric("xdim"))
}

#' @name xdim,scidbst
#' @rdname dimension-scidbst-methods
#' @export
setMethod("xdim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(xdim(x@srs))
  } else {
    warning("No explicit spatial reference found. Using second dimension instead.")
    return(dimensions(x)[2])
  }
})

#' @name xdim,SRS
#' @rdname dimension-scidbst-methods
#' @export
setMethod("xdim",signature(x="SRS"),function(x){
  return(x@dimnames[2])
})

if (!isGeneric("ydim")){
  setGeneric("ydim",function(x) standardGeneric("ydim"))
}

#' @name ydim,scidbst
#' @rdname dimension-scidbst-methods
#' @export
setMethod("ydim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(ydim(x@srs))
  } else {
    warning("No explicit spatial reference found. Using first dimension instead.")
    return(dimensions(x)[1])
  }
})

#' @name ydim,SRS
#' @rdname dimension-scidbst-methods
#' @export
setMethod("ydim",signature(x="SRS"),function(x){
  return(x@dimnames[1])
})

#' @name tdim,TRS
#' @rdname dimension-scidbst-methods
#' @export
setMethod("tdim",signature(x="TRS"),function(x) {
  return(x@dimname)
})

#' @name tdim,scidbst
#' @rdname dimension-scidbst-methods
#' @export
setMethod("tdim",signature(x="scidbst"),function(x) {
  if (x@isTemporal) {
    return(tdim(x@trs))
  } else {
    warning("scidbst object has no temporal reference. Returning first dimension.")
    return(dimensions(x)[1])
  }
})
