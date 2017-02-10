#' Gets and sets the spatial extent of a scidb/scidbst array
#'
#' This function let you retrieve the spatial extent as a Extent-class object or it
#' assigns or replaces an extent for a scidb/scidbst object.
#'
#' @importFrom raster extent
#' @rdname extent-scidbst-methods
#' @export
setMethod("extent",signature(x="scidbst"), function(x) {
  if (x@isSpatial || !is.null(x@extent)) {
    return(x@extent)
  } else {
    stop("There is no spatial extent for this SciDB array")
  }
})

.setExtent = function(x,value) {
  if(class(x) == "scidb") {
    .scidbst = new("scidbst")
    .scidbst@proxy = x
    .scidbst@extent = value

    x = .scidbst
  } else if (class(x) == "scidbst") {
    x@extent = value
  }

  if (!is.null(x@srs) && !is.null(x@affine)) {
    x@isSpatial = TRUE
  }
  return(x)
}

if (!isGeneric("extent<-")) {
  setGeneric("extent<-",function(x,value) standardGeneric("extent<-"))
}
#' @rdname extent-scidbst-methods
#' @export
setReplaceMethod("extent",signature(x="scidbst",value="Extent"), .setExtent)
#' @rdname extent-scidbst-methods
#' @export
setReplaceMethod("extent",signature(x="ANY",value="Extent"), .setExtent)
