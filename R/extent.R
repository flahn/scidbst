#' @include scidbst-class-decl.R
NULL

#' Gets and sets the spatial extent of a scidbst array
#'
#' This function let you retrieve the spatial extent as a Extent-class object. With the additional functions xmin, xmax, ymin and ymax
#' particular information about the extent can be retrieved directly from the scidbst object. Using the "<-" operator a spatial extent can be set for a scidb array or it replaces the spatial extent of a scidbst
#' array with an Extent-class object.
#'
#' @param x scidb or scidbst array
#' @param value An object of class Extent
#' @return \code{\link[raster]{Extent-class}} object or \code{\link{scidbst}} object
#' @importFrom raster extent
#' @name extent,scidbst
#' @rdname extent-scidbst-methods
#'
#' @note Setting the extent with \code{extent<-} has an informative meaning. By setting the extent on a scidbst object the
#' extent will be replaced, but NO cropping or subsetting will be performed.
#'
#' @seealso \code{\link[raster]{extent}}
#' @examples
#' \dontrun{
#' arr = scidbst("sp_ref_array")
#' extent(arr)
#' xmin(arr)
#' xmax(arr)
#' ymin(arr)
#' ymax(arr)
#' }
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

#' @importFrom raster extent<-
#' @aliases setExtent,scidbst extent<-,scidbst
#' @rdname extent-scidbst-methods
#' @export
setReplaceMethod("extent",signature(x="scidbst",value="Extent"), .setExtent)
#' @aliases setExtent,scidb extent<-,scidb
#' @rdname extent-scidbst-methods
#' @export
setReplaceMethod("extent",signature(x="scidb",value="Extent"), .setExtent)



#' @aliases xmin,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("xmin",signature(x="scidbst"),function(x) {
  return(xmin(x@extent))
})

#' @aliases ymin,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("ymin",signature(x="scidbst"),function(x) {
  return(ymin(x@extent))
})

#' @aliases xmax,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("xmax",signature(x="scidbst"),function(x) {
  return(xmax(x@extent))
})

#' @aliases ymax,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("ymax",signature(x="scidbst"),function(x) {
  return(ymax(x@extent))
})
