#' Get the spatial extent of a scidbst array
#'
#' This function let you retrieve the spatial extent as a Extent-class object. With the additional functions xmin, xmax, ymin and ymax
#' particular information about the extent can be retrieved directly from the scidbst object.
#'
#' @param x scidbst array
#' @return Extent-class object
#' @importFrom raster extent
#' @name extent,scidbst
#' @rdname extent-scidbst-methods
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

#' Sets the spatial extent of a scidb/scidbst array
#'
#' Sets the spatial extent of a scidb array or replaces the spatial extent of a scidbst array with an Extent-class object.
#'
#' @param x scidb or scidbst array
#' @param value An object of class Extent
#' @return scidbst object
#' @importFrom raster extent<-
#' @name extent<-,scidbst
#' @aliases setExtent,scidbst
#' @rdname setExtent-scidbst-methods
#' @export
setReplaceMethod("extent",signature(x="scidbst",value="Extent"), .setExtent)
#' @name extent<-,scidb
#' @aliases setExtent,scidb
#' @rdname setExtent-scidbst-methods
#' @export
setReplaceMethod("extent",signature(x="scidb",value="Extent"), .setExtent)



#' @name xmin,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("xmin",signature(x="scidbst"),function(x) {
  return(xmin(x@extent))
})

#' @name ymin,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("ymin",signature(x="scidbst"),function(x) {
  return(ymin(x@extent))
})

#' @name xmax,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("xmax",signature(x="scidbst"),function(x) {
  return(xmax(x@extent))
})

#' @name ymax,scidbst
#' @rdname extent-scidbst-methods
#' @export
setMethod("ymax",signature(x="scidbst"),function(x) {
  return(ymax(x@extent))
})
