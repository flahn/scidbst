#' Gets and sets the Coordinate Reference System for a scidb/scidbst object
#'
#' Returns the coordinate reference system of a scidbst object in form of a CRS object. The method can also be used to
#' set or replace the CRS for a scidb or scidbst object.
#'
#' @note The changes only apply in R. To persist the changes, make the object truly spatial by setting also the
#' affine transformation and the spatial extent and then run scidbsteval.
#'
#' @param x scidb or scidbst object
#' @param value An CRS-class object
#' @param ... additional parameter passed to \link[raster]{crs}
#' @return A \link{scidbst} object for set, a \link[rgdal]{CRS} object for get

#' @name crs,scidbst
#' @rdname crs-scidbst-methods
#' @seealso \code{\link[raster]{projection}}, \code{\link[sp]{CRS-class}}
#' @export
setMethod("crs",signature(x="scidbst"),function(x, ...) {
  return(CRS(x@srs@projargs))
})

.setCRS = function(x,value) {
  if (class(x) == "scidb") {
    .scidbst = new("scidbst")
    .scidbst@proxy = x
    .scidbst@srs = new("SRS")
    .scidbst@srs@projargs <- value@projargs
    return(.scidbst)
  }

  if (class(x) == "scidbst") {
    x@srs@projargs <- value@projargs

    if (length(x@affine) == 6 && !is.null(x@extent)) {
      x@isSpatial = TRUE
    }
  }
  return(x)
}

if (!isGeneric("crs<-")) {
  setGeneric("crs<-", function(x,value) standardGeneric("crs<-"))
}

#' @importFrom raster crs<-
#' @name crs<-,scidbst
#' @aliases setCRS,scidbst
#' @rdname crs-scidbst-methods
#' @export
setReplaceMethod("crs", signature(x="scidbst", value="CRS"), .setCRS)
#' @name crs<-,scidb
#' @aliases setCRS,scidb
#' @rdname crs-scidbst-methods
#' @export
setReplaceMethod("crs", signature(x="scidb", value="CRS"), .setCRS)
