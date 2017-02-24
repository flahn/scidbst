setGeneric("affine", function(x) standardGeneric("affine"))
setGeneric("affine<-", function(x,value) standardGeneric("affine<-"))

#' Handling for the affine transformation on scidb(st) arrays
#'
#' Returns the affine transformation as a 2x3 matrix (x0,xres(x),xshear(x) \\ y0, yshear(x), yres(x)) or sets the affine transformation
#' for a scidb or scidbst object.
#'
#' @name affine,scidbst
#' @aliases affine
#' @rdname affine-scidbst-methods
#' @param x scidbst object
#' @return a numeric matrix containing the affine transformation parameter
#' @examples
#' \dontrun{
#'  # Getter
#'  .scidbst = scidbst("ref_array")
#'  aff.trans = affine(.scidbst)
#'
#'  # Setter
#'  .scidb = scidb("an_array")
#'  m = matrix(c(1000,1,0, 1000,0,1),byrow=TRUE,nc=3,nr=2)
#'  affine(.scidb) <- m
#'
#' }
#' @export
setMethod("affine",signature(x="scidbst"),function(x) {
  if (is.spatial(x) || !is.null(x@affine)) {
    return(x@affine)
  } else {
    stop("The array is not spatial. There is no affine transformation.")
  }
})

.setAffine = function(x,value) {
  value = na.omit(value)
  if ( nrow(value) == 0 || !(ncol(value) == 3 && nrow(value) == 2) ) {
    stop("Cannot set or replace affine transformation, because the assigned matrix is no affine transformation of dimension 2x3.")
  }

  if (class(x) == "scidb") {
    .scidbst = new("scidbst")
    .scidbst@proxy = x
    .scidbst@affine = value
    x = .scidbst
  } else if (class(x) == "scidbst") {
    x@affine = value
  }

  if (!is.null(x@srs) && !is.null(x@extent)) {
    x@isSpatial = TRUE
  }
  return(x)
}

#' @name affine<-,scidbst
#' @rdname affine-scidbst-methods
#' @export
setReplaceMethod("affine",signature(x="scidbst",value="matrix"),.setAffine)

#' @name affine<-,scidb
#' @rdname affine-scidbst-methods
#' @export
setReplaceMethod("affine",signature(x="scidb",value="matrix"),.setAffine)
