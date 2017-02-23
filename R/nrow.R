#' @include scidbst-class-decl.R
NULL

.nrow = function(x) {
  if (x@isSpatial) {
    indices = .calculateDimIndices(x,extent(x))
    nrows = ymax(indices)-ymin(indices)
    return(nrows)
  } else if (x@isTemporal){
    return(1)
  }
  stop("Did not expect to go here. Probably no spatial or temporal reference")
}

#' @rdname resolution-scidbst-methods
#' @export
setMethod("nrow",signature(x="scidbst"),.nrow)
