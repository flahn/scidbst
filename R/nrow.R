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

#' Number of rows
#'
#' returns the number of rows
#'
#' @param x scibst
#' @return number of rows for a scidbst object
#' @name nrow,scidbst
#' @export
setMethod("nrow",signature(x="scidbst"),.nrow)
