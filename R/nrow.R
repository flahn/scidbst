#' Number of rows
#'
#' returns the number of rows
#'
#' @param x scibst
#' @return number of rows for a scidbst object
#'
#' @export
setMethod("nrow",signature(x="scidbst"),function(x) {
  if (x@isSpatial) {
    lengths = .getLengths(x)
    if (length(lengths) == 1) {
      return(1)
    } else {
      return(lengths[getYDim(x)])
    }
  } else if (x@isTemporal){
    return(1)
  }
  stop("Did not expect to go here.")
})
