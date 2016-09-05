#' Number of columns
#'
#' returns the number of columns
#'
#' @param x scibst
#' @return number of columns for a scidbst object
#'
#' @export
setMethod("ncol",signature(x="scidbst"),function(x) {
  if (x@isSpatial) {
    if (!hasValues(x)) {
      lengths = .getLengths(x)
      if (length(lengths) == 1) {
        return(lengths[1])
      } else {
        return(lengths[getXDim(x)])
      }
    } else {
      return(x@ncols)
    }

  } else if (x@isTemporal) {
    return(as.numeric(difftime(tmax(x),tmin(x),tunit(x)))+1)
  }
})
