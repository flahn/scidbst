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
    extent = .calculateDimIndices(x,extent(x))
    return(extent@xmax-extent@xmin+1)
  } else if (x@isTemporal) {
    return(as.numeric(difftime(x@tExtent[["max"]],x@tExtent[["min"]],x@tUnit))+1)
  } else {
    lengths = .getLengths(x)
    if (length(lengths) == 1) {
      return(lengths[1])
    } else {
      return(lengths[getXDim(x)])
    }
  }
})
