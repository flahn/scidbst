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
    #dim = x@spatial_dims$ydim
    extent = .calculateDimIndices(x,extent(x))
    return(extent@ymax-extent@ymin+1)
  } else if (x@isTemporal){
    return(1)
  } else {
    lengths = .getLengths(x)
    if (length(lengths) == 1) {
      return(1)
    } else {
      return(lengths[getYDim(x)])
    }
  }
})
