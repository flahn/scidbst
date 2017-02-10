#' @include scidbst-class-decl.R
NULL

.ncol = function(x) {
  if (x@isSpatial) {
    indices = .calculateDimIndices(x,extent(x))
    ncols=xmax(indices)-xmin(indices)
    return(ncols)

  } else if (x@isTemporal) {
    # delta_t/t_res
    return((as.numeric(difftime(tmax(x),tmin(x),units=tunit(x)))+1)/tres(x))
  }
}

#' Number of columns
#'
#' returns the number of columns
#'
#' @param x scibst
#' @return number of columns for a scidbst object
#'
#' @name ncol,scidbst
#' @export
setMethod("ncol",signature(x="scidbst"),.ncol)
