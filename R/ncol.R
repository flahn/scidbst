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

#' @rdname resolution-scidbst-methods
#' @export
setMethod("ncol",signature(x="scidbst"),.ncol)
