#' @export
setMethod("extent",signature(x="scidbst"), function(x) {
  if (x@isSpatial || !is.null(x@extent)) {
    return(x@extent)
  } else {
    stop("There is no spatial extent for this SciDB array")
  }
})
