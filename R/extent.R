#' @export
setMethod("extent",signature(x="scidbst"), function(x) {
  return(x@extent)
})
