setGeneric("t.extent", function(x, ...) standardGeneric("t.extent"))


#' Extracts the temporal extent of an scidbst array
#'
#' This method will return the stored temporal extent that contains the minimum and maximum date values in a list. The list will be
#' empty if the array has no temporal dimension.
#'
#' @param x a scidbst object
#'
#' @return The temporal extent as a list of POSIXlt values
#'
#' @export
setMethod("t.extent",signature(x="scidbst"), function(x) {
  if (!x@isTemporal) {
    warn(paste("Array",x@title,"does not have a temporal dimension."))
    list()
  } else {
    x@tExtent
  }
})
