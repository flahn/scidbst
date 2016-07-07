setGeneric("getXDim",function(x) standardGeneric("getXDim"))

#' Get spatial x dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a West-East dimension. If the object is not
#' spatially referenced it will return the second dimension name.
#'
#' @param x scidbst object
#' @return The name of the spatia West-East dimension or the first dimension name
#' @export
setMethod("getXDim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(x@spatial_dims$xdim)
  } else {
    return(dimensions(x)[2])
  }
})

setGeneric("getYDim",function(x) standardGeneric("getYDim"))

#' Get spatial y dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a North-South dimension. If the object is not
#' spatially referenced it will return the first dimension name.
#'
#' @param x scidbst object
#' @return The name of the spatia North-South dimension or the first dimension name
#' @export
setMethod("getYDim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(x@spatial_dims$ydim)
  } else {
    return(dimensions(x)[1])
  }
})

setGeneric("getTDim",function(x) standardGeneric("getTDim"))

#' Get temporal dimension name
#'
#' Returns the name of the temporal dimension of a scidbst array. If the object is not temporal it will
#' return the first dimension name.
#'
#' @param x scidbst object
#' @return The temporal dimension name or the first dimension name
#' @export
setMethod("getTDim",signature(x="scidbst"),function(x){
  if (x@isTemporal) {
    return(x@temporal_dim)
  } else {
    return(dimensions(x)[1])
  }
})
