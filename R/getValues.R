#' @include scidbst-class.R
NULL

# #' getValues method
# #'
# #' This function retrieves data from the remote scidb database and stores them internally like a Raster* object. This function
# #' work in principle like scidbs array materialization 'array []'. However this function also needs the multidimensional array
# #' to be reduced to a simple 2 dimensional array (spatial dimensions)
# #'
# #' @param x scidbst object
# #'
# #' @return vector or matrix of raster values
# #'
# #' @examples
# #' \dontrun{
# #' scidbconnect(...)
# #' array_proxy = scidbst(...)
# #' getValues(array_proxy)
# #' }
# #' @export
# setMethod("getValues", signature(x='scidbst', row='missing', nrows='missing'),
#           function(x) {
#             ndims = length(dimnames(x))
#             if (ndims > 2 ) {
#               stop("Too many dimensions detected. Try 'slice' to subset the image for example time.")
#             }
#
#             if (! hasValues(x) ) {
#                 x <- readAll(x)
#             }
#             #colnames(x@data@values) <- x@data@names
#             x@data@names = scidb_attributes(x)
#             colnames(x@data@values) <- scidb_attributes(x)
#
#             x@data@values
#           }
# )
