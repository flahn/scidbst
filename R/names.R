#' names function
#'
#' Returns the names of the dimensions and attributes used in the remote scidb array.
#'
#' @note This function overwrites the standard S3 names function of raster.
#'
#' @param x scibst object
#' @return vector of character containing the names of dimensions and attributes
#' @export
#'
setMethod("names",signature(x="scidbst"), function(x) {
  return(c(c(dimensions(x),scidb_attributes(x))))
})
