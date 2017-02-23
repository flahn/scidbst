if (!isGeneric("transform")) {
  setGeneric("transform", function(`_data`, ...) standardGeneric("transform"))
}

#' @rdname transform-scidbst-method
#' @method transform scidbst
#' @export
transform.scidbst = function (`_data`, ...) {

  scidbst.obj = `_data`
  scidb.obj = as(scidbst.obj,"scidb")
  scidb.obj = transform(scidb.obj,...)
  scidbst.obj@proxy = scidb.obj

  return(scidbst.obj)
}

#' Transform / Apply function for scidbst object
#'
#' This function applies arithmetic operations on the attributes of an SciDB array.
#'
#' @aliases apply.scidbst transform.scidbst
#' @rdname transform-scidbst-method
#' @inheritParams base::transform
#'
#' @examples
#' \dontrun{
#' ls7_brazil_regrid = scidbst("LS7_brazil_regridded")
#' # Calculating the NDVI and MDVI of a Landsat 7 scene
#' ls7_calc = transform(ls7_brazil_regrid, ndvi = "(band4 - band3) / (band4 + band3)", mdvi = "(band8 - band3) / (band8 + band3)")
#' }
#'
#' @seealso \link{transform}, \link{transform.scidb}
#' @export
setMethod("transform", signature(`_data`="scidbst"), transform.scidbst)
