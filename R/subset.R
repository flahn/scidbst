if (!isGeneric("subset")) {
  setGeneric("subset", function(x, ...) standardGeneric("subset"))
}

#' @rdname subset-scidbst-method
#' @method subset scidbst
#' @export
subset.scidbst = function(x, ...) {
  .scidb.object = as(x,"scidb")

  .scidb.object = scidb:::filter_scidb(.scidb.object, ...)
  x@proxy = .scidb.object
  return(x)
}


#' Subset / Filter method for scidbst objects
#'
#' This function wraps scidbs subset function for scidbst objects. Internally, scidb uses its \code{filter} function to create the
#' subset. To formulate the filter expression, please have a look at the scidb documentation (\code{\link{subset.scidb}}).
#' Since scidbs filter function is used, the resulting array will probably be sparse.
#' Out of convenience the subset operation can also be used with filter instead of \code{subset}.
#'
#' @name subset,scidbst
#' @rdname subset-scidbst-method
#' @param x scidbst object
#' @param ... filter expression
#' @return a scidbst object
#'
#' @note It is recommended to use the string based SciDB filter syntax.
#'
#' @examples
#' \dontrun{
#' scidbconnect()
#' ls7_ndvi = scidbst("LS7_BRAZIL_NDVI")
#'
#' #using subset
#' ls7_ndvi_gr05 = subset(ls7_ndvi, "NDVI > 0.5")
#'
#' # using filter
#' ls7_ndvi_sm05 = filter(ls7_ndvi, "NDVI < 0.5 and height > 1500")
#' }
#' @seealso \code{\link{subset.scidb}}
#' @export
setMethod("subset",signature(x="scidbst"), subset.scidbst)

if (!isGeneric("filter")) {
  setGeneric("filter", function(x, ...) standardGeneric("filter"))
}

#' @name filter,scidbst
#' @rdname subset-scidbst-method
#' @export
setMethod("filter",signature(x="scidbst"), subset.scidbst)
