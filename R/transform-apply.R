if (!isGeneric("transform")) {
  setGeneric("transform")
}

#' @rdname transform-scidbst-method
#' @method transform scidbst
#' @export
transform.scidbst = function (`_data`, ...) {

  scidbst.obj = `_data`
  scidb.obj = .toScidb(scidbst.obj)
  scidb.obj = transform(scidb.obj,...)
  out = .scidbst_class(scidb.obj)
  out = .cpMetadata(scidbst.obj,out)

  return(out)
}

#' Transform / Apply function for scidbst object
#'
#' This function overrides the S3method 'transform' and provides the same functionality as the 'apply' in SciDB.
#'
#' @aliases apply.scidbst transform.scidbst
#' @rdname transform-scidbst-method
#' @inheritParams base::transform
#'
#' @seealso \link{transform}
#' @export
setMethod("transform", signature(`_data`="scidbst"), transform.scidbst)
