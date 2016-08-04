if (!isGeneric("cumulate")) {
  setGeneric("cumulate", function(x, ...) standardGeneric("cumulate"))
}

#' # Do this to make life with scidb::cumulate easier!
#' cumulate.scidb = scidb::cumulate
#'
#' #' @importFrom scidb cumulate
#' #' @export
#' setMethod("cumulate", signature(x="scidb"), cumulate.scidb)

.cumulate.scidbst = function (x, expression, dimension) {

  scidbst.obj = x
  scidb.obj = .toScidb(scidbst.obj)
  scidb.obj = scidb::cumulate(scidb.obj, expression, dimension)
  out = .scidbst_class(scidb.obj)
  out = .cpMetadata(scidbst.obj,out)

  return(out)
}

#' Cumulate function for scidbst objects
#'
#' This function wraps scidbs cumulate function in order to maintain the scidbst object with the dimension references.
#'
#' @aliases cumulate.scidbst

#' @param x scidbst object
#' @param expression any valid SciDB aggregate expression as a character string
#' @param dimension optional parameter, the dimension along which the cumulative sum or product will be calculates
#'
#' @return a scidbst object
#'
#' @seealso \link{scidb::cumulate}
#' @export
setMethod("cumulate", signature(x="scidbst"), .cumulate.scidbst)


