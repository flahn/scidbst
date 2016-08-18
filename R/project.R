if (!isGeneric("project")) {
  setGeneric("project", function(x, ...) standardGeneric("project"))
}

.project.scidbst = function(x,attributes) {
  .scidb.object = .toScidb(x)

  .scidb.object = scidb::project(x=.scidb.object, attributes=attributes)
  out = .scidbst_class(.scidb.object)
  out = .cpMetadata(x,out)
  return(out)
}

#' Project onto a subset of attributes in a scidbst object
#'
#' This function will create a selection of attributes by passing the attribute statement to the scidb project function.
#'
#' @param x \code{scidbst} object
#' @param attributes a character vector of array attribute names or a numeric vector of attribute positions
#' @return a new \code{scidbst} object
#'
#' @seealso \code{\link[scidb]{project}}
#' @export
setMethod("project",signature(x="scidbst"), .project.scidbst)
