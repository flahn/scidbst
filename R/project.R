if (!isGeneric("project")) {
  setGeneric("project", function(x, ...) standardGeneric("project"))
}

.project.scidbst = function(x,attributes) {
  .scidb.object = as(x,"scidb")

  .scidb.object = scidb::project(x=.scidb.object, attributes=attributes)
  x@proxy = .scidb.object
  return(x)
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
#' @name project,scidbst
#' @export
setMethod("project",signature(x="scidbst"), .project.scidbst)
