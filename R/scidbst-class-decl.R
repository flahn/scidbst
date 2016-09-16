#' @include TemporalExtent-class-decl.R
#' @include TRS-class-decl.R
#' @include SRS-class-decl.R
NULL

#' Class scidbst
#'
#' Class \code{scidbst} inherits from class \code{scidb}
#'
#' @name scidbst
#' @rdname scidbst-class
#' @slot extent The outer boundary of the SciDB array in referenced coordinates
#' @slot affine The affine transformation used to convert real-world coordinates into image frame coordinates
#' @slot tExtent the temporal extent as a \code{\link{TemporalExtent}} object
#' @slot isSpatial A flag whether or not this object has a spatial reference
#' @slot isTemporal A flag whether or not this object has a temporal reference
#' @slot tref A named list with the elements retrieved by eo_gettrs function
#' @slot trs The temporal reference object (\code{\link{TRS}})
#' @slot srs The spatial reference object \code{\link{SRS}}
#' @slot proxy scidb proxy \code{\link[scidb]{scidb}}
#' @aliases scidbst
#' @exportClass scidbst
.scidbst_class = setClass("scidbst",
                          contains=list("RasterBrick"),
                          slots=c(
                            affine = "matrix",
                            tref = "list",
                            tExtent = "TemporalExtent",
                            isSpatial ="logical",
                            isTemporal = "logical",
                            trs = "TRS",
                            srs = "SRS",
                            proxy = "ANY"
                          )
)
