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
#' @slot trs The temporal reference object (\code{\link{TRS}})
#' @slot srs The spatial reference object \code{\link{SRS}}
#' @slot proxy scidb proxy \code{\link[scidb]{scidb}}
#' @slot extent spatial \code{\link[raster]{Extent}}
#' @slot title The name of the original array
#' @aliases scidbst
#' @importClassesFrom raster Extent
#' @exportClass scidbst
.scidbst_class = setClass("scidbst",
                          slots=c(
                            affine = "matrix",
                            tref = "list",
                            title = "character",
                            extent = "Extent",
                            tExtent = "TemporalExtent",
                            isSpatial ="logical",
                            isTemporal = "logical",
                            trs = "TRS",
                            srs = "SRS",
                            proxy = "ANY"
                          )
)
