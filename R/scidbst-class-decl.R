#' @include TemporalExtent-class-decl.R
#' @include TRS-class-decl.R
#' @include SRS-class-decl.R
NULL

# just a precaution, since the class was not exported in the package SciDBR (remvoed S3Methods=TRUE for now)
# setClass("scidb",
#          representation(name="character",
#                         meta="environment",
#                         gc="environment"),
#          S3methods = TRUE
# )

setClassUnion("SRSOrNULL",c("SRS","NULL"))

#' @importClassesFrom raster Extent
setClassUnion("ExtentOrNULL",c("Extent","NULL"))

setClassUnion("TRSOrNULL",c("TRS","NULL"))

setClassUnion("TemporalExtentOrNULL",c("TemporalExtent","NULL"))

setClassUnion("CharacterOrNULL",c("character","NULL"))

#' Class scidbst
#'
#' The scidbst class contains several objects in order to describe a SciDB array spatially and temporally. Therefore the
#' object holds a reference to the SciDB array via a scidb object and the dimension references for space and time with a
#' SRS and TRS object respectively.
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
#' @exportClass scidbst
setClass("scidbst",
                slots=c(
                  affine = "matrix",
                  title = "character",
                  extent = "ExtentOrNULL",
                  tExtent = "TemporalExtentOrNULL",
                  isSpatial ="logical",
                  isTemporal = "logical",
                  trs = "TRSOrNULL",
                  srs = "SRSOrNULL",
                  proxy = "ANY",
                  temps = "CharacterOrNULL"
                ),
                prototype = list(
                  title=character(1),
                  affine=matrix(),
                  isSpatial=logical(1),
                  isTemporal=logical(1),
                  trs=NULL,
                  srs=NULL,
                  extent=NULL,
                  tExtent=NULL,
                  proxy = NULL,
                  temps = NULL
                )
)
