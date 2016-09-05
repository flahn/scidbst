#' @include TemporalExtent-class-decl.R
#' @include TRS-class-decl.R

#' Class scidbst
#'
#' Class \code{scidbst} inherits from class \code{scidb}
#'
#' @name scidbst-class
#' @rdname scidbst-class
#' @slot CRS The coordinate reference system used as class 'CRS' that represents a Proj.4 string
#' @slot extent The outer boundary of the SciDB array in referenced coordinates
#' @slot affine The affine transformation used to convert real-world coordinates into image frame coordinates
#' @slot spatial_dims the names of the spatial dimensions as a named list. 'xdim' describes the west-east axis and 'ydim' the north-south axis.
#' @slot tExtent the temporal extent as a \link{\code{TemporalExtent}} object
#' @slot isSpatial A flag whether or not this object has a spatial reference
#' @slot isTemporal A flag whether or not this object has a temporal reference
#' @slot sref A named list of elements that represent the spatial reference as specified in scidb by eo_getsrs
#' @slot tref A named list with the elements retrieved by eo_gettrs function
#' @slot trs The temporal reference object (\link{\code{TRS}})
#' @aliases scidbst
#' @exportClass scidbst
.scidbst_class = setClass("scidbst",
                          contains=list("scidb","RasterBrick"),
                          slots=c(
                            affine = "matrix",
                            sref = "list",
                            tref = "list",
                            spatial_dims = "list",
                            tExtent = "TemporalExtent",
                            isSpatial ="logical",
                            isTemporal = "logical",
                            trs = "TRS"
                          )
)
