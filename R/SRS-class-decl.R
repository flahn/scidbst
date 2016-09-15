#' Spatial Reference Class
#'
#' This class holds information about the spatial dimensions. In particular, the dimension names and the projection arguments. The projection
#' arguments are inherited from the \code{\link[sp]{CRS}} class
#'
#' @rdname SRS-class
#' @name SRS
#' @slot dimnames character - The names for the dimensions in the order y-axis (South-North), x-axis(West-East)
#' @importClassesFrom sp CRS
#' @exportClass SRS
.srs_class = setClass("SRS",
                      contains=list("CRS"),
                      representation(dimnames="character")

)


