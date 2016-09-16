#' Spatial Reference Class
#'
#' This class holds information about the spatial dimensions. In particular, the dimension names and the projection arguments. The projection
#' arguments are inherited from the \code{\link[sp]{CRS}} class
#'
#' @rdname SRS-class
#' @name SRS
#' @slot dimnames character - The names for the dimensions in the order y-axis (South-North), x-axis(West-East)
#' @slot authority character - The name of the authority that provides the information about the reference system
#' @slot srid integer - The integer identifier that 'names' the spatial reference system
#' @slot srtext character - The textual description of the reference system in OGC Well Known Text (WKT) format
#' @importClassesFrom sp CRS
#' @exportClass SRS
.srs_class = setClass("SRS",
                      contains=list("CRS"),
                      representation(
                        dimnames="character",
                        authority="character",
                        srid="integer",
                        srtext="character"
                      )

)


