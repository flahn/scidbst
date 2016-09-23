#' Temporal Extent class
#'
#' This class represents a temporal interval with POSIXt dates.
#'
#' @name TemporalExtent
#' @rdname TemporalExtent-class
#' @slot min POSIXt derived object defining the minimum inclusive boundary
#' @slot max POSIXt derived object defining the maximum inclusive boundary
#' @exportClass TemporalExtent
.textent_class = setClass("TemporalExtent",
                          slots=c(
                            min="POSIXt",
                            max="POSIXt"
                          )
)
