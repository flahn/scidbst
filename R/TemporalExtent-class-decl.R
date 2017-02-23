#' Temporal Extent class
#'
#' This class represents a temporal interval with POSIXt dates. The constructor for this class is the function
#' \code{\link{textent,scidbst}}.
#'
#' @name TemporalExtent
#' @rdname TemporalExtent-class
#' @slot min POSIXt derived object defining the minimum inclusive boundary
#' @slot max POSIXt derived object defining the maximum inclusive boundary
#'
#' @examples
#' tmin = as.POSIXlt("2016-01-01")
#' tmax = strptime("01-02-2016",format="%d-%m-%Y")
#' ext = textent(tmin,tmax)
#'
#' @seealso \code{\link{textent,scidbst}}
#' @exportClass TemporalExtent
.textent_class = setClass("TemporalExtent",
                          slots=c(
                            min="POSIXt",
                            max="POSIXt"
                          )
)
