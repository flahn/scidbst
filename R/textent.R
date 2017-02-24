#' @include scidbst-class-decl.R
#' @include TemporalExtent-class-decl.R
NULL

setGeneric("textent", function(x, ...) standardGeneric("textent"))

.textent.get = function(x) {
  if (!x@isTemporal) {
    stop(paste("Array",x@proxy@name,"does not have a temporal dimension."))
  }
  return(x@tExtent)
}

#' Getter and constructor for a TemporalExtent
#'
#' This constructor function creates a TemporalExtent object that defines an interval on the temporal dimension with a minimum
#' and maximum POSIXt value.
#' This method will return the stored temporal extent that contains the minimum and maximum date values in a list. The list will be
#' empty if the array has no temporal dimension.
#'
#' @name textent,scidbst
#' @rdname textent-methods
#' @param x a scidbst object or a POSIXt derived object for the minimum inclusive boundary
#' @param y A POSIXt derived object for the maximum inclusive boundary
#'
#' @return The temporal extent as a \code{\link{TemporalExtent}} object
#'
#' @examples
#' # manually creating a TemporalExtent object
#' tmin = as.POSIXlt("2016-01-01")
#' tmax = strptime("01-02-2016",format="%d-%m-%Y")
#' ext = textent(tmin,tmax)
#'
#' \dontrun{
#' # Get the temporal extent from a scidbst object
#' st.arr = scidbst("st_array")
#' ext = textent(st.arr)
#' }
#' @export
setMethod("textent",signature(x="scidbst"), .textent.get)


.textent.create = function(x, y) {
  min = x

  if (missing(y)) {
    warning("No upper boundary was set. Using the minimum boundary to create an temporal incident.")
    max = x
  } else {
    max = y
  }

  if (!(inherits(min,"POSIXt") && inherits(max,"POSIXt"))) {
    stop("Minimum or maximum value is not a POSIXt value.")
  }

  out = .textent_class()
  if (max < min) {
    out@min = max
    out@max = min
  } else {
    out@min = min
    out@max = max
  }
  return(out)
}

#' @rdname textent-methods
#' @export
setMethod("textent",signature(x="POSIXt"), .textent.create)
