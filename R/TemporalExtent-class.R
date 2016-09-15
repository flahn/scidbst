#' @include scidbst-class-decl.R
#' @include TemporalExtent-class-decl.R
NULL



#' TemporalExtent class constructor
#'
#' This constructor function creates a TemporalExtent object that defines an interval on the temporal dimension.
#' @rdname TemporalExtent-class
#' @param min A POSIXt derived object for the minimum inclusive boundary
#' @param max A POSIXt derived object for the maximum inclusive boundary
#' @return TemporalExtent object
#'
#' @examples
#' tmin = as.POSIXlt("2016-01-01")
#' tmax = strptime("01-02-2016",format="%d-%m-%Y")
#' ext = textent(tmin,tmax)
#' @export
textent = function(min, max) {
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

if (!isGeneric("tmin")) {
  setGeneric("tmin",function(x) {
    standardGeneric("tmin")
  })
}
if (!isGeneric("tmax")) {
  setGeneric("tmax",function(x) {
    standardGeneric("tmax")
  })
}


#' Minimum temporal boundary
#'
#' Return the minimum boundary of a TemporalExtent object
#' @rdname tmin-method
#' @name tmin
#' @param x TemporalExtent
#' @return POSIXt object referring to the minimum boundary
#'
#' @export
setMethod("tmin",signature(x="TemporalExtent"),function(x) {
  return(x@min)
})

#' @rdname tmin-method
#' @export
setMethod("tmin",signature(x="scidbst"),function(x) {
  return(tmin(x@tExtent))
})

#' Maximum temporal boundary
#'
#' Return the maximum boundary of a TemporalExtent object
#' @rdname tmax-method
#' @name tmax
#' @param x TemporalExtent
#' @return POSIXt object referring to the maximum boundary
#'
#' @export
setMethod("tmax",signature(x="TemporalExtent"),function(x) {
  return(x@max)
})

#' @rdname tmax-method
#' @export
setMethod("tmax",signature(x="scidbst"),function(x) {
  return(tmax(x@tExtent))
})

#' Show method
#'
#' creates a print version of the TemporalExtent object
#'
#' @param x TemporalExtent
#'
#' @export
setMethod("show",signature(object="TemporalExtent"),function(object){
  cat("Temporal Extent:\n")
  cat(paste("\tFrom:\t",object@min,"\n",sep=""))
  cat(paste("\tTo:\t",object@max,"\n",sep=""))
})
