#' @include scidbst-class.R
NULL

if (!isGeneric("slice")) {
  setGeneric("slice", function(x,d,n) standardGeneric("slice"))
}

.slice = function (x,d,n) {
  out = .scidbst_class(scidb::slice(x,d,n))

  out = .cpMetadata(x,out)
  if (d %in% x@temporal_dim) {
    baseTime = 0

    if (x@tUnit == "weeks") {
      baseTime = 7*24*60*60
    } else if (x@tUnit == "days") {
      baseTime = 24*60*60
    } else if (x@tUnit == "hours") {
      baseTime = 60 * 60
    } else if (x@tUnit == "mins") {
      baseTime = 60
    } else if (x@tUnit == "secs") {
      baseTime = 1
    } else {
      stop("currently no other temporal unit supported")
    }

    #adapt temporal extent
    newStart = as.POSIXlt(as.character(x@startTime + n * x@tResolution * baseTime))
    newEnd = as.POSIXlt(as.character(x@startTime + (n+1) * x@tResolution * baseTime))
    out@tExtent[["min"]] = newStart
    out@tExtent[["max"]] = newEnd
    out@isTemporal = FALSE
    # it is not temporal anymore since the dimension is removed, even though the temporal information is still
    # present in the R object
  }

  return(out)
}

#' Slice the array
#'
#' Takes a dimension name and a value to create a slice of an array. This usually means reducing the dimensions
#' of an array.
#'
#' @inheritParams scidb::slice
#' @return scidbst A new scidbst object with reduced number of dimensions
#' @export
setMethod('slice', signature(x="scidbst",d="character",n="ANY") , function(x,d,n) {
  if (d %in% x@temporal_dim) {
    if (is.character(n) ) {
      #|| !tryCatch(is.na.POSIXlt(n,error=function(e) {return(TRUE)}))
      index = suppressWarnings(as.numeric(n)) #disable warnings that might result from converting a plain string
      if (is.na(index)) {
        n = .calcTDimIndex(x,n)
      } else {
        n = round(index)
      }
    } else if (is.numeric(n)) {
      n = round(n)
    } else {
      stop("Not recognized value for time dimension during slice operation")
    }
  }

  return(.slice(x,d,n))
})
