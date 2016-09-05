#' @include scidbst-class-decl.R
#' @include TRS-class-decl.R
NULL

TRS = function(dimension,t0, tres, tunit) {
  if (! inherits(t0,"POSIXt")) {
    if (inherits(t0,"character")) {
      t0 = as.POSIXct(t0)
    } else {
      stop("Cannot set datum for temporal reference.")
    }
  }
  if (!inherits(tunit,"character") && !tunit %in% c("days","weeks","secs","mins","hours")) {
    stop("Cannot recognize temporal unit")
  }
  if (! is.numeric(tResolution)) {
    stop("Cannot establish the temporal resolution")
  }

  if (is.character(dimension)) {
    if (length(dimension) == 0) {
      warning("No dimension name specified. Using 't' as a default.")
      dimension = "t"
    }
  } else {
    stop("Cannot create TRS, because of no stated dimension name.")
  }
  .trs = .trs_class()
  .trs@dimname = dimension
  .trs@t0 = t0
  .trs@tResolution = tres
  .trs@tUnit = tunit
}

if (!isGeneric("tres")) {
  setGeneric("tres", function(x) {
    standardGeneric("tres")
  })
}
if (!isGeneric("tunit")) {
  setGeneric("tunit", function(x) {
    standardGeneric("tunit")
  })
}
if (!isGeneric("t0")) {
  setGeneric("t0", function(x) {
    standardGeneric("t0")
  })
}
if (!isGeneric("tdim")) {
  setGeneric("tdim", function(x) {
    standardGeneric("tdim")
  })
}

#' @export
setMethod("tres",signature(x="TRS"),function(x) {
  return(x@tResolution)
})
#' @export
setMethod("tres",signature(x="scidbst"),function(x) {
  return(tres(x@trs))
})
#' @export
setMethod("tunit",signature(x="TRS"),function(x) {
  return(x@tUnit)
})
#' @export
setMethod("tunit",signature(x="scidbst"),function(x) {
  return(tunit(x@trs))
})
#' @export
setMethod("t0",signature(x="TRS"),function(x) {
  return(x@t0)
})
#' @export
setMethod("t0",signature(x="scidbst"),function(x) {
  return(t0(x@trs))
})
#' @export
setMethod("tdim",signature(x="TRS"),function(x) {
  return(x@dimname)
})
#' @export
setMethod("tdim",signature(x="scidbst"),function(x) {
  if (x@isTemporal) {
    return(tdim(x@trs))
  } else {
    stop("scidbst object has no temporal reference.")
  }
})
