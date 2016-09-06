#' @include scidbst-class-decl.R
#' @include TRS-class-decl.R
NULL

#' TRS constructor
#'
#' This function creates a new temporal reference object
#' @rdname TRS-class
#' @param dimension character - the dimension name
#' @param t0 POSIXt - the datum
#' @param tres numeric - the temporal resolution
#' @param tunit character - The temporal unit
#' @export
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

#' Get temporal dimension name
#'
#' Returns the name of the temporal dimension of a scidbst array. If the object is not temporal it will
#' return the first dimension name.
#' @name tdim
#' @rdname tdim-method
#' @param x scidbst object
#' @return The temporal dimension name or the first dimension name
#' @export
#' @export
setMethod("tdim",signature(x="TRS"),function(x) {
  return(x@dimname)
})

#' @rdname tdim-method
#' @export
setMethod("tdim",signature(x="scidbst"),function(x) {
  if (x@isTemporal) {
    return(tdim(x@trs))
  } else {
    warning("scidbst object has no temporal reference. Returning first dimension.")
    return(dimensions(x)[1])
  }
})

