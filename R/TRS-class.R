#' @include scidbst-class-decl.R
#' @include TRS-class-decl.R

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
  if (! is.numeric(tres)) {
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

  return(.trs)
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

#' Returns the temporal resolution
#'
#' This function returns the temporal resolution as a numeric value with the unit stated by 'tunit'.
#'
#' @name tres
#' @rdname tres-method
#' @param x TRS object
#' @return numeric value
#' @export
setMethod("tres",signature(x="TRS"),function(x) {
  return(x@tResolution)
})

#' @rdname tres-method
#' @param x scidbst object
#' @export
setMethod("tres",signature(x="scidbst"),function(x) {
  return(tres(x@trs))
})

#' Returns the temporal unit
#'
#' This function returns the temporal unit as a string (character)
#'
#' @name tunit
#' @rdname tunit-method
#' @param x TRS object
#' @return character value
#' @export
setMethod("tunit",signature(x="TRS"),function(x) {
  return(x@tUnit)
})

#' @rdname tunit-method
#' @param x scidbst object
#' @export
setMethod("tunit",signature(x="scidbst"),function(x) {
  return(tunit(x@trs))
})

#' Returns the temporal datum (t0)
#'
#' This function returns the reference t0 value. In combination with scidbst this refers to the 0 value in the temporal dimension.
#' @name t0
#' @rdname t0-method
#' @param x TRS object
#' @return POSIXt value
#' @export
setMethod("t0",signature(x="TRS"),function(x) {
  return(x@t0)
})

#' @rdname t0-method
#' @param x scidbst object
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
#' @param x TRS object
#' @return The temporal dimension name or the first dimension name
#' @export
setMethod("tdim",signature(x="TRS"),function(x) {
  return(x@dimname)
})

#' @rdname tdim-method
#' @param x TRS object
#' @export
setMethod("tdim",signature(x="scidbst"),function(x) {
  if (x@isTemporal) {
    return(tdim(x@trs))
  } else {
    warning("scidbst object has no temporal reference. Returning first dimension.")
    return(dimensions(x)[1])
  }
})


if (!isGeneric("getRefPeriod")) {
  setGeneric("getRefPeriod", function(x) {
    standardGeneric("getRefPeriod")
  })
}

#' Creates a character expression for the reference time period
#'
#' Returns the reference period of a scidbst object, e.g. P1D, P16D or P1M. It combines the temporal resolution with an abbreviation
#' for the time unit. Usually the expression is used as "dt" (delta t).
#'
#' @param x scidbst object
#' @return a character describing the reference period for a time unit
#' @export
getRefPeriod = function(x) {
  m = matrix(cbind(c("P(\\d)+D","P(\\d)+M","P(\\d)+Y","P(\\d)+W","P(\\d)+h","P(\\d)+m","P(\\d)+s"),
                   c("days","months","years","weeks","hours","mins","secs"),
                   c("D","M","Y","W","h","m","s")),ncol=3)
  colnames(m)=c("regexp","tunit","abbrev")

  out = paste("P",tres(x),m[m[,"tunit"]==tunit(x),"abbrev"],sep="")
  return(out)
}

#' @export
setMethod("show",signature(object="TRS"), function(object){
  out = paste("TRS:\n","\tdimension: \t\"",tdim(object),"\"\n","\tt0: \t\t",t0(object),"\n","\tdt: \t\t",getRefPeriod(object),"\n",sep="")
  cat(out)
})
