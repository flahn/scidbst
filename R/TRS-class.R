#' @include scidbst-class-decl.R
#' @include TRS-class-decl.R

#' TRS constructor
#'
#' This function creates a new temporal reference object
#'
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

if (!isGeneric("trs")) {
  setGeneric("trs", function(x) {
    standardGeneric("trs")
  })
}

#' Returns the Temporal reference object
#'
#' @param x scidbst object
#' @return \code{\link{TRS} object}
#' @export
setMethod("trs",signature(x="scidbst"), function(x){
  if (x@isTemporal || !is.null(x@trs)) {
    return(x@trs)
  } else {
    stop("Object has no temporal reference")
  }
})

#######################
# getRefPeriod
#######################

if (!isGeneric("getRefPeriod")) {
  setGeneric("getRefPeriod", function(x) {
    standardGeneric("getRefPeriod")
  })
}

.getRefPeriod = function(x) {
  m = matrix(cbind(c("P(\\d)+D","P(\\d)+M","P(\\d)+Y","P(\\d)+W","P(\\d)+h","P(\\d)+m","P(\\d)+s"),
                   c("days","months","years","weeks","hours","mins","secs"),
                   c("D","M","Y","W","h","m","s")),ncol=3)
  colnames(m)=c("regexp","tunit","abbrev")

  out = paste("P",tres(x),m[m[,"tunit"]==tunit(x),"abbrev"],sep="")
  return(out)
}

#' Creates a character expression for the reference time period
#'
#' Returns the reference period of a scidbst object, e.g. P1D, P16D or P1M. It combines the temporal resolution with an abbreviation
#' for the time unit. Usually the expression is used as "dt" (delta t).
#'
#' @name getRefPeriod
#' @rdname getRefPeriod-method
#' @param x scidbst object
#' @return a character describing the reference period for a time unit
#' @export
setMethod("getRefPeriod",signature(x="scidbst"), .getRefPeriod )

#' @rdname getRefPeriod-method
#' @export
setMethod("getRefPeriod",signature(x="TRS"), .getRefPeriod)

#' @export
setMethod("show",signature(object="TRS"), function(object){
  out = paste("TRS:\n","\tdimension: \t\"",tdim(object),"\"\n","\tt0: \t\t",t0(object),"\n","\tdt: \t\t",getRefPeriod(object),"\n",sep="")
  cat(out)
})

##########################
# setTRS
##########################

if (!isGeneric("setTRS")) {
  setGeneric("setTRS", function(x,trs, ...) {
    standardGeneric("setTRS")
  })
}

.setTRS = function(x,trs, return=FALSE) {
  cmd = paste("eo_settrs(",x@name,",'",tdim(trs),"','",as.character(t0(trs)),"','",getRefPeriod(trs),"'",")",sep="")
  iquery(cmd)
  if (return) {
    return(scidbst(x@name))
  }
}

#' Add a TRS to a scidb array
#'
#' The function adds a temporal reference to a scidb array and returns a scidbst object
#'
#' @param x scidb
#' @param trs TRS
#' @param return logical - if a scidbst object shall be returned (default FALSE)
#' @return scidbst if parameter return=TRUE
#'
#' @note Using \code{setTRS} will also persist the metadata for the array directly in SciDB.
#'
#' @export
setMethod("setTRS", signature(x="scidb",trs="TRS"), .setTRS)


######################
# copyTRS
######################

if (!isGeneric("copyTRS")) {
  setGeneric("copyTRS", function(x,y) {
    standardGeneric("copyTRS")
  })
}
.cptrs = function(x,y) {
  if (!y@isTemporal) {
    stop("Cannot copy the temporal reference of 'y', because y is not temporal.")
  }
  xname = if (class(x)=="scidbst") x@proxy@name else x@name
  yname = y@title

  xValidName = length(grep(c("[,\\(\\)]"),xname)) == 0
  if (!xValidName) {
    stop("Function statements in name of object 'x' detected. Please execute the calculation and store the results before running this method again.")
  }

  cmd = sprintf("eo_settrs(%s,%s)",xname,yname)
  iquery(cmd)

  out = scidbst(xname)
  return(out)
}

#' Copy temporal reference
#'
#' Copies the temporal reference systems from scidbst object y to scidbst object x.
#' @rdname copyTRS-methods
#' @param x scidbst or scidb object
#' @param y scidbst object
#'
#' @return modified x
#'
#' @examples
#' \dontrun{
#'  st.arr1 = scidbst("st_arr_1")
#'  st.arr2  = scidbst("st_arr_2")
#'  simple.arr = scidb("some_array")
#'
#'  # overwrite trs of array 1 with trs of array 2
#'  st.arr1 = copyTRS(st.arr1, st.arr2)
#'
#'  # set the TRS of array 2 for the simple array (having the same named temporal dimension)
#'  t.arr1 = copyTRS(simple.arr, st.arr2)
#' }
#' @export
setMethod("copyTRS",signature(x="scidbst",y="scidbst"), .cptrs)

#' @rdname copyTRS-methods
#' @export
setMethod("copyTRS",signature(x="scidb",y="scidbst"), .cptrs)


##############
# is.temporal
##############
if (!isGeneric("is.temporal")) {
  setGeneric("is.temporal", function(x) {
    standardGeneric("is.temporal")
  })
}

.is.temporal = function(x) {
  return(x@isTemporal && !is.null(x@trs) && !is.null(x@tExtent))
}

#' Check for a scidbst object having a temporal reference
#'
#' The function checks for certain parameter considered to describe the temporal reference.
#'
#' @param x scidbst object
#' @return logical whether or not the object has a temporal reference
#'
#' @export
setMethod("is.temporal",signature(x="scidbst"), .is.temporal)
