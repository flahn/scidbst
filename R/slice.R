#' @include scidbst-class.R
NULL

if (!isGeneric("slice")) {
  setGeneric("slice", function(x,d,n) standardGeneric("slice"))
}

.slice = function (x,d,n) {
  .scidb.obj = as(x,"scidb")

  .scidb.obj = scidb::slice(.scidb.obj,d,n)

  x@proxy = .scidb.obj
  if (d %in% tdim(x)) {
    baseTime = 0

    if (tunit(x) == "weeks") {
      baseTime = 7*24*60*60
    } else if (tunit(x) == "days") {
      baseTime = 24*60*60
    } else if (tunit(x) == "hours") {
      baseTime = 60 * 60
    } else if (tunit(x) == "mins") {
      baseTime = 60
    } else if (tunit(x) == "secs") {
      baseTime = 1
    } else {
      stop("currently no other temporal unit supported")
    }

    #adapt temporal extent
    newStart = as.POSIXlt(as.character(t0(x) + n * tres(x) * baseTime))
    newEnd = as.POSIXlt(as.character(t0(x) + (n+1) * tres(x) * baseTime))
    x@tExtent@min = newStart
    x@tExtent@max = newEnd
    x@isTemporal = FALSE
    # it remains temporal in R, but not in scidb => keep information, but set temporal to false
  }

  if (d %in% x@srs@dimnames) {
    x@isSpatial = FALSE
  }

  return(x)
}

# x: scibdst
# d: name of dimension
# n: coordinate value as character in a formatted date/time
.slice.character = function(x,d,n) {
  if (d %in% tdim(x)) {
    if (is.character(n) ) {
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
}

#' Slice a scidbst object at a particular dimension value
#'
#' Takes a dimension name and a value to create a slice of an array. This usually means reducing the dimensions
#' of an array by one. The dimension name and value choosen fixed and the other dimensions and attributes are returned.
#'
#' @name slice,scidbst
#' @rdname slice-scidbst-methods
#' @param x scidbst array object
#' @param d name of a dimension
#' @param n coordinate value to slice on, e.g. a POSIXt, POSIXt conform date/time string, temporal index
#' @return scidbst array with a reduced number of dimensions.
#'
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' scidbst.obj = scidbst(array_name) #scidb array with spatial and temporal dimension
#'
#' #examples on the temporal dimension
#' slice1 = slice(scidbst.obj,"t","0") # @ temporal index 0
#' slice2 = slice(scidbst.obj,"t",1) # @ temporal index 1
#' slice3 = slice(scidbst.obj,"t","2016-05-05") # @ the temporal index for a date
#' # other existing dimensions are also applicable
#' }
#' @seealso \link[scidb]{slice}
#' @export
setMethod('slice', signature(x="scidbst",d="character",n="character") , .slice.character)

#' @rdname slice-scidbst-methods
#' @export
setMethod('slice', signature(x="scidbst",d="character",n="numeric") , .slice)

#' @rdname slice-scidbst-methods
#' @export
setMethod('slice', signature(x="scidbst",d="character",n="integer") , .slice)

# x: scibdst
# d: name of dimension
# n: POSIXt
.slice.POSIXt = function(x,d,n) {
  if (is.temporal(x)) {
    if (missing(d)) {
      d = tdim(x)
    } else {
      if (! d %in% tdim(x)) {
        stop("Cannot calculate an index for an unreferenced temporal dimension.")
      }
    }
  }
  n = .calcTDimIndex(x,n)

  return(.slice(x,d,n))
}

#' @rdname slice-scidbst-methods
#' @export
setMethod('slice', signature(x="scidbst",d="missing",n="POSIXt") , .slice.POSIXt)

#' @rdname slice-scidbst-methods
#' @export
setMethod('slice', signature(x="scidbst",d="character",n="POSIXt"), .slice.POSIXt)
