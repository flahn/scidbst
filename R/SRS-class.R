#' @include SRS-class-decl.R
#' @include scidbst-class-decl.R
#' @include nrow.R
#' @include ncol.R
NULL

#' SRS class constructor
#'
#' Creates a new SRS object with projection arguments and dimension names (y,x)-order
#'
#' @param projargs character - The projection arguments as defined by PROJ4
#' @param dimnames character - The dimension names
#' @rdname SRS-class
#' @export
SRS = function(projargs, dimnames) {
  .srs = .srs_class()
  .srs@projargs = projargs
  .srs@dimnames = dimnames
  return(.srs)
}

########################
# setSRS
########################
.setSRS = function (x, srs, affine, return=FALSE) {
  #eo_setsrs:  {name,xdim,ydim,authname,authsrid,affine_str}
  # if (!inherits(x,"scidb")) stop("Parameter x is no scidb array")
  cmd = paste("eo_setsrs(",x@name,",'",xdim(srs),"','",ydim(srs),"','",srs@authority,"',",srs@srid,",'","x0=",affine[1,1]," y0=",affine[2,1]," a11=",affine[1,2]," a22=",affine[2,3]," a12=",affine[1,3]," a21=",affine[2,2],"'",")",sep="")
  iquery(cmd)

  if (return) {
    return(scidbst(x@name))
  }
}

if (!isGeneric("setSRS")) {
  setGeneric("setSRS", function(x, srs, affine, ...) {
    standardGeneric("setSRS")
  })
}

#' Sets the spatial reference system at a given array
#'
#' By knowing the \code{\link{SRS}} and the \code{\link{affine}} projection a spatial reference can be established on a given
#' scidb array.
#'
#' @name setSRS,scidb
#' @aliases setSRS setSRS,scidbst
#' @param x a \link{scidb} array
#' @param srs a \link{SRS} class
#' @param affine a 2x3 matrix containing the affine projection
#' @param return logical - if a scidbst object shall be returned (default FALSE)
#' @return a scidbst object if return=TRUE
#'
#' @export
setMethod("setSRS", signature(x="scidb",srs="SRS",affine="matrix"), .setSRS)

###################
# srs
###################
.getSRS = function(x) {
  if (x@isSpatial || !is.null(x@srs)) {
    return(x@srs)
  } else {
    stop("The scidbst array has no spatial reference to return.")
  }
}

if (!isGeneric("srs")) {
  setGeneric("srs", function(x) {
    standardGeneric("srs")
  })
}

#' Returns the spatial reference
#'
#' The function returns the SRS object of a scidbst object.
#'
#' @param x scidbst
#' @return \code{\link{SRS}} class
#'
#' @export
setMethod("srs",signature(x="scidbst"), .getSRS)

#################
# copySRS
#################

if (!isGeneric("copySRS")) {
  setGeneric("copySRS", function(x,y) {
    standardGeneric("copySRS")
  })
}

.cpsrs = function(x,y) {
  if (!y@isSpatial) {
    stop("Cannot copy the spatial reference of 'y', because y is not spatial.")
  }
  xname = if (class(x)=="scidbst") x@proxy@name else x@name
  yname = y@title

  xValidName = length(grep(c("[,\\(\\)]"),xname)) == 0
  if (!xValidName) {
    stop("Function statements in name of object 'x' detected. Please execute the calculation and store the results before running this method again.")
  }

  cmd = sprintf("eo_setsrs(%s,%s)",xname,yname)
  iquery(cmd)

  out = scidbst(xname)
  return(out)
}

#' Copy spatial reference
#'
#' Copies the spatial reference systems from scidbst object y to scidb(st) object x. This applies for the R object
#' as well as the array in SciDB. All necessary information will be copied from the source to the target array
#' in R and afterwards an iquery command is executed to persist the changes in SciDB.
#'
#' @rdname copySRS-methods
#' @param x scidbst or scidb object
#' @param y scidbst object
#'
#' @return modified x
#' @examples
#' \dontrun{
#'  target = scidb("no_srs_array")
#'  source = scidbst("srs_array")
#'  target = copySRS(target,source)
#' }
#' @export
setMethod("copySRS",signature(x="scidbst",y="scidbst"), .cpsrs)

#' @rdname copySRS-methods
#' @export
setMethod("copySRS",signature(x="scidb",y="scidbst"), .cpsrs)

##############
# is.spatial
##############
if (!isGeneric("is.spatial")) {
  setGeneric("is.spatial", function(x) {
    standardGeneric("is.spatial")
  })
}

#' Check for a scidbst object having a spatial reference
#'
#' The function checks for certain parameter considered to describe the spatial reference.
#'
#' @param x scidbst object
#' @return logical whether or not the object has a spatial reference
#'
#' @export
setMethod("is.spatial",signature(x="scidbst"), function(x) {
  return(x@isSpatial && !is.null(x@srs) && !is.null(x@extent))
})
