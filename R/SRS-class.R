#' @include SRS-class-decl.R
#' @include scidbst-class-decl.R
#' @include ncol.R
#' @include nrow.R
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

#' Returns the Coordinate Reference System
#'
#' Returns the coordinate reference system of a scidbst object
#'
#' @param x scidbst object
#' @return \link[rgdal]{CRS}
#'
#' @export
setMethod("crs",signature(x="scidbst"),function(x, ...) {
  return(CRS(x@srs@projargs))
})

if (!isGeneric("xdim")) {
  setGeneric("xdim",function(x) standardGeneric("xdim"))
}

#' Get spatial x dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a West-East dimension. If the object is not
#' spatially referenced it will return the second dimension name.
#'
#' @name xdim
#' @rdname xdim-method
#' @param x scidbst object
#' @return The name of the spatia West-East dimension or the first dimension name
#' @export
setMethod("xdim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(xdim(x@srs))
  } else {
    warning("No explicit spatial reference found. Using second dimension instead.")
    return(dimensions(x)[2])
  }
})

#' @rdname xdim-method
#' @export
setMethod("xdim",signature(x="SRS"),function(x){
  return(x@dimnames[2])
})

if (!isGeneric("ydim")){
  setGeneric("ydim",function(x) standardGeneric("ydim"))
}

#' Get spatial y dimension name
#'
#' Returns the name of the spatial dimension of a scidbst array, that relates to a North-South dimension. If the object is not
#' spatially referenced it will return the first dimension name.
#'
#' @name ydim
#' @rdname ydim-method
#' @param x scidbst object
#' @return The name of the spatia North-South dimension or the first dimension name
#' @export
setMethod("ydim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(ydim(x@srs))
  } else {
    warning("No explicit spatial reference found. Using first dimension instead.")
    return(dimensions(x)[1])
  }
})

#' @rdname ydim-method
#' @export
setMethod("ydim",signature(x="SRS"),function(x){
  return(x@dimnames[1])
})

#' @export
setMethod("xmin",signature(x="scidbst"),function(x) {
  return(xmin(x@extent))
})

#' @export
setMethod("ymin",signature(x="scidbst"),function(x) {
  return(ymin(x@extent))
})

#' @export
setMethod("xmax",signature(x="scidbst"),function(x) {
  return(xmax(x@extent))
})

#' @export
setMethod("ymax",signature(x="scidbst"),function(x) {
  return(ymax(x@extent))
})

#' @rdname resolution
#' @export
setMethod("xres", signature(x="scidbst"), function(x) {
  e = extent(x)
  dx = xmax(e)-xmin(e)
  ncol = .ncol(x)
  return(dx/ncol)
})

#' @rdname resolution
#' @export
setMethod("yres", signature(x="scidbst"), function(x) {
  e = extent(x)
  dy = ymax(e)-ymin(e)
  nrow = .nrow(x)
  return(dy/nrow)
})

#' Spatial resolution
#'
#' Returns informaiton about the spatial resolution as a numeric values, e.g.(xres,yres), xres or yres.
#'
#' @rdname resolution
#' @param x scidbst
#' @return numeric or numeric vector
#'
#' @export
setMethod("res", signature(x="scidbst"), function(x) {
  if (x@isSpatial) {
    return(c(xres(x),yres(x)))
  }
})

########################
# setSRS
########################

if (!isGeneric("setSRS")) {
  setGeneric("setSRS", function(x, srs, affine, ...) {
    standardGeneric("setSRS")
  })
}

#' Sets the spatial reference system to a given array
#'
#' By stating the srs and the affine projection together with a given scidb array, that array
#' becomes a spatial array.
#'
#' @param x a scidb array
#' @param srs a SRS class
#' @param affine a 2x3 matrix containing the affine projection
#' @param return logical - if a scidbst object shall be returned (default FALSE)
#' @return a scidbst object if return=TRUE
#'
#' @export
setMethod("setSRS", signature(x="ANY",srs="SRS",affine="matrix"), function (x, srs, affine, return=FALSE) {
  #eo_setsrs:  {name,xdim,ydim,authname,authsrid,affine_str}
  # if (!inherits(x,"scidb")) stop("Parameter x is no scidb array")
  cmd = paste("eo_setsrs(",x@name,",'",xdim(srs),"','",ydim(srs),"','",srs@authority,"',",srs@srid,",'","x0=",affine[1,1]," y0=",affine[2,1]," a11=",affine[1,2]," a22=",affine[2,3]," a12=",affine[1,3]," a21=",affine[2,2],"'",")",sep="")
  iquery(cmd)

  if (return) {
    return(scidbst(x@name))
  }
})

###################
# srs
###################
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
setMethod("srs",signature(x="scidbst"), function(x) {
  if (x@isSpatial || !is.null(x@srs)) {
    return(x@srs)
  } else {
    stop("The scidbst array has no spatial reference to return.")
  }
})

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
#' Copies the spatial reference systems from scidbst object y to scidb(st) object x.
#'
#' @rdname copySRS-methods
#' @param x scidbst or scidb object
#' @param y scidbst object
#'
#' @return modified x
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
