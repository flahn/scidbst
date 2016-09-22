#' @import scidb
#' @import raster
#' @include TemporalExtent-class.R
#' @include scidbst-class-decl.R
#' @include TRS-class.R
#' @include SRS-class.R
NULL

# just a precaution, since the class was not exported in the package SciDBR (remved S3Methods=TRUE for now)
setClass("scidb",
         representation(name="character",
                        meta="environment",
                        gc="environment")
)


#' Constructor for scidbst
#'
#' @name scidbst
#' @rdname scidbst-class
#' @param ... parameter that are passed on to \code{\link[scidb]{scidb}}
#'
#' @note At least parameter \code{name} should be provided
#' @return scidbst object
#' @export
scidbst = function(...){
  .scidbst = .scidbst_class()
  .scidb = scidb(...)
  .scidbst@proxy = .scidb
  .scidbst@title = .scidb@name

  .extent = iquery(paste("eo_extent(",.scidb@name,")",sep=""),return=TRUE)

  if (nrow(.extent) == 0) {
    stop("There is no spatial or temporal extent for this array.")
  }

  .srs = iquery(paste("eo_getsrs(",.scidb@name,")",sep=""),return=TRUE)
  .scidbst@isSpatial = (nrow(.srs) > 0)

  if (.scidbst@isSpatial) {
    .scidbst@affine <- .createAffineTransformation(.srs)
    .scidbst@srs = SRS(.srs$proj4text,dimnames=c(.srs[,"ydim"],.srs[,"xdim"]))
    .scidbst@srs@authority = .srs$auth_name
    .scidbst@srs@srid =.srs$auth_srid
    .scidbst@srs@srtext = .srs$srtext

    .scidbst@extent = extent(.extent[,"xmin"],.extent[,"xmax"],.extent[,"ymin"],.extent[,"ymax"])
  }


  .trs = iquery(paste("eo_gettrs(",.scidb@name,")",sep=""),return=TRUE)
  .scidbst@isTemporal = (nrow(.trs) > 0)

  if (.scidbst@isTemporal) {

    # TRS variables
    temporal_dim = .trs[,"tdim"]
    tResolution = as.numeric(unlist(regmatches(.trs[,"dt"],gregexpr("(\\d)+",.trs[,"dt"]))))
    tUnit = .findTUnit(.trs[,"dt"])
    startTime = .getDateTime(.trs[,"t0"],tUnit)

    # temporal extent variables
    tmin = .getDateTime(.extent[,"tmin"],tUnit)
    tmax = .getDateTime(.extent[,"tmax"],tUnit)

    .scidbst@trs = TRS(temporal_dim,startTime,tResolution,tUnit)
    .scidbst@tExtent = textent(tmin,tmax)
  }

  return(.scidbst)
}

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
  if (x@isTemporal) {
    return(x@trs)
  } else {
    stop("Objecthas no temporal reference")
  }
})

#' @export
setMethod("show",signature(object="scidbst"), function(object){
  s = as(object,"scidb")
  cat(paste("Title:\t\t",object@title,"\n",sep=""))
  if (object@isSpatial){
    cat(paste("Spatial Extent:\n"))
    cat(paste("\txmin:\t",xmin(object),"\n",sep=""))
    cat(paste("\txmax:\t",xmax(object),"\n",sep=""))
    cat(paste("\tymin:\t",ymin(object),"\n",sep=""))
    cat(paste("\tymax:\t",ymax(object),"\n",sep=""))
    cat("CRS:\n")
    cat(paste("\t",crs(object),"\n",sep=""))
  }
  if (object@isTemporal) {
    show(t.extent(object))
    show(trs(object))
  }
  show(s)
})

setGeneric("affine", function(x) standardGeneric("affine"))

#' Returns the affine transformation
#'
#' The function returns the stored affine transformation. The matrix has a dimensionality of 2x3 and contains the following values:
#' x0,xres(x),xshear(x) \\ y0, yshear(x), yres(x)
#'
#' @param x scidbst object
#' @return a numeric matrix containing the affine transformation parameter
#' @export
setMethod("affine",signature(x="scidbst"),function(x) {
  if (x@isSpatial || !is.null(x@affine)) {
    return(x@affine)
  } else {
    stop("The array is not spatial. There is no affine transformation.")
  }

})

