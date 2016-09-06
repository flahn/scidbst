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

  .srs = iquery(paste("eo_getsrs(",.scidb@name,")",sep=""),return=TRUE)

  .scidbst@sref = list()
  for (n in names(.srs)) {
    if (n %in% c("i")) {
      next
    } else {
      .scidbst@sref[n] = .srs[1,n]
    }
  }

  .trs = iquery(paste("eo_gettrs(",.scidb@name,")",sep=""),return=TRUE)
  .scidbst@tref = list()
  for (n in names(.trs)) {
    if (n %in% c("i")) {
      next
    } else {
      .scidbst@tref[n] = .trs[1,n]
    }
  }

  .extent = iquery(paste("eo_extent(",.scidb@name,")",sep=""),return=TRUE)

  if (nrow(.extent) == 0) {
    stop("There is no spatial or temporal extent for this array.")
  }

  .scidbst@isSpatial = (nrow(.srs) > 0)
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


  if (.scidbst@isSpatial) {
    .scidbst@affine <- .createAffineTransformation(.srs)


    .scidbst@srs = SRS(.srs$proj4text,dimnames=c(.srs[,"ydim"],.srs[,"xdim"]))



    .scidbst@extent = extent(.extent[,"xmin"],.extent[,"xmax"],.extent[,"ymin"],.extent[,"ymax"])

    #get minimum and maximum extent for spatial dimensions in terms of dimension indices
    .scidbst@nrows = as.integer(nrow(.scidbst))
    .scidbst@ncols = as.integer(ncol(.scidbst))
  }



  return(.scidbst)
}


#' #' @export
#' setMethod("show",signature(object="scidbst"), function(object){
#'   s = .toScidb(object)
#'   show(s)
#' })


