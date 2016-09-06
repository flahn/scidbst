#' @import methods
#' @import scidb
#' @import raster
#' @include TemporalExtent-class.R
#' @include scidbst-class-decl.R
#' @include TRS-class.R
#' @include SRS-class.R
NULL

# just a precaution, since the class was not exported in the package SciDBR (remved S3Methods for now)
setClass("scidb",
         representation(name="character",
                        meta="environment",
                        gc="environment")
)


#' Constructor for scidbst
#'
#' @name scidbst
#' @rdname scidbst-class
#' @param name a character string name of a stored SciDB array or a valid SciDB AFL expression
#' @param gc a logical value, TRUE means connect the SciDB array to R's garbage collector
#' @return scidbst object
#' @export
scidbst = function(...){
  .scidb = .scidbst_class(scidb(...))
  .scidb@title = .scidb@name
  .srs = iquery(paste("eo_getsrs(",.scidb@name,")",sep=""),return=TRUE)

  .scidb@sref = list()
  for (n in names(.srs)) {
    if (n %in% c("i")) {
      next
    } else {
      .scidb@sref[n] = .srs[1,n]
    }
  }

  .trs = iquery(paste("eo_gettrs(",.scidb@name,")",sep=""),return=TRUE)
  .scidb@tref = list()
  for (n in names(.trs)) {
    if (n %in% c("i")) {
      next
    } else {
      .scidb@tref[n] = .trs[1,n]
    }
  }

  .extent = iquery(paste("eo_extent(",.scidb@name,")",sep=""),return=TRUE)

  if (nrow(.extent) == 0) {
    stop("There is no spatial or temporal extent for this array.")
  }

  .scidb@isSpatial = (nrow(.srs) > 0)
  .scidb@isTemporal = (nrow(.trs) > 0)

  if (.scidb@isTemporal) { #make sure that there is actually a temporal reference
    temporal_dim = .trs[,"tdim"]
    tResolution = as.numeric(unlist(regmatches(.trs[,"dt"],gregexpr("(\\d)+",.trs[,"dt"]))))
    tUnit = .findTUnit(.trs[,"dt"])
    startTime = .getDateTime(.trs[,"t0"],tUnit)
    .scidb@trs = TRS(temporal_dim,startTime,tResolution,tUnit)

    tmin = .getDateTime(.extent[,"tmin"],tunit(.scidb))
    tmax = .getDateTime(.extent[,"tmax"],tunit(.scidb))
    .scidb@tExtent = textent(tmin,tmax)
  }


  if (.scidb@isSpatial) {
    .scidb@affine <- .createAffineTransformation(.srs)


    .scidb@srs = SRS(.srs$proj4text,dimnames=c(.srs[,"ydim"],.srs[,"xdim"]))



    .scidb@extent = extent(.extent[,"xmin"],.extent[,"xmax"],.extent[,"ymin"],.extent[,"ymax"])

    #get minimum and maximum extent for spatial dimensions in terms of dimension indices
    .scidb@nrows = as.integer(nrow(.scidb))
    .scidb@ncols = as.integer(ncol(.scidb))
  }

  return(.scidb)
}


#' #' @export
#' setMethod("show",signature(object="scidbst"), function(object){
#'   s = .toScidb(object)
#'   show(s)
#' })


