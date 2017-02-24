#' @include TemporalExtent-class.R
#' @include scidbst-class-decl.R
#' @include TRS-class.R
#' @include SRS-class.R
NULL



#' Constructor for scidbst
#'
#' @name scidbst
#' @rdname scidbst-class
#' @param ... parameter that are passed on to \code{\link[scidb]{scidb}}
#'
#' @note At least parameter \code{name} should be provided
#' @return scidbst object
#'
#' @examples
#' \dontrun{
#' scidbconnect(host,port,user,pwd,protocol,authtype)
#' chicago = scidbst(name="chicago_sts")
#' }
#' @export
scidbst = function(...){
  args = list(...)
  if ("name" %in% names(args) || length(args) == 1) {
    .scidbst = new("scidbst")
    .scidb = scidb(...)
    .scidbst@proxy = .scidb
    .scidbst@title = .scidb@name


    .extent = iquery(paste("eo_extent(",.scidb@name,")",sep=""),return=TRUE)

    if (nrow(.extent) == 0) {
      warning("There is no spatial or temporal extent for this array. Creating a scidbst object without spatial and temporal reference instead.",immediate. = TRUE)
    } else {
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
    }
    return(.scidbst)
  } else {
    return(new("scidbst"))
  }
}

#' Show scidbst object
#'
#' Creates a printable representation about relevant information about the scidbst object.
#'
#' @param object scidbst object
#'
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
    show(textent(object))
    show(trs(object))
  }
  if(!is.null(s)) {
    show(s)
  }
})

