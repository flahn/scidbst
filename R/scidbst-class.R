#' @import methods
#' @import scidb
#' @import raster
NULL

# just a precaution, since the class was not exported in the package SciDBR (remved S3Methods for now)
setClass("scidb",
         representation(name="character",
                        meta="environment",
                        gc="environment")
         )

#' Class scidbst
#'
#' Class \code{scidbst} inherits from class \code{scidb}
#'
#' @name scidbst-class
#' @rdname scidbst-class
#' @slot CRS The coordinate reference system used as class 'CRS' that represents a Proj.4 string
#' @slot extent The outer boundary of the SciDB array in referenced coordinates
#' @slot affine The affine transformation used to convert real-world coordinates into image frame coordinates
#' @slot spatial_dims the names of the spatial dimensions as a named list. 'xdim' describes the west-east axis and 'ydim' the north-south axis.
#' @slot temporal_dim the name of the temporal dimension
#' @slot startTime the start time as a POSIXlt object
#' @slot tResolution The temporal resolution as a numeric
#' @slot tExtent the temporal extent (min/max) as a list
#' @slot tUnit the temporal base unit for this timeseries
#' @slot isSpatial A flag whether or not this object has a spatial reference
#' @slot isTemporal A flag whether or not this object has a temporal reference
#' @slot sref A named list of elements that represent the spatial reference as specified in scidb by eo_getsrs
#' @slot tref A named list with the elements retrieved by eo_gettrs function
#' @aliases scidbst
#' @exportClass scidbst
.scidbst_class = setClass("scidbst",
                          contains=list("scidb","RasterBrick"),
                          representation=representation(
                            affine = "matrix",
                            sref = "list",
                            tref = "list",
                            spatial_dims = "list",
                            temporal_dim = "character",
                            startTime = "ANY",
                            tExtent = "list",
                            tResolution = "numeric",
                            tUnit = "character",
                            isSpatial ="logical",
                            isTemporal = "logical"
                          )
)


#' Constructor for scidbst
#'
#' @name scidbst
#' @rdname scidbst-class
#' @param name a character string name of a stored SciDB array or a valid SciDB AFL expression
#' @param gc a logical value, TRUE means connect the SciDB array to R's garbage collector
#' @return scidbst object
#' @import scidb
#' @export
scidbst = function(...){

  .scidb = .scidbst_class(scidb(...))

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
    .scidb@temporal_dim = .trs[,"tdim"]
    .scidb@tResolution = as.numeric(unlist(regmatches(.trs[,"dt"],gregexpr("(\\d)+",.trs[,"dt"]))))
    .scidb@tUnit = .findTUnit(.trs[,"dt"])
    .scidb@startTime = .getDateTime(.trs[,"t0"],.scidb@tUnit)
    .scidb@tExtent = list(min=.getDateTime(.extent[,"tmin"],.scidb@tUnit),max=.getDateTime(.extent[,"tmax"],.scidb@tUnit))
  }


  if (.scidb@isSpatial) {
    .scidb@affine <- .createAffineTransformation(.srs)
    .scidb@crs <- CRS(.srs$proj4text)
    .scidb@spatial_dims = list(xdim=.srs[,"xdim"],ydim=.srs[,"ydim"])
    .scidb@extent = extent(.extent[,"xmin"],.extent[,"xmax"],.extent[,"ymin"],.extent[,"ymax"])
  }

  .attr = scidb_attributes(.scidb)
  .scidb@data@names = .attr
  .scidb@data@nlayers = length(.attr)
  .scidb@data@fromdisk = TRUE

  #get minimum and maximum extent for spatial dimensions
  .schema = schema(.scidb)
  .dims = matrix(strsplit(gsub("[\\[|\\]]","",strsplit(.schema," ")[[1]][2],perl=T),",")[[1]],nrow=3)
  .dims = strsplit(.dims[1,],"[=|:]")
  mins = as.numeric(c(.dims[[1]][2],.dims[[2]][2]))
  maxs = as.numeric(c(.dims[[1]][3],.dims[[2]][3]))
  bbox = cbind(mins,maxs)
  colnames(bbox)=c("min","max")
  rownames(bbox)=c(.dims[[1]][1],.dims[[2]][1])

  .scidb@nrows = as.integer(bbox["y","max"] - bbox["y","min"]+1)
  .scidb@ncols = as.integer(bbox["x","max"] - bbox["x","min"]+1)


  return(.scidb)
}

setMethod("subset",signature(x="scidbst"), function(x, ...) scidb:::filter_scidb(x, ...))


#' @export
setMethod("show",signature(object="scidbst"), function(object){
  s = .toScidb(object)
  show(s)
})


