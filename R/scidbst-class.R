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

# function to calculate the dimension index by a given time string
.calcTDimIndex = function (x, time) {
  if (is.character(time)) {
    time = as.POSIXlt(.getDateTime(time,x@tUnit))
  } else {
    #is this called?
    time = as.POSIXlt(time) #assuming valid POSIX string
  }

  if (time >= x@tExtent$min && time <= x@tExtent$max) {
    t0 = x@startTime
    dt = x@tResolution
    unit = x@tUnit
    index  = floor(as.numeric(difftime(time,t0,unit))/dt)

    return(index)
  } else {
    stop("time statement is out of bounds")
  }
}

.findTUnit = function(res) {
  days = "P(\\d)+D"
  months = "P(\\d)+M"
  years = "P(\\d)+Y"
  weeks = "P(\\d)+W"
  hours = "P(\\d)+h"
  minutes = "P(\\d)+m"
  seconds = "P(\\d)+s"

  if (grepl(days,res)) {
    return("days")
  }
  if (grepl(months,res)) {
    return("months")
  }
  if (grepl(years,res)) {
    return("years")
  }
  if (grepl(weeks,res)) {
    return("weeks")
  }
  if (grepl(hours,res)) {
    return("hours")
  }
  if (grepl(minutes,res)) {
    return("mins")
  }
  if (grepl(seconds,res)) {
    return("secs")
  }
}


# transforms a string by a given temporal unit into a valid POSIX string
.getDateTime = function (str, unit) {
  if (unit == "days") {
    tmp = strptime(str, "%Y-%m-%d") #day in month of year
    if (is.na(tmp)) {
      tmp = strptime(str, "%Y-%j") #day of year
    }
    return(tmp)
  }
  stop("Cannot extract start time of the time series")
}

.isMatrixEmpty = function (m) {
  return(max(is.na(m)) == 1 )
}

.createAffineTransformation = function(srs) {
  .res_matrix = matrix(ncol=3,nrow=2)

  .vec = as.numeric(as.matrix(as.data.frame(strsplit(strsplit(srs$A,"\\s")[[1]],"="))[2,]))
  .res_matrix[,1] = .vec[1:2]
  .res_matrix[,2] = .vec[c(3,5)]
  .res_matrix[,3] = .vec[c(6,4)]

  return(.res_matrix)
}

.transformToWorld = function(trans,x,y) {
  return(trans %*% c(1,x,y))
}







#' names function
#'
#' Returns the names of the dimensions and attributes used in the remote scidb array.
#'
#' @note This function overwrites the standard S3 names function of raster.
#'
#' @param x scibst object
#' @return vector of character containing the names of dimensions and attributes
#'
setMethod("names",signature(x="scidbst"), function(x) {
  return(c(c(dimensions(x),scidb_attributes(x))))
})


setMethod("subset",signature(x="scidbst"), function(x, ...) scidb:::filter_scidb(x, ...))

.cpMetadata = function(from,to) {
  if (class(from) == "scidbst" && class(to) == "scidbst") {
    to@extent = from@extent
    crs(to) = crs(from)

    to@affine = from@affine

    to@data@names = scidb_attributes(to)
    to@data@nlayers = length(to@data@names)
    to@data@fromdisk = TRUE


    to@spatial_dims = from@spatial_dims
    to@temporal_dim = from@temporal_dim
    to@startTime = from@startTime
    to@tExtent = from@tExtent
    to@tResolution = from@tResolution
    to@tUnit = from@tUnit
    to@isSpatial = from@isSpatial
    to@isTemporal = from@isTemporal
    to@sref = from@sref
    to@tref = from@tref

    if (inMemory(from)) {
      to@data@inmemory = FALSE
    }

    return(to)
  }
}

setMethod("nrow",signature(x="scidbst"),function(x) {
  if (x@isSpatial) {
    #dim = x@spatial_dims$ydim
    extent = .calculateDimIndices(x,extent(x))
    return(extent@ymax-extent@ymin+1)
  } else if (x@isTemporal){
    return(1)
  } else {
    lengths = .getLengths(x)
    if (length(lengths) == 1) {
      return(1)
    } else {
      return(lengths[getYDim(x)])
    }
  }
})

setMethod("ncol",signature(x="scidbst"),function(x) {
  if (x@isSpatial) {
    extent = .calculateDimIndices(x,extent(x))
    return(extent@xmax-extent@xmin+1)
  } else if (x@isTemporal) {
    return(as.numeric(difftime(x@tExtent[["max"]],x@tExtent[["min"]],x@tUnit))+1)
  } else {
    lengths = .getLengths(x)
    if (length(lengths) == 1) {
      return(lengths[1])
    } else {
      return(lengths[getXDim(x)])
    }
  }
})

#' @export
setMethod("show",signature(object="scidbst"), function(object){
  s = .toScidb(object)
  show(s)
})

.calculateDimIndices = function(object, extent) {
  ll = c(xmin(extent),ymin(extent))
  ur = c(xmax(extent),ymax(extent))

  origin = object@affine[,1]
  sub = object@affine[,2:3]

  img1 = (solve(sub) %*% (ll - origin))
  img2 = (solve(sub) %*% (ur - origin))

  indices = extent(c(range(img1[1],img2[1]),range(img1[2],img2[2])))
  xmin(indices) = floor(xmin(indices))
  ymin(indices) = floor(ymin(indices))
  xmax(indices) = ceiling(xmax(indices))
  ymax(indices) = ceiling(ymax(indices))
  return(indices)
}

setGeneric("getXDim",function(x) standardGeneric("getXDim"))

#' @export
setMethod("getXDim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(x@spatial_dims$xdim)
  } else {
    return(dimensions(x)[2])
  }
})

setGeneric("getYDim",function(x) standardGeneric("getYDim"))
#' @export
setMethod("getYDim",signature(x="scidbst"),function(x){
  if (x@isSpatial) {
    return(x@spatial_dims$ydim)
  } else {
    return(dimensions(x)[1])
  }
})

setGeneric("getTDim",function(x) standardGeneric("getTDim"))
#' @export
setMethod("getTDim",signature(x="scidbst"),function(x){
  if (x@isTemporal) {
    return(x@temporal_dim)
  } else {
    return(dimensions(x)[1])
  }
})

.getLengths = function(obj) {
  dimnames = dimensions(obj)
  dimbounds = scidb_coordinate_bounds(obj)
  v = as.numeric(dimbounds$length)
  names(v) = dimnames

  return(v)
}

# function to create a scidb array from a scidbst array, this approach copies the environment content (object promises)
# rather than querying the scidb instance anew for the same information
.toScidb = function(x) {
  res = scidb("")
  res@name = x@name
  res@gc = x@gc
  res@meta = suppressWarnings(x@meta)
  return(res)
}

.getRefPeriod = function(x) {
  m = matrix(cbind(c("P(\\d)+D","P(\\d)+M","P(\\d)+Y","P(\\d)+W","P(\\d)+h","P(\\d)+m","P(\\d)+s"),
                   c("days","months","years","weeks","hours","mins","secs"),
                   c("D","M","Y","W","h","m","s")),ncol=3)
  colnames(m)=c("regexp","tunit","abbrev")

  out = paste("P",x@tResolution,m[m[,"tunit"]==x@tUnit,"abbrev"],sep="")
  return(out)
}
