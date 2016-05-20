library(scidb)
library(raster)


#' Class scidbst
#'
#' Class \code{scidbst} inherits from class \code{scidb}
#'
#' @name scidbst-class
#' @rdname scidbst-class
#' @slot CRS The coordinate reference system used as class 'CRS' that represents a Proj.4 string
#' @slot extent The outer boundary of the SciDB array in referenced coordinates
#' @slot affine The affine transformation used to convert real-world coordinates into image frame coordinates
#' @aliases scidbst
#' @import methods
#' @import scidb
#' @import raster
#' @exportClass scidbst
.scidbst_class = setClass("scidbst",
         contains=list("scidb","RasterBrick"),
         representation=representation(
           affine = "matrix",
           selector = "matrix"
         )
)
.scidbst_class

#' Constructor for scidbst
#'
#' @name stdb
#' @rdname scidbst-class
#' @param ... stuff passed to scidb()
#' @return \link{scidbst} object
#' @import scidb
#' @export
stdb = function(...){

  .scidb = .scidbst_class(scidb(...))

  .scidb@selector = matrix(NA,nrow=2,ncol=1)

  .srs = iquery(paste("eo_getsrs(",.scidb@name,")",sep=""),return=TRUE)
  .scidb@affine <- .createAffineTransformation(.srs)
  .scidb@crs <- CRS(.srs$proj4text)


  .schema = schema(.scidb)
  .attr = strsplit(gsub("[<|>]","",strsplit(.schema," ")[[1]][1]),",")[[1]]

  .scidb@data@names = matrix(unlist(sapply(.attr,strsplit,split=":")),length(.attr), 2 ,byrow=TRUE)[,1]
  .scidb@data@nlayers = length(.attr)
  .scidb@data@fromdisk = TRUE

  .dims = matrix(strsplit(gsub("[\\[|\\]]","",strsplit(.schema," ")[[1]][2],perl=T),",")[[1]],nrow=3)
  .dims = strsplit(.dims[1,],"[=|:]")
  mins = as.numeric(c(.dims[[1]][2],.dims[[2]][2]))
  maxs = as.numeric(c(.dims[[1]][3],.dims[[2]][3]))
  bbox = cbind(mins,maxs)
  colnames(bbox)=c("min","max")
  rownames(bbox)=c(.dims[[1]][1],.dims[[2]][1])

  #x and y refer to image coordinates -> flip ymin, ymax
  min = .transformToWorld(.scidb@affine,bbox["x","min"],bbox["y","max"])
  max = .transformToWorld(.scidb@affine,bbox["x","max"],bbox["y","min"])

  .scidb@nrows = as.integer(bbox["y","max"] - bbox["y","min"])
  .scidb@ncols = as.integer(bbox["x","max"] - bbox["x","min"])

  .scidb@extent <- extent(min[1], max[1], min[2], max[2])
  return(.scidb)

  # .scidb = scidb(...)
  # .srs = iquery(paste("eo_getsrs(",.scidb@name,")",sep=""),return=TRUE)
  # .aff <- .createAffineTransformation(.srs)
  # .crs <- CRS(.srs$proj4text)
  # .extent <- extent(-180, 0, 0, 90)
  # .st = new("scidbst", CRS=.crs,affine=.aff,extent=.extent)

  # sn = slotNames("scidb")
  # for (i in 1:length(sn)) {
  #   slot(.st, sn[i]) <- slot(.scidb, sn[i])
  # }
  # return(.st)
}

.isMatrixEmpty = function (m) {
  return(max(is.na(m[,1])) == 0 )
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


setMethod("getValues", signature(x='scidbst', row='missing', nrows='missing'),
          function(x,taxis="t",tindex=0) {
            x@selector = matrix(c(taxis,tindex),nrow=2)

            if (! inMemory(x) ) {
              if ( fromDisk(x) ) {
                x <- readAll(x)
              } else {
                return( matrix(rep(NA, ncell(x) * nlayers(x)), ncol=nlayers(x)) )
              }
            }
            colnames(x@data@values) <- x@data@names
            x@data@values
          }
)

#' readAll
#'
#' In combination with \code{raster::getValues()} this function is called to retrieve the values from the source and store them in memory
#'
#' @export
setMethod('readAll', signature(object='scidbst'),
          function(object){
            #print("picked correct function")
            if (! object@data@fromdisk)  {
              stop('cannot read values; there is no file associated with this scidbst raster inheriting thing')
            }
            object@data@inmemory <- TRUE
            object@data@values <- .materializeSCIDBValues(object, 1, object@nrows)
            w <- getOption('warn')
            on.exit(options('warn' = w))
            options('warn'=-1)
            rge <- apply(object@data@values, 2, FUN=function(x){ range(x, na.rm=TRUE) } )
            object@data@min <- as.vector(rge[1,])
            object@data@max <- as.vector(rge[2,])
            object@data@haveminmax <- TRUE
            return(object)
          }
)

setMethod("names",signature(x="scidbst"), function(x) {
  return(c(c(dimensions(x),scidb_attributes(x))))
})


setMethod("subset",signature(x="scidbst"), function(x, ...) scidb:::filter_scidb(x, ...))


.materializeSCIDBValues = function(object, startrow, nrows=1, startcol=1, ncols=ncol(object)) {
  offs <- c((startrow - 1), (startcol - 1))
  reg <- c(nrows, ncols)
  #con <- GDAL.open(object@file@name, silent = TRUE)

  #		result <- getRasterData(con, offset=offs, region.dim=reg)
  #		result <- do.call(cbind, lapply(1:nlayers(object), function(i) as.vector(result[,,i])))
  # just as fast, it seems:
  result <- matrix(nrow = (ncol(object)+1) * (nrow(object)+1), ncol = nlayers(object))
  print("Downloading data...")
  #paste(object@selector[1,1],"==",object@selector[2,1],sep="")
  .data = subset(object,t==1)[] #materialize scidb array represented by object

  for (b in 1:object@data@nlayers) {
    lname = object@data@names[b]
    #result[, b] <- getRasterData(con, offset = offs,region.dim = reg, band = b)
    #result[,b] <- sample(0:255,ncols*nrows,replace=T)
    #result[,b] <- .data[,lname] #NA values are skipped by SciDB, need to add values via column, row indices
    for(i in 1:length(.data[,1])) {
      row = .data[i,"y"]
      col = .data[i,"x"]
      val = .data[i,lname]

      index = (row)*(ncol(object)+1)+col
      result[index,b] = val
    }
  }


  return(result)
}
