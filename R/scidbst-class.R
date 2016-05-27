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

  .scidb@nrows = as.integer(bbox["y","max"] - bbox["y","min"]+1)
  .scidb@ncols = as.integer(bbox["x","max"] - bbox["x","min"]+1)

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

#' @export
setGeneric("setSelection", function(x, dim, val) {
  standardGeneric("setSelection")
})

#' @export
setMethod("setSelection",
          signature(x='scidbst',dim='character',val='numeric'),
          function(x,dim,val) {
            x@selector = matrix(c(dim,val),nrow=2)
            return(x)
          }
)

setMethod("getValues", signature(x='scidbst', row='missing', nrows='missing'),
          function(x,taxis,tindex) {
            if (! missing(taxis) & ! missing(tindex)) {
              x@selector = matrix(c(taxis,tindex),nrow=2)
            }


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
  result <- matrix(nrow = (ncol(object)) * (nrow(object)), ncol = nlayers(object))



  print("Downloading data...")
  sel = paste(object@selector[1,1],"=",object@selector[2,1],sep="") #TODO replace with function that creates the string
  .data = subset(object,sel)[] #materialize scidb array represented by object


  for (b in 1:object@data@nlayers) {
    lname = object@data@names[b]
    if (!is.na(.data[,lname]) || sum(.data[,lname])>0) {
      #result[, b] <- getRasterData(con, offset = offs,region.dim = reg, band = b)
      #result[,b] <- sample(0:255,ncols*nrows,replace=T)
      #result[,b] <- .data[,lname] #NA values are skipped by SciDB, need to add values via column, row indices

      tmp = matrix(nrow=(nrow(object)),ncol=(ncol(object)))
      m = .data[order(.data[,"y"],.data[,"x"]),]
      m[,1:2]=m[,1:2]+1
      tmp2 = matrix(m[,lname],nrow=length(unique(m[,"y"])),ncol=length(unique(m[,"x"])),byrow=T)
      tmp[unique(m[,"y"]),unique(m[,"x"])] = tmp2
      #restructure the matrix to a one dimensional vector
      restruct = as.vector(t(tmp))


      result[,b] = restruct
      # for(i in 1:length(.data[,1])) { #for each row
      #   row = .data[i,"y"]
      #   col = .data[i,"x"]
      #   val = .data[i,lname]
      #
      #   index = (row)*(ncol(object)+1)+col
      #   result[index,b] = val
      # }
    }
  }

  return(result)
}

#copy pasted from raster
setMethod("spplot", signature(obj='scidbst'),
          function(obj, ..., maxpixels=50000, as.table=TRUE, zlim)  {

            obj <- sampleRegular(obj, maxpixels, asRaster=TRUE,useGDAL=TRUE)
            if (!missing(zlim)) {
              if (length(zlim) != 2) {
                warning('zlim should be a vector of two elements')
              }
              if (length(zlim) >= 2) {
                obj[obj < zlim[1] | obj > zlim[2]] <- NA
              }
            }

            obj <- as(obj, 'SpatialGridDataFrame')
            #obj@data <- obj@data[, ncol(obj@data):1]
            spplot(obj, ..., as.table=as.table)
          }
)

#copied from raster with small changes
setMethod('sampleRegular', signature(x='scidbst'),
          function( x, size, ext=NULL, cells=FALSE, xy=FALSE, asRaster=FALSE, sp=FALSE, useGDAL=FALSE, ...) {

            stopifnot(hasValues(x))

            size <- round(size)
            stopifnot(size > 0)
            nl <- nlayers(x)
            rotated <- rotated(x)

            if (is.null(ext)) {
              if (size >= ncell(x)) {
                if (asRaster) {
                  if (!rotated) {
                    return(x)
                  }
                } else {
                  if (cells) {
                    return(cbind(1:ncell(x), values(x)))
                  } else {
                    return(values(x))
                  }
                }
              }
              rcut <- raster(x)
              firstrow <- 1
              lastrow <- nrow(rcut)
              firstcol <- 1
              lastcol <- ncol(rcut)

            } else {

              rcut <- crop(raster(x), ext)
              ext <- extent(rcut)
              if (size >= ncell(rcut)) {
                x <- crop(x, ext)
                if (asRaster) {
                  return(x)
                } else {
                  return(getValues(x))
                }
              }
              yr <- yres(rcut)
              xr <- xres(rcut)
              firstrow <- rowFromY(x, ext@ymax-0.5 *yr)
              lastrow <- rowFromY(x, ext@ymin+0.5*yr)
              firstcol <- colFromX(x, ext@xmin+0.5*xr)
              lastcol <- colFromX(x, ext@xmax-0.5*xr)
            }


            Y <- X <- sqrt(ncell(rcut)/size)
            nr <- max(1, floor((lastrow - firstrow + 1) / Y))
            nc <- max(1, floor((lastcol - firstcol + 1) / X))

            rows <- (lastrow - firstrow + 1)/nr * 1:nr + firstrow - 1
            rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
            cols <- (lastcol - firstcol + 1)/nc * 1:nc  + firstcol - 1
            cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)

            cols <- unique(round(cols))
            rows <- unique(round(rows))
            cols <- cols[cols>0]
            rows <- rows[rows>0]
            nr <- length(rows)
            nc <- length(cols)


            if (fromDisk(x)) {

              if (cells | any(rotated | raster:::.driver(x, FALSE) != 'gdal')) {
                useGDAL <- FALSE
              }
              if (useGDAL) {
                offs <- c(firstrow,firstcol)-1
                reg <- c(nrow(rcut), ncol(rcut))-1

                if (inherits(x, 'RasterStack')) {

                  v <- matrix(NA, ncol=nl, nrow=prod(nr, nc))

                  for (i in 1:nl) {
                    xx <- x[[i]]
                    con <- GDAL.open(xx@file@name, silent=TRUE)
                    band <- bandnr(xx)
                    vv <- getRasterData(con, band=band, offset=offs, region.dim=reg, output.dim=c(nr, nc))
                    closeDataset(con)
                    if (xx@data@gain != 1 | xx@data@offset != 0) {
                      vv <- vv * xx@data@gain + xx@data@offset
                    }
                    if (xx@file@nodatavalue < 0) {
                      vv[vv <= xx@file@nodatavalue] <- NA
                    } else {
                      vv[vv == xx@file@nodatavalue] <- NA
                    }
                    v[, i] <- vv
                  }

                } else {
                  if (nl == 1) {
                    band <- bandnr(x)
                  } else {
                    band <- NULL
                  }
                  con <- GDAL.open(x@file@name, silent=TRUE)
                  v <- getRasterData(con, band=band, offset=offs, region.dim=reg, output.dim=c(nr, nc))
                  closeDataset(con)

                  if (x@data@gain != 1 | x@data@offset != 0) {
                    v <- v * x@data@gain + x@data@offset
                  }

                  if (raster:::.naChanged(x)) {
                    if (x@file@nodatavalue < 0) {
                      v[v <= x@file@nodatavalue] <- NA
                    } else {
                      v[v == x@file@nodatavalue] <- NA
                    }
                  }
                }

                if (asRaster) {
                  if (is.null(ext))  {
                    outras <- raster(x)
                  } else {
                    outras <- raster(ext)
                  }
                  nrow(outras) <- nr
                  ncol(outras) <- nc
                  if (nl > 1) {
                    outras <- brick(outras, nl=nl)
                    outras <- setValues(outras, v)
                  } else {
                    outras <- setValues(outras, as.vector(v))
                  }
                  names(outras) <- scidb_attributes(x)
                  if (any(is.factor(x))) {
                    levels(outras) <- levels(x)
                  }
                  return(outras)

                } else {
                  if (cells) {
                    warning("'cells=TRUE' is ignored when 'useGDAL=TRUE'")
                  }
                  if (xy) {
                    warning("'xy=TRUE' is ignored when 'useGDAL=TRUE'")
                  }
                  if (sp) {
                    warning("'sp=TRUE' is ignored when 'useGDAL=TRUE'")
                  }
                  return( as.vector(v) )
                }
              }
            }

            cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))


            if (asRaster) {
              if (rotated) {
                if (is.null(ext)) {
                  outras <- raster(extent(x))
                } else {
                  outras <- raster(ext)
                }
                ncol(outras) <- nc
                nrow(outras) <- nr
                xy <- xyFromCell(outras, 1:ncell(outras))
                m <- raster:::.xyValues(x, xy)

              } else {
                m <- raster:::.cellValues(x, cell)

                if (is.null(ext))  {
                  outras <- raster(x)
                } else {
                  outras <- raster(ext)
                }
                nrow(outras) <- nr
                ncol(outras) <- nc

              }
              if (nl > 1) {
                outras <- brick(outras, nl=nl)
              }
              outras <- setValues(outras, m)
              names(outras) <- scidb_attributes(x)
              if (any(is.factor(x))) {
                levels(outras) <- levels(x)
              }
              return(outras)

            } else {

              m <- NULL
              if (xy) {
                m <- xyFromCell(x, cell)
              }
              if (cells) {
                m <- cbind(m, cell=cell)
              }
              m <- cbind(m, raster:::.cellValues(x, cell))

              if (sp) {
                m <- SpatialPointsDataFrame(xyFromCell(x, cell), data.frame(m), proj4string=projection(x, asText=FALSE))
              }

              return(m)
            }
          }

)
