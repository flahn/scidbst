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
#' @aliases scidbst
#' @exportClass scidbst
.scidbst_class = setClass("scidbst",
                          contains=list("scidb","RasterBrick"),
                          representation=representation(
                            affine = "matrix",
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
  .trs = iquery(paste("eo_gettrs(",.scidb@name,")",sep=""),return=TRUE)
  .extent = iquery(paste("eo_extent(",.scidb@name,")",sep=""),return=TRUE)

  .scidb@isSpatial = (nrow(.srs) > 0)
  .scidb@isTemporal = (nrow(.trs) > 0)

  if (.scidb@isTemporal) { #make sure that there is actually a temporal reference
    .scidb@temporal_dim = .trs[,"tdim"]
    .scidb@tResolution = as.numeric(unlist(regmatches(.trs[,"dt"],gregexpr("(\\d)+",.trs[,"dt"]))))
    .scidb@tUnit = .findTUnit(.trs[,"dt"])
    .scidb@startTime = .getDateTime(.trs[,"t0"],.scidb@tUnit)

  }


  if (.scidb@isSpatial) {
    .scidb@affine <- .createAffineTransformation(.srs)
    .scidb@crs <- CRS(.srs$proj4text)
    .scidb@spatial_dims = list(xdim=.srs[,"xdim"],ydim=.srs[,"ydim"])
  }

  if (nrow(.extent > 0)) {
    .scidb@extent = extent(.extent[,"xmin"],.extent[,"xmax"],.extent[,"ymin"],.extent[,"ymax"])
    .scidb@tExtent = list(min=.getDateTime(.extent[,"tmin"],.scidb@tUnit),max=.getDateTime(.extent[,"tmax"],.scidb@tUnit))
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

.calcTDimIndex = function (x, time) {
  if (is.character(time)) {
    time = as.POSIXlt(.getDateTime(time,x@tUnit))
  } else {
    time = as.POSIXlt(time) #first simply try it
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

#' getValues method
#'
#' This function retrieves data from the remote scidb database and stores them internally like a Raster* object. This function
#' work in principle like scidbs array materialization 'array []'. However this function also needs the multidimensional array
#' to be reduced to a simple 2 dimensional array (spatial dimensions)
#'
#' @param x scidbst object
#'
#' @return vector or matrix of raster values
#'
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' array_proxy = scidbst(...)
#' getValues(array_proxy)
#' }
#' @export
setMethod("getValues", signature(x='scidbst', row='missing', nrows='missing'),
          function(x) {
            if (length(dimnames(x)) > 2 ) {
              stop("Too many dimensions detected. Try 'slice' to subset the image for example time.")
            }

            if (! inMemory(x) ) {
              if ( fromDisk(x) ) {
                x <- readAll(x)
              } else {
                return( matrix(rep(NA, ncell(x) * nlayers(x)), ncol=nlayers(x)) )
              }
            }
            #colnames(x@data@values) <- x@data@names
            x@data@names = scidb_attributes(x)
            colnames(x@data@values) <- scidb_attributes(x)

            x@data@values
          }
)

#' spplot for scidbst objects
#'
#' Like the raster version for spplot, this function plots a spatially referenced scidb array. To make this work on a
#' multidimensional array, the number of dimensions must be reduced to the correct two spatial dimensions. This can
#' be done by using the 'slice' operation of scidb.
#'
#' @param obj scidbst object
#'
#' @export
setMethod("spplot", signature(obj="scidbst"),function (obj, maxpixels=50000, as.table=TRUE, zlim,...) {
  if (length(dimnames(obj)) > 2 ) {
    stop("Too many dimensions detected. Try slice to make a 2D subset of the image.")
  }

  #following: code from raster::spplot
  obj <- sampleRegular(obj, maxpixels, asRaster=TRUE, useGDAL=TRUE)
  if (!missing(zlim)) {
    if (length(zlim) != 2) {
      warning('zlim should be a vector of two elements')
    }
    if (length(zlim) >= 2) {
      obj[obj < zlim[1] | obj > zlim[2]] <- NA
    }
  }

  if (inherits(obj,"scidb")) {
    attr_names = scidb_attributes(obj)
    obj <- as(obj, 'SpatialGridDataFrame')
    spplot(obj,..., as.table=as.table)
  } else {
    obj <- as(obj, 'SpatialGridDataFrame')
    spplot(obj,... , as.table=as.table)
  }

})

#' readAll
#'
#' Like \code{raster::getValues()} this function is called to retrieve the values from the source and store them in memory. It
#' differs from getValues in the fact that the scidbst object will be manipulated and returned back.
#'
#' @param object scidbst object
#' @return the modified scidbst object with values
#'
#' @export
setMethod('readAll', signature(object='scidbst'),
          function(object){
            # if (! object@data@fromdisk)  {
            #   stop('cannot read values; there is no file associated with this scidbst raster inheriting thing')
            # }
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


.materializeSCIDBValues = function(object, startrow, nrows=1, startcol=1, ncols=ncol(object)) {
  if (length(dimensions(object))>2) {
    stop("Array has more than two dimensions to fetch data in a raster format")
    #TODO if time is referenced allow download of multiple timesteps, if needed
  }
  offs <- c((startrow - 1), (startcol - 1))
  reg <- c(nrows, ncols)
  result <- matrix(nrow = (ncol(object)) * (nrow(object)), ncol = nlayers(object))

  extent = as.matrix(.calculateDimIndices(object,extent(object)))

  cat("Downloading data...\n")
  .data = iquery(object@name,return=T)

  dims = dimensions(object)
  ndims = length(dims)

  if (nrow(.data) == 0) { #scidb does not return data. Stop here
    stop("Image is empty.")
  }

  for (b in 1:object@data@nlayers) {
    lname = object@data@names[b]

    if (!all(is.na(.data[,lname]))) {

      if (ndims == 2) {
        ydim = getYDim(object)
        xdim = getXDim(object)

        tmp = matrix(nrow=(nrow(object)),ncol=(ncol(object)))
        m = .data[order(.data[,ydim],.data[,xdim]),]
        start_y = min(.data[,ydim])
        start_x = min(.data[,xdim])
        #shift x coordinates 0->1 and remove offset
        m[,xdim]=m[,xdim]+1-start_x
        #same for y
        m[,ydim]=m[,ydim]+1-start_y

        tmp2 = matrix(m[,lname],nrow=length(unique(m[,ydim])),ncol=length(unique(m[,xdim])),byrow=T)

        tmp[unique(m[,ydim]),unique(m[,xdim])] = tmp2
        #restructure the matrix to a one dimensional vector
        restruct = as.vector(t(tmp))


        result[,b] = restruct
      } else { #number of dimensions is 1
        tdim = getTDim(object)

        tmp = matrix(nrow=1,ncol=(max(.data[,tdim])-min(.data[,tdim])+1))
        m = .data[order(.data[,tdim]),]
        start_t = min(.data[,tdim])
        #shift t coordinates 0->1 and remove offset
        m[,tdim]=m[,tdim]+1-start_t

        tmp2 = matrix(m[,lname],nrow=1,ncol=length(unique(m[,tdim])),byrow=T)

        tmp[1,unique(m[,tdim])] = tmp2
        #restructure the matrix to a one dimensional vector
        restruct = as.vector(t(tmp))


        result[,b] = restruct
      }





    }
  }
  colnames(result) = scidb_attributes(object)
  return(result)
}

#' Regular Sample
#'
#' Take a systematic sample from a scidbst object.
#'
#' @inheritParams raster::sampleRegular
#' @export
#copied from raster with small changes
# changed "names(outras) <- names(x)" to "names(outras) <- scidb_attributes(x)"
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
                print("using GDAL")
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

            # calculates the number of cells
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

if (!isGeneric("slice")) {
  setGeneric("slice", function(x,d,n) standardGeneric("slice"))
}

.slice = function (x,d,n) {
  out = .scidbst_class(scidb::slice(x,d,n))

  .cpMetadata(x,out)
  if (d %in% x@temporal_dim) {
    baseTime = 0

    if (x@tUnit == "weeks") {
      baseTime = 7*24*60*60
    } else if (x@tUnit == "days") {
      baseTime = 24*60*60
    } else if (x@tUnit == "hours") {
      baseTime = 60 * 60
    } else if (x@tUnit == "mins") {
      baseTime = 60
    } else if (x@tUnit == "secs") {
      baseTime = 1
    } else {
      stop("currently no other temporal unit supported")
    }

    #adapt temporal extent
    newStart = as.POSIXlt(x@startTime + n * x@tResolution * baseTime)
    newEnd = as.POSIXlt(x@startTime + (n+1) * x@tResolution * baseTime)
    out@tExtent[["min"]] = newStart
    out@tExtent[["max"]] = newEnd
    out@isTemporal = TRUE
  }

  return(out)
}

#' Slice the array
#'
#' Takes a dimension name and a value to create a slice of an array. This usually means reducing the dimensions
#' of an array.
#'
#' @inheritParams scidb::slice
#' @return scidbst A new scidbst object with reduced number of dimensions
#' @export
setMethod('slice', signature(x="scidbst",d="character",n="ANY") , function(x,d,n) {
  if (d %in% x@temporal_dim) {
    if (is.character(n) || !tryCatch(is.na.POSIXlt(n,error=function(e) {return(TRUE)}))) {

      n = .calcTDimIndex(x,n)
    } else if (is.numeric(n)) {
      n = round(n)
    } else {
      stop("Not recognized value for time dimension during slice operation")
    }
  }

  return(.slice(x,d,n))
})


#' crop function
#'
#' This function creates a spatial subset of a scidbst array and returns the subset scidbst object.
#'
#' @param x scidbst object
#' @param y Extent object, or any object from which an Extent object can be extracted
#' @param snap Character. One of 'near', 'in', or 'out', for use with alignExtent
#' @param ...	Additional arguments as for writeRaster
#'
#' @return scidbst object with refined spatial extent
#' @export
setMethod('crop', signature(x='scidbst', y='ANY'),
    function(x, y, snap='near', ...) {
            if (length(dimnames(x)) > 2 ) {
              stop("More than two dimensions")
            }


            # as in the raster package, try to get the extent of object y
            y <- try ( extent(y), silent=TRUE )
            if (class(y) == "try-error") {
              stop('Cannot get an Extent object from argument y')
            }
            validObject(y)

            e <- intersect(extent(x), extent(y))
            e <- alignExtent(e, x, snap=snap)

            out = .calculateDimIndices(x,e)

            #TODO check creation with
            limits = as.matrix(out)

            res = subarray(x=x,limits=limits[dimensions(x),],between=TRUE)
            res = .scidbst_class(res)
            res = .cpMetadata(x,res) #first copy all, then adapt

            res@extent = e
            nrow(res) = (ymax(out) - ymin(out))+1
            ncol(res) = (xmax(out) - xmin(out))+1
            # +1 because origin in scidb is 0,0


            return(res)
            #apply eo_cpsrs / eo_(g/s)ettrs


          }
)

.cpMetadata = function(from,to) {
  if (class(from) == "scidbst" && class(to) == "scidbst") {
    to@extent = from@extent
    crs(to) = crs(from)

    to@affine = from@affine
    #nrow(to) = nrow(from)
    #ncol(to) = ncol(from) # should be calculated automatically now

    # +1 because origin in scidb is 0,0

    to@data@names = from@data@names
    to@data@nlayers = nlayers(from)
    to@data@fromdisk = TRUE


    to@spatial_dims = from@spatial_dims
    to@temporal_dim = from@temporal_dim
    to@startTime = from@startTime
    to@tExtent = from@tExtent
    to@tResolution = from@tResolution
    to@tUnit = from@tUnit
    to@isSpatial = from@isSpatial
    to@isTemporal = from@isTemporal

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
  show(scidb(object@name))
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
