#' @include utils.R
NULL



#' As SpatialPointsDataFrame
#'
#' Coerces a scidbst object into a \code{\link{SpatialPointsDataFrame}} by downloading the array information and recalculating
#' the spatial dimension indices into spatial coordinates.
#'
#' @name as-scidbst,SpatialPointsDataFrame
#' @family coerce-scidbst
#' @importClassesFrom sp SpatialPointsDataFrame
setAs("scidbst","SpatialPointsDataFrame",function(from,to) {

  if (from@isSpatial) {
    if (!from@isTemporal) {
      .data = .downloadData(from)
      if (nrow(.data) == 0) { #scidb does not return data. Stop here
        stop("Image is empty.")
      }

      coords = rbind(rep(1,nrow(.data)),.data[,xdim(from)],.data[,ydim(from)])
      res = t(affine(from) %*% coords) #much faster than the previous
      colnames(res) = c("sx","sy")

      res = as.data.frame(res)
      coordinates(res) <- ~sx+sy
      crs(res) <- crs(from)
      res = suppressWarnings(SpatialPointsDataFrame(res,as.data.frame(.data[,names(.data) %in% scidb_attributes(from)])))
      names(res@data) = scidb_attributes(from)

      return(res)
    } else {
      stop("Array has a temporal dimension. Cannot allocate time in SpatialPointsDataFrame. Please use aggregate or slice to remove the temporal dimension.")
    }

  } else {
    stop("No spatial coordinates found.")
  }
})

#' As RasterBrick
#'
#' Coerces a scidbst object into a \code{RasterBrick} object. The information of the array will be downloaded to the local client
#' and will be hold in memory. The spatial dimension indices will be recalculated into spatial coordinates during the process.
#'
#' @name as-scidbst,RasterBrick
#' @family coerce-scidbst
#' @importClassesFrom raster RasterBrick
setAs("scidbst","RasterBrick",function(from,to) {
  points = as(from,"SpatialPointsDataFrame")
  cat("Processing image structure...\n")
  gridded(points) = TRUE
  b = suppressWarnings(brick(points))
  return(b)
})


#' As STSDF
#'
#' Coerces a scidbst object into a STSDF object. Spatial and temporal dimension indices are calculated into real-world
#' coordinates using existing spatio-temporal references.
#'
#' @name as-scidbst,STSDF
#' @family coerce-scidbst
#' @importClassesFrom spacetime STSDF
setAs("scidbst","STSDF",function(from,to) {
  if (from@isSpatial && from@isTemporal) {
    .data = .downloadData(from)
    if (nrow(.data) == 0) { #scidb does not return data. Stop here
      stop("Image is empty.")
    }

    # sp
    .data = transformAllSpatialIndices(from,.data)

    # in principle it would be great to use unique locations, but it is hard to assign the indices of the tuples (with 'which')
    # so we are going to try out if it works with redundant points

    # time

    .data = transformAllTemporalIndices(from,.data)
    uniqueDates = unique(.data[,tdim(from)])
    xtsDates = xts(1:length(uniqueDates),order.by=uniqueDates) # no data just time

    # index
    indices = matrix(cbind(1:nrow(.data),rep(NA,nrow(.data))),nrow=nrow(.data),ncol=2)
    # first col refers to spatialpoints
    # second col refers to the dates
    for (pos in 1:length(uniqueDates)) {
      d = as.numeric(uniqueDates[pos])
      indices[as.numeric(.data[,tdim(from)])==d,2] = pos
    }
    sp = SpatialPoints(.data[,c(xdim(from),ydim(from))])
    crs(sp) = crs(from)
    gridded(sp) = TRUE
    # data

    stsdf = STSDF(sp=sp,time=xtsDates,data=.data[,scidb_attributes(from),drop=FALSE],index=indices)

    return(stsdf)

  } else {
    stop("Array has no temporal or spatial reference")
  }
})

setOldClass("xts")

#' As xts
#'
#' Coerces a scidbst object into a xts time series.
#'
#' @name as-scidbst,xts
#' @family coerce-scidbst
#' @importFrom xts xts
setAs("scidbst","xts",function(from,to) {
  if (from@isTemporal && !from@isSpatial) {
    .data = .downloadData(from)

    .attributes = as.data.frame(.data[,scidb_attributes((from))])
    colnames(.attributes) = scidb_attributes(from)
    ts = .data[,tdim(from)]
    if (tunit(from) == "days") {
      dates = lapply(ts,function(x,y){as.Date(.calculatePOSIXfromIndex(y,x))},y=from)
      dates = as.Date(unlist(dates),origin="1970-01-01")


      return(xts(.attributes,dates))
    } else {
      stop("Currently only tUnit = \"days\" is supported.")
    }

  } else {
    stop("The scidbst object is either not temporal or it contains spatially dimensions.")
  }
})

#' As data.frame
#'
#' Coerces the scidbst object into a data.frame object. The dimension values will NOT be recalculated!
#'
#' @note Dimensions and attributes will be treated as columns of the data.frame in the same way as \code{\link{[.scidb}}
#' does.
#'
#' @seealso \link{[.scidb}
#' @name as-scidbst,data.frame
#' @family coerce-scidbst
setAs("scidbst","data.frame",function(from,to) {
  return(.downloadData(from))
})

#' Returns the scidb proxy part of the scidbst object
#'
#' @name as-scidbst,scidb
#' @family coerce-scidbst
setAs("scidbst","scidb",function(from,to) {
  return(from@proxy)
})
