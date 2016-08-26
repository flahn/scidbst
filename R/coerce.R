
#' Creates a SpatialPointsDataFrame from scidbst
#'
#' @name as
#' @family scidbst
#' @importClassesFrom sp SpatialPointsDataFrame
setAs("scidbst","SpatialPointsDataFrame",function(from,to) {
  if (!hasValues(from)) {
    .data = .downloadData(from)
    if (nrow(.data) == 0) { #scidb does not return data. Stop here
      stop("Image is empty.")
    }
  } else {
    stop("Object has data already.")
  }

  if (from@isSpatial) {
    if (!from@isTemporal) {
      coords = rbind(rep(1,nrow(.data)),.data[,getXDim(from)],.data[,getYDim(from)])
      res = t(from@affine %*% coords) #much faster than the previous
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

#' Creates a RasterBrick from scidbst
#'
#' @name as
#' @family scidbst
#' @importClassesFrom raster RasterBrick
setAs("scidbst","RasterBrick",function(from,to) {
  points = as(from,"SpatialPointsDataFrame")
  gridded(points) = TRUE
  b = suppressWarnings(brick(points))
  return(b)
})

setOldClass("STSDF")

#' Creates a STSDF from scidbst
#'
#' @name as
#' @family scidbst
#' @import spacetime
#' @importClassesFrom spacetime STSDF
setAs("scidbst","STSDF",function(from,to) {
  if (!hasValues(from)) {
    .data = .downloadData(from)
    if (nrow(.data) == 0) { #scidb does not return data. Stop here
      stop("Image is empty.")
    }

    coords = rbind(rep(1,nrow(.data)),.data[,getXDim(from)],.data[,getYDim(from)])
    res = t(from@affine %*% coords) #much faster than the previous
    colnames(res) = c("sx","sy")

    res = as.data.frame(res)
    coordinates(res) <- ~sx+sy
    crs(res) <- crs(from)
    # res done (spatial component)
    tdim = getTDim(from)
    tindex = unique(.data[,tdim])
    dates = lapply(tindex,function(x,y) {
      as.Date(.calculatePOSIXfromIndex(y,x))
    },y=from)
    time = .data[,tdim]

    for (i in tindex) {
      pos = which(tindex==i)
      time[time==i] = dates[[pos]]
    }
    time = as.Date(time,origin="1970-01-01")
    endTime = max(time)+1
    browser()
    #time done (temporal component)

    IDs = paste("ID",1:length(time),sep="_")

    mydata = cbind(IDs,.data[,scidb_attributes(from)])
    stfdf = STFDF(res,time,endTime=as.POSIXct(time),mydata)
    return(as(stfdf,"STSDF"))


  } else {
    stop("Object has data already.")
  }

  if (from@isSpatial && from@isTemporal) {

  } else {
    stop("Array has no temporal or spatial reference")
  }
})

setOldClass("xts")

#' Creates a RasterBrick from scidbst
#'
#' @name as
#' @family scidbst
#' @importFrom xts xts
setAs("scidbst","xts",function(from,to) {
  if (from@isTemporal && !from@isSpatial) {
    .data = .downloadData(from)

    .attributes = as.data.frame(.data[,scidb_attributes((from))])
    colnames(.attributes) = scidb_attributes(from)
    ts = .data[,getTDim(from)]
    if (from@tUnit == "days") {
      dates = lapply(ts,function(x,y){as.Date(.calculatePOSIXfromIndex(y,x))},y=from)
      dates = as.Date(unlist(dates),origin="1970-01-01")


      return(xts(.attributes,dates))
    } ese {
      stop("Currently only tUnit = \"days\" is supported.")
    }

  } else {
    stop("The scidbst object is either not temporal or it contains spatially dimensions.")
  }
})
