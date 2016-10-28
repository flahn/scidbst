# checks if two scidbst object have the same coordinate reference system
# x:scidbst, y: scidbst
# returns: logical
.equalSRS = function(x,y) {
  return(x@srs@authority == y@srs@authority && x@srs@srid == y@srs@srid)
}

# checks if two scidnst objects have the same temporal reference
# x,y : scidbst object
# returns : logical
.equalTRS = function(x,y) {
  if (!(x@isTemporal && y@isTemporal)) {
    stop("One array does not have a time dimension.")
  }
  sameUnit = (tunit(x) == tunit(y))
  sameT0 = (t0(x) == t0(y))
  sameTRes = (tres(x) == tres(y))

  return((sameUnit && sameT0 && sameTRes))
}

# checks if two scidbst arrays have the same spatial resolution
# x:scidbst, y: scidbst
# returns: logical
.equalRes = function(x,y) {
  return(all(res(x)==res(y)))
}

#returns a list of scidbst objects in the order A has a higher resolution as B (to merge A into B)
# A: scidst , B: scidbst
# return: list of scidbst with object A having a lower resolution as object B
.sortSpRes = function (A,B) {
  if (all(res(A)>res(B))) {
    return(list(A=B,B=A))
  } else if (all(res(A)<=res(B))) {
    return(list(A=A,B=B))
  } else {
    stop("The spatial resolution is in one arrays dimension higher than the other and in the other lower.")
  }
}

# return B>A -> 1 A has a higher res than B; A>B -> -1 A has a lower resolution than B; A==B -> 0
.compareSpRes = function(A,B) {
  diff = res(B) - res(A)
  if (all(diff < 0)) {
    return(1)
  } else if (all(diff > 0)) {
    return(-1)
  } else if (all(diff == 0)) {
    return(0)
  } else {
    stop("The spatial resolution is in one arrays dimension higher than the other and in the other lower. This is currently not supported.")
  }
}



.sortTRes = function(A,B) {
  if (tunit(A) != tunit(B)) {
    # no comparable tUnit translate into seconds as "smallest unit"
    A.sec = .tres2seconds(A)
    B.sec = .tres2seconds(B)

    if (A.sec > B.sec) { # the higher resolution value means to be the target
      return(list(A=B,B=A))
    } else {
      return(list(A=A,B=B))
    }
  } else {
    if (tres(A) > tres(B)) {
      return(list(A=B,B=A))
    } else {
      return(list(A=A,B=B))
    }
  }
}

# return B>A -> 1 A has higher res than B; A>B -> -1 A has lower res than B; A==B -> 0
.compareTRes = function(A,B) {
  if (tunit(A) != tunit(B)) {
    # no comparable tUnit translate into seconds as "smallest unit"
    A.sec = .tres2seconds(A)
    B.sec = .tres2seconds(B)

    if (A.sec > B.sec) { # the higher resolution value means to be the target
      return(-1)
    } else if (A.sec < B.sec) {
      return(1)
    } else {
      return(0)
    }
  } else {
    if (tres(A) > tres(B)) {
      return(-1)
    } else if (tres(A) < tres(B)){
      return(1)
    } else {
      return(0)
    }
  }
}

setGeneric("equalize",function(x,y,...) {
  standardGeneric("equalize")
})

#' Equalizes the dimensionality of two scidbst arrays
#'
#' This function modifies the dimension properties of a source scidbst array to match the spatial
#' and/or temporal resolution of a target array. Therefore scidb will perform either an aggregation
#' for upscaling (higher to lower resolution) or it breaks down cells for a downscaling (lower to higher
#' resolution).
#'
#' @note Please consider to pick an appropriate aggregation function when aggregating. This means that it depends
#' on the observed phenomenon, how measurements shall be aggregated.
#'
#' @param x source scidbst array
#' @param y target scidbst array
#' @param type character one of c("S","T","ST") two equalize spatial, temporal or spatio-temporal
#' @param storeTemp logical - whether or not to preevaluate the arrays and store them temporarily
#' @param raf the spatial aggregation function to apply
#' @param taf the temporal aggregation function to apply
#' @return the modified scidbst array source array with the dimensional representation as the target
#' @export
#' @seealso \href{http://www.paradigm4.com/HTMLmanual/15.7/scidb_ug/Aggregates.html}{scidb aggregation functions}
setMethod("equalize", signature(x="scidbst",y="scidbst"), function(x,y,type,storeTemp,saf,taf) {
  type = toupper(type)
  if (!type %in% c("S","T","ST")) {
    stop("Please specifiy how in which dimension(s) the arrays shall be equalized")
  }
  if (type == "S") {
    #TODO rework equalizeSpatial to distinguish source and target array, using either aggregate or xgrid
    arrays = .equalizeSpatial(x,y,storeTemp,saf)
  } else if (type == "T") {
    #TODO same thing as for equalizeSpatial
    arrays = .equalizeTemporal(x,y,storeTemp,taf)
  } else if (type == "ST") {
    arrays = .equalizeSpatial(x,y,storeTemp,saf)
    arrays = .equalizeTemporal(arrays$A, arrays$B, storeTemp, taf)
  } else {
    stop("This should not happened. Wrong type.")
  }
  return(arrays$A) #returns the adapted source array
})

# prepares the arrays in the form that both arrays have the same spatial resolution
# x,y : scidbst objects
# saf the aggregation function for the regridding
# return list with two scidbst objects (A - the resampled array (former higher resolution),
# B - the target array in terms of spatial resolution)
.equalizeSpatial = function(x,y,storeTemp,saf) {

  bothSpatial = x@isSpatial && y@isSpatial
  if (!bothSpatial) stop("Arrays are not spatial. This feature is currently not supported")

  if (!.equalSRS(x,y)) stop("The arrays have different spatial reference systems. Currently resampling methods are not provided by 'scidbst' or in 'SciDB'")


  #check extents and crop if needed
  ex = extent(x)
  ey = extent(y)

  ei = intersect(ex,ey)
  diffs = c(abs(xmin(ex)-xmin(ey)),abs(xmax(ex)-xmax(ey)),abs(ymin(ex)-ymin(ey)),abs(ymax(ex)-ymax(ey)))

  #calculate tolerance
  rx = max(xres(x),xres(y))
  ry = max(yres(x),yres(y))
  delta = sqrt(rx^2 + ry^2)

  compXY = .compareSpRes(x,y)

  if (compXY < 0) {
    # x < y means x has a higher resolution than y -> regrid/aggregate
    if (any(diffs > delta)) {


      ei.y = .calculateDimIndices(y,ei) #lower resolution and potentially larger extent
      ei.x = .calculateDimIndices(x,ei.y) #calculate the bbox for A from Bs intersected extent
      ei = intersect(ei.x,ei.y) # should be close to the correct one

      cropped.x = crop(x,ei)
      cropped.y = crop(y,ei)
      if (storeTemp) {
        x = scidbsteval(cropped.x,temp=TRUE,name=.getTempNames(cropped.x,1))
        y = scidbsteval(cropped.y,temp=TRUE,name=.getTempNames(cropped.y,1))
        #rename the title because otherwise we will have internal temp_xxx_2345234534 chains for temp arrays
      } else {
        x = cropped.x
        y = cropped.y
      }
    }

    # bring resolution together (regrid) and sort from higher resolution to lower
    # resample A into B

    expr = .createExpression(x,saf)
    x = resample(x,y,expr) # use B as target grid structure

    if (storeTemp) {
      # y = scidbsteval(y, .getTempNames(y,1), temp=TRUE) #shouldn't be necessary since there are no changes really
      x = scidbsteval(x, .getTempNames(x,1), temp=TRUE)
    }
    return(list(A=x,B=y))

  } else if (compXY > 0) {
    # x > y means x has a lower resolution than y -> break down with xgrid
    # stop("Currently not supported.")
    if (any(diffs > delta)) {


      ei.x = .calculateDimIndices(x,ei) #lower resolution and potentially larger extent
      ei.y = .calculateDimIndices(y,ei.x) #calculate the bbox for A from Bs intersected extent
      ei = intersect(ei.y,ei.x) # should be close to the correct one

      cropped.y = crop(y,ei)
      cropped.x = crop(x,ei)
      if (storeTemp) {
        x = scidbsteval(cropped.x,temp=TRUE,name=.getTempNames(cropped.x,1))
        y = scidbsteval(cropped.y,temp=TRUE,name=.getTempNames(cropped.y,1))
        #rename the title because otherwise we will have internal temp_xxx_2345234534 chains for temp arrays
      } else {
        x = cropped.x
        y = cropped.y
      }
    }
    x = xgrid(x,y,type="S")
    if (storeTemp) {
      x = scidbsteval(x,.getTempNames(x,1), temp=TRUE)
    }
    return(list(A=x,B=y))
  } else {
    #nothing to do here, simply return the array. both arrays have the same spatial resolution considering the tolerance
    return(list(A=x,B=y))
  }

}

# x,y: scidbst objects
#taf: the temporal aggregation function (a scidb aggregation function)
.equalizeTemporal = function(x,y,storeTemp,taf) {

  # 0. sort arrays for their temporal resoultion
  sortedList = .sortTRes(x,y)
  # A = sortedList$A #origin the higher resolution
  # B = sortedList$B #target the lower resolution


  compXY = .compareSpRes(x,y)

  if (t0(x) != t0(y)) {
    # 1. get temporal extents
    x.te = t.extent(x)
    y.te = t.extent(y)

    # 2. calculate intersection (use the one with the lower resolution (e.g. pick 16 days over 12 hours))
    if (tmin(y) <= tmin(x)) {
      min = tmin(y)
    } else {
      # take tmin(A) and calculate "nearest" value in B and transform back to Date
      tminx.in.y = .calcTDimIndex(y,tmin(x))
      min = .calculatePOSIXfromIndex(y,tminx.in.y)
    }

    if (tmax(y) >= tmax(x)) {
      max = tmax(y)
    } else {
      # take tmin(A) and calculate "nearest" value in B and transform back to Date
      tmaxx.in.y = .calcTDimIndex(y,tmax(x))
      max = .calculatePOSIXfromIndex(y,tmaxx.in.y)
    }
    te = textent(min,max)

    # 3. create subarrays (between=FALSE) to set both arrays to the same t0
    x = subarray(x,te)
    y = subarray(y,te)

    if (storeTemp) {
      # 4. store temporarily
      x = scidbsteval(x,.getTempNames(x,1),temp=TRUE)
      y = scidbsteval(y,.getTempNames(y,1),temp=TRUE)
    }
  }
  x.res.sec = .tres2seconds(x)
  y.res.sec = .tres2seconds(y)

  if (compXY < 0) {
    #if the time span is unequal then we need a regrid
    expr = .createExpression(x,taf)
    x = resample(x,y,expr,type="T") # use B as target grid structure for time

    if (storeTemp) {
      x = scidbsteval(x,.getTempNames(x,1),temp=TRUE)
    }
    return(list(A=x,B=y))
  } else if (compXY > 0){
    x = xgrid(x,y,type="T")
    if(storeTemp) {
      x = scidbsteval(x,.getTempNames(x,1),temp=TRUE)
    }
    return(list(A=x,B=y))
  } else {
    #nothing to do here
    return(list(A=x,B=y))
  }

}
