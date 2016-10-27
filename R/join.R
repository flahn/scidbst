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

# return B>A -> 1; A>B -> -1; A==B -> 0
.compareSpRes = function(A,B) {
  diff = res(B) - res(A)
  if (all(diff < 0)) {
    return(-1)
  } else if (all(diff > 0)) {
    return(1)
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

# return B>A -> 1; A>B -> -1; A==B -> 0
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


# joins attributes from A into B under the condition that both arrays are strictly spatial and have the same resolution and SRS
.join.normalized = function(A,B) {

  # code from Marius (modified)
  join.ref.st = transfer(A,B) #eo_over
  join.ref = as(join.ref.st,"scidb")

  attributestr = scidb:::build_attr_schema(as(A,"scidb"))

  ### rename the dimensions of the eo_over output (e.g. x,y,t -> x_old, y_old, t_old)
  dimrename = NULL
  if (xdim(A) == xdim(B)) {
    dimrename = rbind(dimrename, c(xdim(A),
                                   paste(xdim(A), "_orig", sep="")))
  }
  if (ydim(A) == ydim(B)) {
    dimrename = rbind(dimrename, c(ydim(A), paste(ydim(A), "_orig", sep="")))
  }
  if (A@isTemporal) {
    dimrename = rbind(dimrename, c(tdim(A), paste(tdim(A), "_orig", sep="")))
  }

  ### rename dimensions in scidb
  if (!is.null(dimrename)) {
    join.ref = dimension_rename(join.ref,dimrename[,1], dimrename[,2])
  }

  ### rename attributes into dimension (over_x, over_y, over_t -> x,y,t)
  attrrename = NULL
  if (B@isSpatial) {
    attrrename = rbind(attrrename, c("over_x",  xdim(B) ))
    attrrename = rbind(attrrename, c("over_y",  ydim(B) ))
  }
  if (B@isTemporal && A@isTemporal) {
    attrrename = rbind(attrrename, c("over_t",  tdim(B) ))
  }
  ### rename attributes in scidb
  join.ref = attribute_rename(join.ref,attrrename[,1],attrrename[,2])
  # now: join.ref is the array A which was translated into the dimension index space of B, renamed and joined values
  # also join.ref is a scidb object

  ### create array expression for scidb
  attributestr = scidb:::build_attr_schema(as(A,"scidb"))
  dimensionstr = scidb:::build_dim_schema(as(B,"scidb"))

  ### redimension
  q.redim = paste("redimension(", join.ref@name, ",",
                  paste(attributestr,dimensionstr,sep=""), ", false )", sep="")

  q.redim.scidb = scidb(q.redim)
  #q.redim is now the redimensioned array of join.ref (translated array A)

  #TODO evaluate statement and store as proxy of A, also overwrite the dimension names with the values of B (in case they are differently named)

  B.scidb = as(B,"scidb")

  ### Option join
  # q.join = paste("join(", q.redim , ",", scidb_op(B)  ,")", sep="")
  # B@proxy = scidb(q.join)

  ### Option scidb::merge (either join, equi_join or cross_join)
  # joined.scidb = merge(q.redim.scidb,B.scidb,by=intersect(scidb::dimensions(q.redim.scidb),scidb::dimensions(B.scidb)))
  # B@proxy = joined.scidb

  ### Option cross_join explicit
  # if both are not temporal it is ok
  # if not then the temporal array needs to be passed as first argument
  # in both cases B is the target and will be used for SRS and extent
  if (A@isTemporal || (!A@isTemporal && !B@isTemporal)) {
    # dim.match = paste(c("A","B"),cbind(rep(intersect(scidb::dimensions(q.redim.scidb),dimensions(B)),2)),sep=".",collapse=",")

    # intersectingDim = intersect(dimensions(A),dimensions(B))
    # dim.match = paste(c("A","B"),matrix(rep(intersectingDim ,2),ncol=length(intersectingDim),byrow=T),sep=".",collapse=",")

    dim.match = paste("A.",ydim(A),", B.",ydim(B),", A.", xdim(A),", B.",xdim(B),sep="")

    if (A@isTemporal && B@isTemporal) {
      dim.match = paste(dim.match,", A.",tdim(A),", B.",tdim(B),sep="")
    }

    q.cjoin = paste("cross_join(", q.redim , " as A,", scidb_op(B)  ," as B,",dim.match,")", sep="")
    B@proxy = scidb(q.cjoin)
    if (A@isTemporal && !B@isTemporal) {
      B@isTemporal = TRUE
      B@tExtent = A@tExtent
      B@trs = trs(A)
    }
    B@temps = c(B@temps,A@temps)
    return(B)
  } else { # B has temporal ref or both have
    # dim.match = paste(c("B","A"),cbind(rep(intersect(dimensions(B),scidb::dimensions(q.redim.scidb)),2)),sep=".",collapse=",")
    # dim.match = paste(c("B","A"),matrix(rep(intersect(dimensions(B),dimensions(A)),2),ncol=2,byrow=T),sep=".",collapse=",")

    dim.match = paste("B.",ydim(B),", A.",ydim(A),", B.", xdim(B),", A.",xdim(A),sep="")
    if (A@isTemporal && B@isTemporal) {
      dim.match = paste(dim.match,", B.",tdim(B),", A.",tdim(A),sep="")
    }
    q.cjoin = paste("cross_join(", scidb_op(B) , " as B,",  q.redim ," as A,",dim.match,")", sep="")
    B@proxy = scidb(q.cjoin)

    B@temps = c(B@temps,A@temps)
    return(B)
  }
}

if (!isGeneric("join")) {
  setGeneric("join",function(x,y,...) {
    standardGeneric("join")
  })
}

# returns a vector of names for potential temporary arrays
# x: scidbst, n: number of names to be created
.getTempNames = function(x,n) {
  if (!is.null(x@temps)) {
    usedIDs = as.integer(regmatches(x@temps,regexpr("(\\d)+$",x@temps)))
    ids = sample.int(2147483647,n,replace=FALSE)

    # rule out that there are any double ids
    loglist = any(ids %in% usedIDs)
    startExpr = loglist
    while(startExpr) {
      doublets = ids[startExpr]
      amount = length(doublets)
      newIds = sample.int(2147483647,amount,replace=FALSE)
      ids[startExpr] = newIds
      startExpr = any(ids %in% usedIDs)
    }
  } else {
    ids = sample.int(2147483647,n,replace=FALSE)
  }

  names = paste("__temp_",x@title,"_",ids,sep="")
  return(names)
}

setGeneric("equalize",function(x,y,...) {
  standardGeneric("equalize")
})

#' Equalizes the dimensionality of two scidbst arrays
#'
#' This function modifies the dimension properties of a source scidbst array to match the spatial
#' and/or temporal resolution of a target array. Therefore scidb will perform either an aggregation
#' for upscaling (higher to lower resolution) or it breaks down cells for a downscaling (lower to higher
#' resolution)
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
    stop("Currently not supported.")
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

  } else {
    #nothing to do here, simply return the array. both arrays have the same spatial resolution
    return(x)
  }

}

# x,y: scidbst objects
#taf: the temporal aggregation function (a scidb aggregation function)
.equalizeTemporal = function(x,y,storeTemp,taf) {

  # 0. sort arrays for their temporal resoultion
  sortedList = .sortTRes(x,y)
  A = sortedList$A #origin the higher resolution
  B = sortedList$B #target the lower resolution


  if (t0(A) != t0(B)) {
    # 1. get temporal extents
    A.te = t.extent(A)
    B.te = t.extent(B)

    # 2. calculate intersection (use the one with the lower resolution (e.g. pick 16 days over 12 hours))
    if (tmin(B) <= tmin(A)) {
      min = tmin(B)
    } else {
      # take tmin(A) and calculate "nearest" value in B and transform back to Date
      tminA.in.B = .calcTDimIndex(B,tmin(A))
      min = .calculatePOSIXfromIndex(B,tminA.in.B)
    }

    if (tmax(B) >= tmax(A)) {
      max = tmax(B)
    } else {
      # take tmin(A) and calculate "nearest" value in B and transform back to Date
      tmaxA.in.B = .calcTDimIndex(B,tmax(A))
      max = .calculatePOSIXfromIndex(B,tmaxA.in.B)
    }
    te = textent(min,max)

    # 3. create subarrays (between=FALSE) to set both arrays to the same t0
    A = subarray(A,te)
    B = subarray(B,te)

    if (storeTemp) {
      # 4. store temporarily
      A = scidbsteval(A,.getTempNames(A,1),temp=TRUE)
      B = scidbsteval(B,.getTempNames(B,1),temp=TRUE)
    }
  }
  A.res.sec = .tres2seconds(A)
  B.res.sec = .tres2seconds(B)

  if (A.res.sec != B.res.sec) {
    #if the time span is unequal then we need a regrid
    expr = .createExpression(A,taf)
    A = resample(A,B,expr,type="T") # use B as target grid structure for time

    if (storeTemp) {
      A.title = A@title
      A = scidbsteval(A,.getTempNames(A,1),temp=TRUE)
      A@title = A.title
    }
    return(list(A=A,B=B))
  } else {
    #nothing to do here
    return(list(A=A,B=B))
  }

}


.join = function (x,y,storeTemp=FALSE,name,saf="avg",taf="avg") {

  bothSpatial = x@isSpatial && y@isSpatial
  bothTemporal = x@isTemporal && y@isTemporal

  if (storeTemp && missing(name)) {
    stop("There is no name for the resulting array, if you want to optimize the process with temporary storing.")
  }

  # rename the attribute names to distinguish the attributes later on
  x.attr = paste(x@title,scidb_attributes(x),sep="_")
  y.attr = paste(y@title,scidb_attributes(y),sep="_")

  x@proxy = attribute_rename(as(x,"scidb"),scidb_attributes(x),x.attr)
  y@proxy = attribute_rename(as(y,"scidb"),scidb_attributes(y),y.attr)

  #TODO rework this function to accept two arrays that are dimension/resolution wise similar given a certain
  #tolerance

  # case 1: both arrays are spatial, but not temporal
  # case 2: both arrays are spatial, and 1 is temporal
  if (bothSpatial && !bothTemporal) {
    arrays = .equalizeSpatial(x,y,storeTemp,saf)
    A = arrays$A
    B = arrays$B

    #do normal join
    .out = .join.normalized(A,B)
    if (storeTemp) {
      scidbsteval(.out,name) #scidbsteval will delete the temporary arrays automatically
      .out = scidbst(name) #clean possible extent differences
    }
    return(.out)
  } else if (bothSpatial && bothTemporal) { #case 3: both spatial and temporal
    arrays = .equalizeSpatial(x,y,storeTemp,saf)
    arrays = .equalizeTemporal(arrays$A, arrays$B, storeTemp, taf)
    #at this point the arrays should have the same resolutions (temporally and spatial)
    #do normal join
    .out = .join.normalized(arrays$A,arrays$B)
    if (storeTemp) {
      scidbsteval(.out,name) #scidbsteval will delete the temporary arrays automatically
      .out = scidbst(name) #clean possible extent differences
    }
    return(.out)
  } else { # probably if both are temporal but not spatial...
    stop("Should not go here... Please contact the package author")
  }


}
#' Joins the attributes of x and y
#'
#' This function executes a join operation on two scidbst arrays, where all attributes of x and y are joined.
#'
#' @details At this point the join operation works spatial arrays only (this will be extended in the future). As a merge strategy the array with the
#' lower resolution will be choosen as the target array. The array with the higher resolution will be regridded into the target array structure, so that
#' cells can be merged by transfering the coordinates into the target arrays dimension indices.
#'
#' @param x scidbst object
#' @param y scidbst object
#' @param storeTemp logical whether or not some arrays are stored temporarily during the operation
#' @param name The name of the joined output array (required if storeTemp==TRUE)
#' @param raf the aggregation function to be applied during the spatial regrid
#' @return scidbst object with a combined object
#'
#' @export
setMethod("join",signature(x="scidbst",y="scidbst"), .join)
