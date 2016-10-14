# checks if two scidbst object have the same coordinate reference system
# x:scidbst, y: scidbst
# returns: logical
.equalSRS = function(x,y) {
  return(x@srs@authority == y@srs@authority && x@srs@srid == y@srs@srid)
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
.compareSpRes = function (A,B) {
  if (all(res(A)>res(B))) {
    return(list(A=B,B=A))
  } else if (all(res(A)<=res(B))) {
    return(list(A=A,B=B))
  } else {
    stop("The spatial resolution is in one arrays dimension higher than the other and in the other lower.")
  }
}


# joins attributes from A into B under the condition that both arrays are strictly spatial and have the same resolution and SRS
.join.spatial.normalized = function(A,B) {
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
  # if (B@isTemporal) {
  #   attrrename = rbind(attrrename, c("over_t",  tdim(B) ))
  # }
  ### rename attributes in scidb
  join.ref = attribute_rename(join.ref,attrrename[,1],attrrename[,2])


  ### create array expression for scidb
  attributestr = scidb:::build_attr_schema(as(A,"scidb"))
  dimensionstr = scidb:::build_dim_schema(as(B,"scidb"))

  ### redimension
  q.redim = paste("redimension(", join.ref@name, ",",
                  paste(attributestr,dimensionstr,sep=""), ", false )", sep="")

  # TODO join is maybe deprecated in the future (use cross_join) --> done
  q.redim.scidb = scidb(q.redim)
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
  # in both cases B is spatial target and will be used for SRS and extent
  if (A@isTemporal || (!A@isTemporal && !B@isTemporal)) {
    # dim.match = paste(c("A","B"),cbind(rep(intersect(scidb::dimensions(q.redim.scidb),dimensions(B)),2)),sep=".",collapse=",")
    dim.match = paste(c("A","B"),matrix(rep(intersect(dimensions(A),dimensions(B)),2),ncol=2,byrow=T),sep=".",collapse=",")
    q.cjoin = paste("cross_join(", q.redim , " as A,", scidb_op(B)  ," as B,",dim.match,")", sep="")
    B@proxy = scidb(q.cjoin)
    if (A@isTemporal) {
      B@isTemporal = TRUE
      B@tExtent = A@tExtent
      B@trs = trs(A)
    }
    B@temps = c(B@temps,A@temps)
    return(B)
  } else { # B has temporal ref
    # dim.match = paste(c("B","A"),cbind(rep(intersect(dimensions(B),scidb::dimensions(q.redim.scidb)),2)),sep=".",collapse=",")
    dim.match = paste(c("B","A"),matrix(rep(intersect(dimensions(B),dimensions(A)),2),ncol=2,byrow=T),sep=".",collapse=",")
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

# prepares the arrays in the form that both arrays have the same spatial resolution
# x,y : scidbst objects
# raf the aggregation function for the regridding
# return list with two scidbst objects (A - the resampled array (former higher resolution),
# B - the target array in terms of spatial resolution)
.equalizeSpatial = function(x,y,raf) {

  bothSpatial = x@isSpatial && y@isSpatial
  if (!bothSpatial) stop("Arrays are not spatial. This feature is currently not supported")

  if (!.equalSRS(x,y)) stop("The arrays have different spatial reference systems. Currently resampling methods are not provided by 'scidbst' or in 'SciDB'")

  #TODO check extents and crop if needed

  if (!.equalRes(x,y)) {
    # bring resolution together (regrid) and sort from higher resolution to lower
    res.ls = .compareSpRes(x,y)
    # resample A into B
    A = res.ls$A
    B = res.ls$B
    expr = .createExpression(A,raf)
    A = resample(A,B,expr) # use B as target grid structure

    if (storeTemp) {
      B = scidbsteval(B, .getTempNames(B,1), temp=TRUE)
      A = scidbsteval(A, .getTempNames(A,1), temp=TRUE)
    }
    return(list(A=A,B=B))
  } else {
    # nothing to do here
    return(list(A=x,B=y))
  }

}


.join = function (x,y,storeTemp=FALSE,name,raf="avg") {

  bothSpatial = x@isSpatial && y@isSpatial
  bothTemporal = x@isTemporal && y@isTemporal

  if (storeTemp && missing(name)) {
    stop("There is no name for the resulting array, if you want to optimize the process with temporary storing.")
  }

  # case 1: both arrays are spatial, but not temporal
  # case 2: both arrays are spatial, and 1 is temporal
  if (bothSpatial && !bothTemporal) {
    arrays = .equalizeSpatial(x,y)
    A = arrays$A
    B = arrays$B

    #do normal join
    .out = .join.spatial.normalized(A,B)
    if (storeTemp) {
      scidbsteval(.out,name) #scidbsteval will delete the temporary arrays automatically
      .out = scidbst(name) #clean possible extent differences
    }
    return(.out)
  } else if (bothSpatial && bothTemporal) { #case 3: both spatial and temporal

  } else {
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
