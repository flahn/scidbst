.renameAfterEO_OVER = function(over,target,redimAttr) {
  A = over
  B = target

  join.ref = as(A,"scidb")

  attributestr = scidb:::build_attr_schema(as(A,"scidb"))

  ### rename the dimensions of the eo_over output (e.g. x,y,t -> x_old, y_old, t_old)
  dimrename = NULL
  if (A@isSpatial) {
    dimrename = rbind(dimrename, c(ydim(A), paste(ydim(A), "_orig", sep="")))
    dimrename = rbind(dimrename, c(xdim(A), paste(xdim(A), "_orig", sep="")))
  }
  if (A@isTemporal) {
    dimrename = rbind(dimrename, c(tdim(A), paste(tdim(A), "_orig", sep="")))
  }

  ### rename dimensions in scidb
  if (!is.null(dimrename)) {
    join.ref = dimension_rename(join.ref,dimrename[,1], dimrename[,2])
  }

  #make sure the calculated over values are within the dimensional bounds of B, otherwise redimension will fail
  startsB = scidb_coordinate_start(as(B,"scidb"))
  endsB = scidb_coordinate_end(as(B,"scidb"))
  boundsB = NULL
  if (A@isSpatial) {
    dimsB = c("over_y","over_x")
    ypos = which(dimensions(B) == ydim(B))
    xpos = which(dimensions(B) == xdim(B))
    boundsB = cbind(c(startsB[ypos],startsB[xpos]),c(endsB[ypos],endsB[xpos]))
  }

  if (A@isTemporal) {
    dimsB = c(dimsB,"over_t")
    tpos = which(dimensions(B) == tdim(B))
    if (is.null(boundsB)) {
      boundsB = cbind(c(startsB[tpos]),c(endsB[tpos]))
    } else {
      boundsB[,1] = c(boundsB[,1],startsB[tpos])
      boundsB[,2] = c(boundsB[,2],endsB[tpos])
    }
  }

  filter_expr = paste(dimsB," >= ",boundsB[,1]," and ",dimsB," <= ",boundsB[,2], collapse=" and ",sep="")
  join.ref = scidb:::filter_scidb(join.ref,filter_expr)

  ### rename attributes into dimension (over_x, over_y, over_t -> x,y,t)
  attrrename = NULL
  if (A@isSpatial) {
    attrrename = rbind(attrrename, c("over_x",  xdim(B) ))
    attrrename = rbind(attrrename, c("over_y",  ydim(B) ))
  }
  if (A@isTemporal) { # A is the source... we want to modify A for B, if A not temporal than don't rename it

    attrrename = rbind(attrrename, c("over_t",  tdim(B) ))
  }
  ### rename attributes in scidb
  join.ref = attribute_rename(join.ref,attrrename[,1],attrrename[,2])
  # now: join.ref is the array A which was translated into the dimension index space of B, renamed and joined values
  # also join.ref is a scidb object

  ### create array expression for scidb

  # attributestr = scidb:::build_attr_schema(as(A,"scidb"))
  # dimensionstr = scidb:::build_dim_schema(as(B,"scidb")) # build on our own... if A is not temporal and B is this leads to problems
  # [y=0:1836,2048,0,x=0:1836,2048,0,t=0:64,1,0]
  dimensionstr = .dimStr(A,B)


  ### redimension (Attribute of old A [redimAttr])
  q.redim = paste("redimension(", join.ref@name, ",",
                  paste(redimAttr,dimensionstr,sep=""), ", false )", sep="")

  q.redim.scidb = scidb(q.redim)
  A@proxy = q.redim.scidb

  # if A was not temporal or spatial don't force it...
  # A@isSpatial = B@isSpatial
  # A@isTemporal = B@isTemporal

  if (A@isSpatial) {
    A@srs@dimnames = B@srs@dimnames #since B is target structure only dimnames of B are valid
  } else {
    A@srs = NULL
    A@extent = NULL
  }

  if (!A@isTemporal) {
    A@trs = NULL
    A@tExtent = NULL
  } else {
    if (B@isTemporal) {
      A@trs = B@trs
    }
  }

  return(A)
}

#creates a subset of a dimension string
.dimStr = function (A,B) {
  # A is the source array
  # B is the target array from which we will take the boundaries
  expr = ""
  dimension = dimensions(B)
  start = scidb_coordinate_start(B)
  end = scidb_coordinate_end(B)
  chunk = scidb_coordinate_chunksize(B)
  overlap = scidb_coordinate_overlap(B)
  if (A@isSpatial) {
    ypos = which(dimension == ydim(B))
    ydimExpr = paste(dimension[ypos],"=",start[ypos],":",end[ypos],",",chunk[ypos],",",overlap[ypos],sep="")
    xpos = which(dimension == xdim(B))
    xdimExpr = paste(dimension[xpos],"=",start[xpos],":",end[xpos],",",chunk[xpos],",",overlap[xpos],sep="")
    expr = paste(ydimExpr,", ",xdimExpr,sep="")
  }
  if (A@isTemporal) {
    tpos = which(dimension == tdim(B))
    tdimExpr = paste(dimension[tpos],"=",start[tpos],":",end[tpos],",",chunk[tpos],",",overlap[tpos],sep="")
    expr = paste(expr,", ",tdimExpr,sep="")
  }
  expr = paste("[",expr,"]",sep="")
  return(expr)
}

# joins attributes from A into B under the condition that both arrays are strictly spatial and have the same resolution and SRS
.join.equalized = function(A,B,storeTemp=FALSE) {

  # code from Marius (modified)
  oldAttr = scidb:::build_attr_schema(as(A,"scidb"))
  join.ref.st = transfer(A,B) #eo_over

  A = .renameAfterEO_OVER(join.ref.st,B,oldAttr)
  #A is now the redimensioned array of join.ref (translated array A) and the dimension names are also changed into the names of B

  # scidbsteval as temp if storeTemp true
  if (storeTemp) {
    A = scidbsteval(A,.getTempNames(A,1),temp=TRUE)
  }


  # now, either temp(A & B) or !temp(A & B)
  #in each case use B as the target array, meaning it will go on the left side of the crossjoin
  dim.match = NULL
  if (A@isSpatial) {
    dim.match = paste("B.",ydim(B),", A.",ydim(A),", B.", xdim(B),", A.",xdim(A),sep="")
  }

  if (A@isTemporal) {
    if (!is.null(dim.match)) {
      dim.match = paste(dim.match,", B.",tdim(B),", A.",tdim(A),sep="")
    } else {
      dim.match = paste("B.",tdim(B),", A.",tdim(A),sep="")
    }
  }
  if (is.null(dim.match)) {
    stop("There are no matching dimensions for a join. Probably the one or both arrays are neither spatially nor temporally referenced.")
  }


  q.cjoin = paste("cross_join(", scidb_op(B) , " as B,",  scidb_op(A) ," as A,",dim.match,")", sep="")
  B@proxy = scidb(q.cjoin)

  B@temps = c(B@temps,A@temps)
  return(B)
}

if (!isGeneric("join")) {
  setGeneric("join",function(x,y,...) {
    standardGeneric("join")
  })
}


.join = function (x,y,storeTemp=FALSE) {
  if (x@isSpatial && x@isTemporal && !y@isTemporal) {
    stop("Error: Attempting to join a spatio-temporal array into a spatial array. Please consider aggregation over time of the first array or switch arrays.")
  }

  bothSpatial = x@isSpatial && y@isSpatial
  bothTemporal = x@isTemporal && y@isTemporal

  # rename the attribute names to distinguish the attributes later on
  x.attr = paste(x@title,scidb_attributes(x),sep="_")
  y.attr = paste(y@title,scidb_attributes(y),sep="_")

  x@proxy = attribute_rename(as(x,"scidb"),scidb_attributes(x),x.attr)
  y@proxy = attribute_rename(as(y,"scidb"),scidb_attributes(y),y.attr)

  if (!.equalSRS(x,y)) {
    stop("Cannot join two arrays with different SRSs")
  }

  ### check if the two arrays are similar in dimensionality (resolution)
  if (bothSpatial) {
    dif.x = abs(xres(x)-xres(y))
    dif.y = abs(yres(x)-yres(y))
    if (dif.x/max(xres(x),xres(y)) > 0.1  && dif.y/max(yres(x),yres(y)) > 0.1 ) {
      stop("Spatial resolution differs. Please consider using 'equalize' before joining the arrays.")
    }

    #### check if the extents are similar
    ex = extent(x)
    ey = extent(y)

    ei = intersect(ex,ey)
    diffs = c(abs(xmin(ex)-xmin(ey)),abs(xmax(ex)-xmax(ey)),abs(ymin(ex)-ymin(ey)),abs(ymax(ex)-ymax(ey)))

    #calculate sp tolerance
    rx = max(xres(x),xres(y))
    ry = max(yres(x),yres(y))
    delta = 2*sqrt(rx^2 + ry^2)

    # if (!all(diffs <= delta)) stop("The spatial dimensions, do not have a similar extent. Please consider cropping first.")
  }

  if (bothTemporal) {
    dif.t = abs(.tres2seconds(x)-.tres2seconds(y))
    if (dif.t/max(.tres2seconds(x), .tres2seconds(y)) > 0.1) {
      stop("Temporal resolution differs. Please consider using 'equalize' before the join")
    }

    #TODO check temporal extent
    # is B's tmin inside t.extent(A) ?
  }

  #create temporary arrays of input if there is an expression involved
  if (storeTemp) {
    x.op = scidb_op(x)
    if (grepl("\\(",x.op)) {
      #store x temp
      x = scidbsteval(x,.getTempNames(x,1),temp=TRUE)
    }
    y.op = scidb_op(y)
    if (grepl("\\(",y.op)) {
      #store y temp
      y = scidbsteval(y,.getTempNames(y,1),temp=TRUE)
    }

  }

  #join.normalized meaning that x will be joined into the dimension structure of B
  .out = .join.equalized(x,y,storeTemp=storeTemp)

  return(.out)
}


#' Joins the attributes of x and y
#'
#' This function executes a join operation on two scidbst arrays, where all attributes of x and y are joined. To make it more concrete
#' the array dimension structure of y will be used as target structure for the dimensions. Read the method as "join x into y".
#'
#' @details The join function will assume that the two arrays are similar in their dimensional representation. To be more concrete, x will
#' be modified into the array structure including lower and upper dimensional boundaries, overlaps and chunksizes. To make sure the arrays
#' are similar start by using \code{\link{equalize}} to smooth the transition by equalizing the cell resolution.
#'
#' @param x scidbst object
#' @param y scidbst object
#' @param storeTemp logical whether or not some arrays are stored temporarily during the operation
#' @return scidbst object with a combined object
#'
#' @seealso \code{\link[scidb]{merge.scidb}}, \code{\link{equalize}}
#' @export
setMethod("join",signature(x="scidbst",y="scidbst"), .join)
