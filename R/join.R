

# joins attributes from A into B under the condition that both arrays are strictly spatial and have the same resolution and SRS
.join.equalized = function(A,B) {

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


.join = function (x,y,storeTemp=FALSE,name) {

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
  }

  if (bothTemporal) {
    dif.t = abs(.tres2seconds(x)-.tres2seconds(y))
    if (dif.t/max(.tres2seconds(x), .tres2seconds(y)) < 0.1) {
      stop("Temporal resolution differs. Please consider using 'equalie' before the join")
    }
  }

  #### check if the extents are similar
  ex = extent(x)
  ey = extent(y)

  ei = intersect(ex,ey)
  diffs = c(abs(xmin(ex)-xmin(ey)),abs(xmax(ex)-xmax(ey)),abs(ymin(ex)-ymin(ey)),abs(ymax(ex)-ymax(ey)))

  #calculate sp tolerance
  rx = max(xres(x),xres(y))
  ry = max(yres(x),yres(y))
  delta = sqrt(rx^2 + ry^2)
  if (!all(diffs <= delta)) stop("The spatial dimensions, do not have a similar extent. Please consider cropping first.")

  #TODO check temporal extent


  #join.normalized
  .out = .join.equalized(x,y)
  if (storeTemp) {
    scidbsteval(.out,name) #scidbsteval will delete the temporary arrays automatically
    .out = scidbst(name) #clean possible extent differences
  }
  return(.out)
  # # case 1: both arrays are spatial, but not temporal
  # # case 2: both arrays are spatial, and 1 is temporal
  # if (bothSpatial && !bothTemporal) {
  #   arrays = .equalizeSpatial(x,y,storeTemp,saf)
  #   A = arrays$A
  #   B = arrays$B
  #
  #   #do normal join
  #   .out = .join.normalized(A,B)
  #   if (storeTemp) {
  #     scidbsteval(.out,name) #scidbsteval will delete the temporary arrays automatically
  #     .out = scidbst(name) #clean possible extent differences
  #   }
  #   return(.out)
  # } else if (bothSpatial && bothTemporal) { #case 3: both spatial and temporal
  #   arrays = .equalizeSpatial(x,y,storeTemp,saf)
  #   arrays = .equalizeTemporal(arrays$A, arrays$B, storeTemp, taf)
  #   #at this point the arrays should have the same resolutions (temporally and spatial)
  #   #do normal join
  #
  #   return(.out)
  # } else { # probably if both are temporal but not spatial...
  #   stop("Should not go here... Please contact the package author")
  # }


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
#' @return scidbst object with a combined object
#'
#' @export
setMethod("join",signature(x="scidbst",y="scidbst"), .join)
