#' @include scidbst-class.R
NULL

# prepares the grid statement for the scidb call, which reflects the new resolution in dimension
# indices (how many cells are to be combined in each array dimension)
.prepareGrid = function (x, y) {
      .dims = dimensions(x)
      grid = rep(1,length(.dims)) #if no changes here, then we get a copy (1 to 1 cell relation)
      names(grid) = .dims
      if (!missing(y)) {
          oldnx = .ncol(x)
          oldny = .nrow(x)
          newnx = .ncol(y)
          newny = .nrow(y)
          if (newny > oldny && newnx > oldnx) {
            stop("Error: Cannot resample the array, because the output array has at least partially a higher resolution than the input")
          }
          #resample way
          e = extent(y)
          xsize = round(oldnx/newnx)
          ysize = round(oldny/newny)

          #replace spatial grid sizes
          if(x@isSpatial) {
              grid[xdim(x)] = xsize
              grid[ydim(x)] = ysize
          }
      }
      return(grid)
}

.regrid.scidbst = function(x, grid, expr) {
  expr = .createExpression(x,expr)

  .dims = dimensions(x)
  names(grid) = .dims

  #feed parameter to scidb::regrid
  sci.obj = as(x,"scidb")
  sci.obj = regrid(x=sci.obj,grid=grid, expr=expr)
  x@proxy = sci.obj

  if (x@isSpatial && (grid[xdim(x)] > 1 || grid[ydim(x)] > 1)) {
    #adapt affine transformation
    scale_matrix = matrix(c(1,0,0,0,grid[xdim(x)],0,0,0,grid[ydim(x)]),ncol=3)
    #scale
    scaled_matrix = affine(x) %*% scale_matrix

    #calculate real world origin with min dim indices
    .starts = as.numeric(scidb_coordinate_start(x))
    names(.starts) = dimensions(x)
    origin.rw = .transformToWorld(affine(x),.starts[xdim(x)],.starts[ydim(x)])

    #create temporary matrix to calculate the new origin
    help.matrix = cbind(origin.rw,-(scaled_matrix[,2:3]))
    new.origin = help.matrix %*% c(1,.starts[xdim(x)],.starts[ydim(x)])

    #bind new origin and the scaling parameter
    out.affine = cbind(new.origin,scaled_matrix[,2:3])

    x@affine = out.affine
  }

  if (x@isTemporal && (grid[tdim(x) > 1])) {
    #adapt temporal reference
    tres = round(tres(x) * grid[tdim(x)])
    x@trs@tResolution = tres
  }
  return(x)
}

#x: scidbst object
#af: abbreviation of the aggregation function
.createExpression=function(x,af) {
  if (af %in% c("avg","sum","prod","max","min","stdev","var","count")) {
    return(paste(af,"(",scidb_attributes(x),")",sep="",collapse=", "))
  } else {
    return(af) # probably it is an expression term otherwise it will crash in scidb operation
  }
}

#' Regrid / Resample operator for scidbst objects
#'
#' This function changes the resolution of certain dimensions of the scidbst object. 'Regrid' is in this case the targeted scidb operation and reflects
#' a more general regrid approach. 'Resample' is based on the respective function of the raster package. The latter will simply perform a regrid on the
#' spatial dimensions. The grid parameter reflects the number of cells per dimension that are used as a block for resampling. As for the current development
#' of scidb, the aggregation functions are quite limited (min/max, average, sum, standard deviation) and do not support more elaborated function like bilinear
#' interpolation.
#' The resample method performs the same operation as regrid, but it is limited to change the spatial resoultion.
#'
#' @note For information on the aggregation statements in scidb have a look on the supported AFL functions
#' (\href{https://paradigm4.atlassian.net/wiki/display/ESD/SciDB+Aggregate+Operators+and+Functions}{SciDB 15.12 Doc}). To make the query formulation easier we allow also to
#' pass the aggregation function name of the AFL function as parameter "af", which will be used on all attributes.
#'
#' @name regrid,scidbst
#' @aliases resample,scidbst
#' @rdname resample-scidbst-methods
#'
#' @param x The scidbst object
#' @param y target object
#' @param af (optional) aggregation function applied to every attribute on the grid, or a quoted SciDB aggregation expression
#'
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' scibdst.obj = scidbst(array_name)
#' # 50x50 upscaling with scidb expression as aggregation function
#' regridded = regrid(scidbst.obj,c(50,50,1),"avg(attribute1)") #resample (upscaling 50x50 dimension units, here dimensions: y,x,time)
#' # 50x50 upscaling with sum aggregation function
#' regridded = regrid(scidbst.obj,c(50,50,1),sum) #resample (upscaling 50x50 dimension units, here dimensions: y,x,time)
#'
#' r = raster(scidbst.obj, nrows=300, ncols=200) # resample by setting the number of rows and columns of the target
#' resampled = resample(scidbst.obj,r,"avg(attribute1)")
#' resampled2 = resample(scidbst.obj,r,"sum") # aggregation function applied on all attributes
#' }
#'
#' @return scidbst object with new resolution information (adapted affine transformation)
#' @seealso \code{\link{regrid,scidb-method}} for original regrid function, \code{\link{resample}} for the original resample function
#' @export
setMethod("regrid", signature(x="scidbst"), .regrid.scidbst)

#' @rdname resample-scidbst-methods
#' @export
setMethod("resample", signature(x="scidbst", y="Raster"), function(x,y,af="avg") {
    grid = .prepareGrid(x,y)
    return(.regrid.scidbst(x=x,grid=grid,expr=af))
})

#' @rdname resample-scidbst-methods
#' @export
setMethod("resample", signature(x="scidbst", y="scidbst"), function(x,y,af="avg") {
  grid = .prepareGrid(x,y)


  return(.regrid.scidbst(x=x,grid=grid,expr=af))
})
