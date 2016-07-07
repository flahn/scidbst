#' @include scidbst-class.R
NULL

# prepares the grid statement for the scidb call, which reflects the new resolution in dimension
# indice units
.prepareGrid = function (x, y) {
      grid = rep(1,length(dimensions(x)))
      names(grid) = dimensions(x)
      if (!missing(y)) {
          oldnx = ncol(x)
          oldny = nrow(x)
          newnx = ncol(y)
          newny = nrow(y)
          if (newny > oldny && newnx > oldnx) {
            stop("Error: Cannot resample the array, because the output array has at least partially a higher resolution than the input")
          }
          #resample way
          e = extent(y)
          xsize = round(oldnx/newnx)
          ysize = round(oldny/newny)

          #replace spatial grid sizes
          if(x@isSpatial) {
              grid[getXDim(x)] = xsize
              grid[getYDim(x)] = ysize
          }
      }
      return(grid)
}

.regrid.scidbst = function(x, grid, expr) {
  names(grid) = dimensions(x)

  #feed parameter to scidb::regrid
  sci.obj = .toScidb(x)
  sci.obj = regrid(x=sci.obj,grid=grid, expr=expr)


  #copy and adapt metadata
  out = .scidbst_class(sci.obj)
  out = .cpMetadata(x,out)

  if (x@isSpatial && (grid[getXDim(x)] > 1 || grid[getYDim(x)] > 1)) {
    #adapt affine transformation
    out@affine[1,2] = out@affine[1,2] * grid[getXDim(x)]
    out@affine[2,3] = out@affine[2,3] * (-1) *grid[getYDim(x)]
  }

  if (x@isTemporal && (grid[getTDim(x) > 1])) {
    #adapt temporal reference
    tres = round(out@tResolution * grid[getTDim(x)])
    out@tResolution = tres
  }
  return(out)
}

#' Regrid / Resample operator for scidbst objects
#'
#' This function changes the resolution of certain dimensions of the scidbst object. 'Regrid' is in this case the targeted scidb operation and reflects
#' a more general regrid approach. 'Resample' is based on the respective function of the raster package. The latter will simply perform a regrid on the
#' spatial dimensions. The grid parameter reflects the number of cells per dimension that are used as a block for resampling. As for the current development
#' of scidb, the aggregation functions are quite limited (min/max, average, sum, standard deviation) and do not support more elaborated function like bilinear
#' interpolation.
#'
#' @note For information on the aggregation statements in scidb have a look on the supported AFL functions (\url{http://paradigm4.com/HTMLmanual/13.3/scidb_ug/ch12.html})
#'
#' @rdname resample-scidbst-methods
#'
#' @param x The scidbst object
#' @param grid a vector of grid sizes having the same length as dimensions(x)
#' @param expr (optional) aggregation function applied to every attribute on the grid, or a quoted SciDB aggregation expression
#' @param y raster object
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
#' }
#'
#' @return scidbst object with new resolution information (adapted affine transformation)
#' @seealso \code{\link{regrid,scidb-method}} for original regrid function, \code{\link{resample}} for the original resample function
#' @export
setMethod("regrid", signature(x="scidbst"), .regrid.scidbst)

#' @rdname resample-scidbst-methods
#' @export
setMethod("resample", signature(x="scidbst", y="Raster"), function(x,y,expr) {
    grid = .prepareGrid(x,y)
    return(.regrid.scidbst(x=x,grid=grid,expr))
})
