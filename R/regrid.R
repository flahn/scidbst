#' @include scidbst-class.R
NULL

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
  sci.obj = regrid(sci.obj,grid, expr)

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

#' @export
setMethod("regrid", signature(x="scidbst"), .regrid.scidbst)

#' @export
setMethod("resample", signature(x="scidbst", y="Raster"), function(x,y,...) {
    grid = .prepareGrid(x,y)
    return(.regrid.scidbst(x=x,grid=grid,...))
})
