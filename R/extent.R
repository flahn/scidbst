#' @importFrom raster extent
#' @export
setMethod("extent",signature(x="scidbst"), function(x) {

  if (x@isSpatial || !is.null(x@extent)) {
    return(x@extent)
  } else {
    stop("There is no spatial extent for this SciDB array")
  }
})

.calculateExtentFromSciDBBounds = function(x) {
  #TRY: calculate the extent anew, then it is coherent with the proxy size
  dims = dimensions(x)
  index.start = as.numeric(gsub("\\*","Inf",scidb_coordinate_start(x)))
  index.end = as.numeric(gsub("\\*","Inf",scidb_coordinate_end(x)))
  x.pos = which(dims == xdim(x))
  y.pos = which(dims == ydim(x))
  xi.min = index.start[x.pos]
  yi.min = index.start[y.pos]
  xi.max = index.end[x.pos]
  yi.max = index.end[y.pos]

  ul = .transformToWorld(affine(x),xi.min,yi.min)
  lr = .transformToWorld(affine(x),xi.max,yi.max)
  # since the origin if the lower right pixel is in the top left corner the area is not in the extent!
  lr = lr + c(xres(x),yres(x))
  e = extent(c(ul[1],lr[1],lr[2],ul[2]))
  return(e)
}
