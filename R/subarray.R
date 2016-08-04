# if (!isGeneric("subarray")) {
#   setGeneric("subarray", function(x, ...) standardGeneric("subarray"))
# }

#' @export
subarray.scidbst = function (x, limits, between = FALSE) {
  ndim = length(dimensions(s))

  if (is.character(limits)) {
    limits = gsub("\\*","Inf",limits)
    limits = as.numeric(unlist(strsplit(limits,",")))
  }
  #TODO limits = scidbst object

  if (length(limits) != 2*ndim) stop("limits do not match dimension description [#limits != (2*#dims)]")
  if (!between) { #subarray call
    if (x@isSpatial) {
      #image origin has been shifted towards limits
      xindex = which(dimensions(x)==getXDim(x))
      xvals = c(limits[xindex],limits[xindex+ndim]) #min/max for xdim

      yindex = which(dimensions(x)==getYDim(y))
      yvals = c(limits[yindex],limits[yindex+ndim]) #min/max for ydim

      #calculate upper left coordinate (origin of image coordinate system)
      ul = .transformToWorld(x@affine,xvals[1],yvals[2])

      lr = .transformToWorld(x@affine,xvals[2],yvals[1]) #to get an extent
      newExtent = extent(ul[1],lr[1],lr[2],ul[2])
    }

    if (x@isTemporal) {
      tindex = which(dimensions(x)==getTDim(x))
      tvals = c(limits[tindex],limits[tindex+ndim]) #min/max for xdim

      t0 = .calculatePOSIXfromIndex(x,tvals[1])
      if (tvals[2]==Inf) {
        tEnd = x@tExtent[["max"]]
      } else {
        tEnd = .calculatePOSIXfromIndex(x,tvals[2])
      }

    }
  }

  scidbst.obj = x
  scidb.obj = .toScidb(scidbst.obj)
  scidb.obj = scidb::subarray(scidb.obj, limits, between)
  out = .scidbst_class(scidb.obj)
  out = .cpMetadata(scidbst.obj,out)


  if (!between) {
    if (x@isSpatial) {
      #limits are the new coordinates in the old image reference
      # 1) adapt new extent
      out@extent = newExtent

      # 2) adapt transformation parameter (origin)
      # we can simply overwrite the origin, since the array dimension indices are also recalculated
      out@affine[1,1] = ul[1]
      out@affine[2,1] = ul[2]
    }

    if (x@isTemporal) {
      out@startTime = t0
      out@tExtent[["min"]] = t0
      out@tExtent[["max"]] = tEnd
    }
  } #else leave as is since we do not change dimension values




  #.transformToWorld
  #.calculateDimIndices

  return(out)
}
