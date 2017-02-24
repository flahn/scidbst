if (!isGeneric("subarray")) {
  setGeneric("subarray", function(x, limits, ...) standardGeneric("subarray"))
}

subarray.scidbst = function (x, limits, between = FALSE) {
  proxy = x@proxy
  .dims = dimensions(x)
  ndim = length(.dims)

  if (is.character(limits)) {
    limits = gsub("\\*","Inf",limits)
    limits = as.numeric(unlist(strsplit(limits,",")))
  }

  if (length(limits) != 2*ndim) {
    stop("limits do not match dimension description [#limits != (2*#dims)]")
  }

  # adapt extent
  # if (!between) { #subarray call
    if (x@isSpatial) {
      #image origin has been shifted towards limits
      xindex = which(dimensions(x)==xdim(x)) #get position of "x" values
      xvals = c(limits[xindex],limits[xindex+ndim]) #min/max for xdim

      yindex = which(dimensions(x)==ydim(x)) #position of "y" values
      yvals = c(limits[yindex],limits[yindex+ndim]) #min/max for ydim

      #calculate upper left coordinate (origin of image coordinate system)
      ul = .transformToWorld(affine(x),xvals[1],yvals[1]) #note: image coordinate system trans(i0) > trans(iEnd)

      #TODO lower right + 1 to indices to cover the correct extent
      lr = .transformToWorld(affine(x),xvals[2],yvals[2]) #to get an extent
      newExtent = extent(ul[1],lr[1],lr[2],ul[2])
    }

    if (x@isTemporal) {
      tindex = which(dimensions(x)==tdim(x))
      tvals = c(limits[tindex],limits[tindex+ndim]) #min/max for xdim

      t0 = .calculatePOSIXfromIndex(x,tvals[1])
      if (tvals[2]==Inf) {
        tEnd = tmax(x)
      } else {
        tEnd = .calculatePOSIXfromIndex(x,tvals[2])
      }

    }
  # }

  scidb.obj = as(x,"scidb")
  scidb.obj = scidb::subarray(scidb.obj, limits, between)
  x@proxy = scidb.obj


  if (!between) {
    if (x@isSpatial) {
      #limits are the new coordinates in the old image reference
      # 1) adapt new extent
      x@extent = newExtent

      # 2) adapt transformation parameter (origin)
      # we can simply overwrite the origin, since the array dimension indices are also recalculated
      x@affine[1,1] = ul[1]
      x@affine[2,1] = ul[2]
    }

    if (x@isTemporal) {
      x@trs@t0 = t0

      x@tExtent@min = t0
      x@tExtent@max = tEnd
    }
  } else {#else leave as is since we do not change dimension values <- wrong we dont mess with TRS or SRS but still with the extent
    if (x@isSpatial) {
      x@extent = newExtent
    }
    if (x@isTemporal) {
      x@tExtent@min = t0
      x@tExtent@max = tEnd
    }
  }
  return(x)
}

.createSpLimitExpr = function(x, limits, limitExpr) {
  xdim = xdim(x)
  ydim = ydim(x)
  dims = dimensions(x)
  bounds = scidb_coordinate_bounds(x)

  if (missing("limitExpr")) limitExpr = c(bounds$start,bounds$end)
  xminPos = which(dims==xdim)
  xmaxPos = xminPos + length(dims)

  yminPos = which(dims==ydim)
  ymaxPos = yminPos + length(dims)

  e = extent(limits)

  if (xmin(e) < xmin(x)) xmin(e) = xmin(x)
  if (ymin(e) < ymin(x)) ymin(e) = ymin(x)
  if (xmax(e) > xmax(x)) xmax(e) = xmax(x)
  if (ymax(e) > ymax(x)) ymax(e) = ymax(x)

  newe = .calculateDimIndices(x,e)
  limitExpr[xminPos] = xmin(newe)
  limitExpr[yminPos] = ymin(newe)
  limitExpr[xmaxPos] = xmax(newe)
  limitExpr[ymaxPos] = ymax(newe)

  return(limitExpr)
}

.createTLimitExpr = function(x,limits, limitExpr) {
  tdim = tdim(x)
  dims = dimensions(x)
  bounds = scidb_coordinate_bounds(x)

  if (missing(limitExpr)) limitExpr = c(bounds$start,bounds$end)

  tminPos = which(dims==tdim)
  tmaxPos = tminPos + length(dims)

  tmin = .calcTDimIndex(x,tmin(limits))
  tmax = .calcTDimIndex(x,tmax(limits))

  if (tmax > tmax(x)) tmax = tmax(x)
  if (tmin > tmin(x)) tmin = tmin(x)


  limitExpr[tminPos] = tmin
  limitExpr[tmaxPos] = tmax

  return(limitExpr)
}

.subarray.TemporalExtent = function(x,limits,between=FALSE) {
  if (!x@isTemporal) {
    stop("Cannot set limit for time dimension. Array has no such dimension.")
  }
  limitExpr = .createTLimitExpr(x,limits)

  # now call subarray again with a list of indices
  return(subarray(x,limits=limitExpr,between=between))
}

.subarray.with.scidbst = function(x,limits,between=FALSE) {
  if (!x@isSpatial) {
    stop("Cannot set limit for spatial dimensions. Array has no such dimensions.")
  }
  if (!x@isTemporal) {
    stop("Cannot set limit for time dimension. Array has no such dimension.")
  }
  limitExpr = .createSpLimitExpr(x,limits)
  limitExpr = .createTLimitExpr(x,limits,limitExpr=limitExpr)
  return(subarray(x,limits=limitExpr,between=between))
}

#' Subarray function for scidbst object
#'
#' This function is based on the scidb subarray function. It will create a subset of the array based on the stated dimension limits. Based
#' on the 'between' parameter either the 'subarray' or the 'between' function will be used in SciDBs AFL query. When 'between is set \code{TRUE}, then the spatial
#' and/or temporal references will not be changed, because the dimension values are not recalculated. If 'subarray' is used in scidb, then
#' the dimension values are shifted to the new lower boundaries, which requires an adaption of the references (if necessary). In either case the resulting
#' scidbst object will have a modified dimensional extent.
#'
#' @rdname subarray-scidbst-method
#' @name subarray,scidbst
#' @param x scidbst array object
#' @param limits vector of coordinate ranges or a character string (see Details) or \code{\link[raster]{Extent}} or \code{\link[scidbst]{TemporalExtent}} objects
#' @param between (logical) Whether or not the \code{between} function shall be used in scidb instead of \code{subarray}
#' @return modified scidbst array object
#'
#' @details Like in the original \code{subarray} method \code{limits} parameter needs to be either a vector of numerics or characters or a character string. The vector needs to
#' have two times the number of dimension as elements in it. This also applies to the character string, which shall contain the limits
#' divided by commas (','). Examples: c(0,0,0,10,10,10); "0,0,0,10,10,10". Also unbounded dimension statements are supported. Use the
#' asterisk ('*') to define an unbounded dimension value.
#' The scidbst package also allows Extent and TemporalExtent objects as limits. Missing values regarding the spatial extent or temporal extent
#' are kept as is.
#'
#' @note The package will provide this function as a generic S4 function. This means that the original subarray method from
#' the scidb package will not be recognized as S4 function. Use scidb::subarray to call the original method.
#'
#' @seealso \code{\link[scidb]{subarray}}, \code{\link[scidbst]{crop,scidbst}}
#'
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' chicago = scidbst("chicago_sts")
#'
#' # Using character strings and vector
#' chicago_sub1 = subarray(x=chicago,c(501,285,2,901,585,3),between=FALSE) #minimum values, then maximum values for all dimensions
#' chicago_sub2 = subarray(x=chicago,limits=c("501","285","2","901","585","*"))
#' chicago_sub3 = subarray(x=chicago,limits="501,285,2,901,585,3")
#'
#' # Using an extent object
#' extent = c(448000,451000,4635000,4640000)
#' chicago_sub4 = subarray(x=chicago,limits=extent,between=FALSE)
#'
#' # Using a Temporal extent
#' te = textent(as.POSIXct("2016-05-03"),as.POSIXct("2016-05-05"))
#' chicago_sub5 = subarray(x=chicago,limits=te,between=FALSE)
#' }
#'
#' @export
setMethod("subarray",signature(x="scidbst",limits="numeric"), subarray.scidbst)

#' @name subarray,scidbst
#' @rdname subarray-scidbst-method
#' @export
setMethod("subarray",signature(x="scidbst",limits="character"), subarray.scidbst)

#' @name subarray,scidbst
#' @rdname subarray-scidbst-method
#' @export
setMethod("subarray",signature(x="scidbst",limits="Extent"),function(x, limits, between = FALSE) {
  .crop.scidbst(x,limits,between=between)
})

#' @name subarray,scidbst
#' @rdname subarray-scidbst-method
#' @export
setMethod("subarray",signature(x="scidbst",limits="TemporalExtent"), .subarray.TemporalExtent)

#' @name subarray,scidbst
#' @rdname subarray-scidbst-method
#' @export
setMethod("subarray",signature(x="scidbst",limits="scidbst"), .subarray.with.scidbst)


#' @name subarray,scidb
#' @rdname subarray-scidbst-method
#' @export
setMethod("subarray",signature(x="scidb",limits="numeric"), function(x,limits,between=FALSE) {
  scidb::subarray(x=x,limits=limits,between=between)
})
