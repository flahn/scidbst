if (!isGeneric("subarray")) {
  setGeneric("subarray", function(x, ...) standardGeneric("subarray"))
}

subarray.scidbst = function (x, limits, between = FALSE) {
  ndim = length(dimensions(x))

  if (is.character(limits)) {
    limits = gsub("\\*","Inf",limits)
    limits = as.numeric(unlist(strsplit(limits,",")))
  }
  #TODO limits = scidbst object
  #TODO limits = raster or extent (equals raster::crop)
  #TODO limits = temporal extent

  if (length(limits) != 2*ndim) {
    stop("limits do not match dimension description [#limits != (2*#dims)]")
  }

  if (!between) { #subarray call
    if (x@isSpatial) {
      #image origin has been shifted towards limits
      xindex = which(dimensions(x)==getXDim(x)) #get position of "x" values
      xvals = c(limits[xindex],limits[xindex+ndim]) #min/max for xdim

      yindex = which(dimensions(x)==getYDim(x)) #position of "y" values
      yvals = c(limits[yindex],limits[yindex+ndim]) #min/max for ydim

      #calculate upper left coordinate (origin of image coordinate system)
      ul = .transformToWorld(x@affine,xvals[1],yvals[1]) #note: image coordinate system trans(i0) > trans(iEnd)

      lr = .transformToWorld(x@affine,xvals[2],yvals[2]) #to get an extent
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

  return(out)
}

#' Subarray function for scidbst object
#'
#' This function is based on the scidb subarray function. It will create a subset of the array based on the stated dimension limits. Based
#' on the 'between' flag in scidb will be used the AFL function 'subarray' or 'between'. When 'between is set \code{TRUE}, then the spatial
#' and/or temporal references will not be changed, because the dimension values are not recalculated. If 'subarray' is used in scidb, then
#' the dimension values are shifted to the new lower boundaries, which requires an adaption of the references (if necessary).
#'
#' @param x scidbst array object
#' @param limits vector of coordinate ranges or a character string (see Details)
#' @param between (logical) Whether or not the \code{between} function shall be used in scidb instead of \code{subarray}
#' @return scidbst array object with modified dimension references
#'
#' @details The \code{limits} parameter needs to be either a vector of numerics or characters or a character string. The vector needs to
#' have two times the number of dimension as elements in it. This also applies to the character string, which shall contain the limits
#' divided by commas (','). Examples: c(0,0,0,10,10,10); "0,0,0,10,10,10". Also unbounded dimension statements are supported. Use the
#' asterisk ('*') to define an unbounded dimension value.
#'
#' @note The package will provide this function as a generic S4 function. This means that the original subarray method from
#' the scidb package will not be recognized as S4 function. Use scidb::subarray to call the original method.
#'
#' @seealso \code{\link{subarray}}
#'
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' chicago = scidbst("chicago_sts")
#' expression = c(501,285,2,901,585,3) #minimum values, then maximum values for all dimensions
#' expression2 = c("501","285","2","901","585","*")
#' expression3 = "501,285,2,901,585,3"
#'
#' chicago_sub = subarray(chicago,expression,between=FALSE)
#' }
#'
#' @export
setMethod("subarray",signature(x="scidbst"), subarray.scidbst)
