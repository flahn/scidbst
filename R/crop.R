#' @include scidbst-class.R
NULL

.crop.scidbst = function(x, y, snap='near', ..., between=TRUE) {
      ndim = length(dimensions(x))
      if (!x@isSpatial) {
        stop("The array does not have spatial dimensions to crop with a bounding box")
      }

      limits = c(scidb_coordinate_start(x),scidb_coordinate_end(x))

      # as in the raster package, try to get the extent of object y
      y <- try ( extent(y), silent=TRUE )
      if (class(y) == "try-error") {
        stop('Cannot get an Extent object from argument y')
      }
      validObject(y)

      e <- intersect(extent(x), extent(y))
      e <- alignExtent(e, x, snap=snap)

      out = .calculateDimIndices(x,e)

      xindex = which(dimensions(x)==getXDim(x)) #get position of "x" values
      limits[xindex] = xmin(out)
      limits[xindex+ndim] = xmax(out)

      yindex = which(dimensions(x)==getYDim(x)) #position of "y" values
      limits[yindex] = ymin(out)
      limits[yindex+ndim] = ymax(out)

      res = subarray(x=x,limits=limits,between=between) #use modified subarray version
      if (between) {
        res@extent = e
      }


      return(res)
}

#' Crop / spatial subset function
#'
#' This function is based on the similar function in the raster package. It creates a spatial subset of a scidbst array and
#' returns the subset scidbst object. Internally, this function will calculate the spatial indices of the extent object and
#' will use this as boundaries for the spatial dimensions. 'crop' will delegate the subset creation to the 'subarray' method.
#'
#' @rdname crop-scidbst-method
#' @name crop,scidbst
#' @note For proper use the new extent has to contain coordinates that have the same reference system as the scidbst object.
#' @param x scidbst object
#' @param y Extent object, or any object from which an Extent object can be extracted
#' @param snap Character. One of 'near', 'in', or 'out', for use with alignExtent
#' @param ...	Additional arguments as for writeRaster
#' @param between (logical) whether or not to use 'between' or 'subarray' as scidb operation
#'
#' @return scidbst object with refined spatial extent
#'
#' @seealso \code{\link[raster]{crop}} or \code{\link[scidbst]{subarray,scidbst}}
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' scidbst.obj = scidbst(array_name)
#' e = extent(scidbst.obj)
#' ymax(e) = ymin(e)+((ymax(e)-ymin(e))/4)
#' xmax(e) = xmin(e)+((xmax(e)-xmin(e))/4)
#' cropped = crop(scidbst.obj,e)
#' }
#' @export
setMethod('crop', signature(x='scidbst', y='ANY'),
          .crop.scidbst
)
