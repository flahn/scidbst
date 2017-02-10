#' @include scidbst-class.R
NULL

.crop.scidbst = function(x, y, snap='near', ..., between=TRUE) {
      proxy = x@proxy
      .dims = dimensions(x)
      ndim = length(.dims)
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
      # calculate dim indices for using the affine transformation of x

      rasterStruct = raster(x=extent(x),crs=crs(x),resolution=c(xres(x),yres(x)))
      # xres(rasterStruct) = xres(x)
      # yres(rasterStruct) = yres(x)
      e <- alignExtent(e, rasterStruct, snap=snap)

      # make list of dimension indices and use subarray again
      out = .calculateDimIndices(x,e)

      xindex = which(.dims==xdim(x)) #get position of "x" values
      limits[xindex] = xmin(out)
      limits[xindex+ndim] = xmax(out)

      yindex = which(.dims==ydim(x)) #position of "y" values
      limits[yindex] = ymin(out)
      limits[yindex+ndim] = ymax(out)

      # create a subset by passing on the numeric limits to 'subarray'
      res = subarray(x=x,limits=limits,between=between)
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
#' @importFrom raster intersect alignExtent raster extent
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
setMethod("crop", signature(x="scidbst", y="ANY"),
          .crop.scidbst
)
