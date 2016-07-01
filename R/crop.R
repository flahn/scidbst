#' @include scidbst-class.R
NULL

.crop.scidbst = function(x, y, snap='near', ...) {
      if (length(dimnames(x)) > 2 ) {
        stop("More than two dimensions")
      }

      # as in the raster package, try to get the extent of object y
      y <- try ( extent(y), silent=TRUE )
      if (class(y) == "try-error") {
        stop('Cannot get an Extent object from argument y')
      }
      validObject(y)

      e <- intersect(extent(x), extent(y))
      e <- alignExtent(e, x, snap=snap)

      out = .calculateDimIndices(x,e)

      limits = as.matrix(out)

      res = subarray(x=x,limits=limits[dimensions(x),],between=TRUE)
      res = .scidbst_class(res)
      res = .cpMetadata(x,res) #first copy all, then adapt

      res@extent = e
      nrow(res) = (ymax(out) - ymin(out))+1
      ncol(res) = (xmax(out) - xmin(out))+1
      # +1 because origin in scidb is 0,0

      return(res)
}

#' crop function
#'
#' This function creates a spatial subset of a scidbst array and returns the subset scidbst object.
#'
#' @param x scidbst object
#' @param y Extent object, or any object from which an Extent object can be extracted
#' @param snap Character. One of 'near', 'in', or 'out', for use with alignExtent
#' @param ...	Additional arguments as for writeRaster
#'
#' @return scidbst object with refined spatial extent
#' @export
setMethod('crop', signature(x='scidbst', y='ANY'),
          .crop.scidbst
)