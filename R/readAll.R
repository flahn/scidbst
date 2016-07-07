#' @include scidbst-class.R
NULL

.materializeSCIDBValues = function(object, startrow, nrows=1, startcol=1, ncols=ncol(object)) {
  if (length(dimensions(object))>2) {
    stop("Array has more than two dimensions to fetch data in a raster format")
    #TODO if time is referenced allow download of multiple timesteps, if needed
  }
  offs <- c((startrow - 1), (startcol - 1))
  reg <- c(nrows, ncols)
  result <- matrix(nrow = (ncol(object)) * (nrow(object)), ncol = nlayers(object))

  extent = as.matrix(.calculateDimIndices(object,extent(object)))

  cat("Downloading data...\n")
  .data = iquery(object@name,return=T)

  dims = dimensions(object)
  ndims = length(dims)

  if (nrow(.data) == 0) { #scidb does not return data. Stop here
    stop("Image is empty.")
  }

  for (b in 1:object@data@nlayers) {
    lname = object@data@names[b]

    if (!all(is.na(.data[,lname]))) {

      if (ndims == 2) {
        ydim = getYDim(object)
        xdim = getXDim(object)

        tmp = matrix(nrow=(nrow(object)),ncol=(ncol(object)))
        m = .data[order(.data[,ydim],.data[,xdim]),]
        start_y = min(.data[,ydim])
        start_x = min(.data[,xdim])
        #shift x coordinates 0->1 and remove offset
        m[,xdim]=m[,xdim]+1-start_x
        #same for y
        m[,ydim]=m[,ydim]+1-start_y

        tmp2 = matrix(m[,lname],nrow=length(unique(m[,ydim])),ncol=length(unique(m[,xdim])),byrow=T)

        tmp[unique(m[,ydim]),unique(m[,xdim])] = tmp2
        #restructure the matrix to a one dimensional vector
        restruct = as.vector(t(tmp))
        result[,b] = restruct
      } else { #number of dimensions is 1
        tdim = getTDim(object)

        tmp = matrix(nrow=1,ncol=(max(.data[,tdim])-min(.data[,tdim])+1))
        m = .data[order(.data[,tdim]),]
        start_t = min(.data[,tdim])
        #shift t coordinates 0->1 and remove offset
        m[,tdim]=m[,tdim]+1-start_t

        tmp2 = matrix(m[,lname],nrow=1,ncol=length(unique(m[,tdim])),byrow=T)

        tmp[1,unique(m[,tdim])] = tmp2
        #restructure the matrix to a one dimensional vector
        restruct = as.vector(t(tmp))


        result[,b] = restruct
      }
    }
  }
  colnames(result) = scidb_attributes(object)
  return(result)
}

#' readAll
#'
#' Like \code{raster::getValues()} this function is called to retrieve the values from the scidb database and store them in memory. It
#' differs from getValues in the fact that the scidbst object will be manipulated and returned back.
#'
#' @note Currently the number of dimensions for this array needs to be 1 or 2, in order to limit the data download traffic.
#'
#' @param object scidbst object
#' @return the modified scidbst object with values in memory (stored as a RasterBrick)
#'
#' @examples
#' \dontrun{
#' scidbconnect(...)
#' scidbst.obj = scidbst(array_name) # e.g. a spatio-temporal object with dimension (y,x,t)
#' sliced = slice(scidbst.obj, "t",0)
#' spplot(sliced) #note that the dimensionality of scidbst.obj needs to be 2 or less
#' }
#' @export
setMethod('readAll', signature(object='scidbst'),
          function(object){
            # if (! object@data@fromdisk)  {
            #   stop('cannot read values; there is no file associated with this scidbst raster inheriting thing')
            # }
            object@data@inmemory <- TRUE
            object@data@values <- .materializeSCIDBValues(object, 1, object@nrows)
            w <- getOption('warn')
            on.exit(options('warn' = w))
            options('warn'=-1)
            rge <- apply(object@data@values, 2, FUN=function(x){ range(x, na.rm=TRUE) } )
            object@data@min <- as.vector(rge[1,])
            object@data@max <- as.vector(rge[2,])
            object@data@haveminmax <- TRUE
            return(object)
          }
)
