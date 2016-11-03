if (!isGeneric("xgrid")) {
  # same statement as in scidb
  setGeneric("xgrid",function(x, grid, expr) {
    standardGeneric("xgrid")
  })
}

# x,y scidbst objects
.prepareXGrid = function(x,y,type) {
  #make sure that the resolutions are correct res(x) > res(y) for dims(type)
  if (xres(x) < xres(y) && yres(x) < yres(y)) {
    warning("Target array has a lower resolution than the source array. Switching source and target to continue.")
    tmp = x
    x = y
    y = tmp
  }

  dims.x = dimensions(x)
  dims.y = dimensions(y)

  if (length(dims.x) != length(dims.y)) {
    stop("Error while preparing xgrid statement from scidbst arrays. Dimensionality differs.")
  }

  grid = rep(1,length(dims.x)) # use 1 because it will result into identical dimension space for non-changed dimensions

  if (x@isSpatial && y@isSpatial && type !="T") {
    if (!.equalSRS(x,y)) {
      stop("Error while preparing xgrid statement. SRS do not match.")
    }
    xres.fac = floor(xres(x)/xres(y))
    yres.fac = floor(yres(x)/yres(y))

    xpos = which(dims.x == xdim(x))
    ypos = which(dims.x == ydim(x))
    grid[xpos] = xres.fac
    grid[ypos] = yres.fac
  }

  if (x@isTemporal && y@isTemporal && type != "S") {
    tres.fac = round(.tres2seconds(x) / .tres2seconds(y))
    tpos = which(dims.x ==tdim(x))
    grid[tpos] = tres.fac
  }

  return(grid)
}

#' Enlarge function
#'
#' This function can be used to enlarge the dimensional resolution of an array, e.g. changing the spatial resolution
#' from a low resolution to a higher one. It calls the same called function in SciDB to execute the operation. There
#' will be no interpolation of new values. The "old" cell value will be repeated the amount of times stated with the
#' 'grid' statement.
#'
#' @note It is important to mention, that the dimensional scaling factor needs to be a integer value, other values will
#' be rounded to the nearest integer. This is due to the fact that SciDB operates with int64 values as dimensional values.
#'
#' @param x scidbst array
#' @param grid vector of integers which correspond to each dimension or a scidbst array
#' @param expr character specifiying the type (one of c("S","T","ST")) that determins the dimensions to use for scaling
#' @return a modified version of x (scidbst array) with changed dimensionality
#'
#' @export
setMethod("xgrid",signature(x="scidbst"),function(x,grid,expr="S") {

  if (!expr %in% c("S","T","ST")) {
    stop("Cannot relate expression parameter to a known equalization type.")
  }

  dims = dimensions(x)

  .scidb = as(x,"scidb")

  if (class(grid) == "scidbst") {
    grid = .prepareXGrid(x,grid,expr)
  }

  grid = as.integer(grid)

  if (is.integer(grid)) {
    if (length(dims) == length(grid) || length(grid) == 1) {
      if (length(grid) == 1) {
        grid = rep(grid,length(dims))
      }
      .scidb = xgrid(.scidb,grid)

      #change resolutions
      if (x@isSpatial && expr!="T") {
        xpos = which(dims == xdim(x))
        ypos = which(dims == ydim(x))
        a = affine(x)
        a[1,2] = a[1,2] / grid[xpos]
        a[2,3] = a[2,3] / grid[ypos]
        x@affine = a
      }
      if (x@isTemporal && expr != "S") {
        tpos = which(dims == tdim(x))
        newRes = x@trs@tResolution / grid[tpos]
        while (newRes < 1 && tunit(x) != "secs") {
          units = c("secs","mins","hours","days","weeks")
          if (tunit(x) != "secs") {
            newUnit = units[which(units == tunit(x))-1]
            if (newUnit == "days") {
              newRes = newRes * 7
            } else if (newUnit=="hours") {
              newRes = newRes * 24
            } else if (newUnit=="mins") {
              newRes = newRes * 60
            } else if (newUnit=="secs") {
              newRes = newRes * 60
            }

            x@trs@tUnit = newUnit
          }
        }
        #lets round... not exact but necessary for the ISO8601 interval representation
        x@trs@tResolution = round(newRes)
      }

      x@proxy = .scidb
      return(x)
    }
  } else {
    stop("Wrong expression for parameter 'grid'. Use a scidbst array or a integer vector.")
  }

})
