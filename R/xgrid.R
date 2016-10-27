if (!isGeneric("xgrid")) {
  setGeneric("xgrid",function(x,...) {
    standardGeneric("xgrid")
  })
}

# x,y scidbst objects
.prepareXGrid = function(x,y) {
  dims.x = dimensions(x)
  dims.y = dimensions(y)

  if (length(dims.x) != length(dims.y)) {
    stop("Error while preparing xgrid statement from scidbst arrays. Dimensionality differs.")
  }

  grid = rep(1,length(dims.x)) # use 1 because it will result into identical dimension space for non-changed dimensions

  if (x@isSpatial && y@isSpatial) {
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

  if (x@isTemporal && y@isTemporal) {
    tres.fac = round(.tres2seconds(x) / .tres2seconds(y))
    tpos = which(dims.x ==tdim(x))
    grid[tpos] = tres.fac
  }

  return(grid)
}

#' @export
setMethod("xgrid",signature(x="scidbst"),function(x,grid) {
  dims = dimensions(x)

  .scidb = as(x,"scidb")

  if (isClass(grid,"scidbst")) {
    grid = .prepareXGrid(x,grid)
  }

  grid = as.integer(grid)

  if (is.integer(grid)) {
    if (length(dims) == length(grid) || length(grid) == 1) {
      if (length(grid) == 1) {
        grid = rep(grid,length(dims))
      }
      .scidb = xgrid(.scidb,grid)

      #change resolutions
      if (x@isSpatial) {
        xpos = which(dims == xdim(x))
        ypos = which(dims == ydim(x))
        a = affine(x)
        a[1,2] = a[1,2] / grid[xpos]
        a[2,3] = a[2,3] / grid[ypos]
      }
      if (x@isTemporal) {
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
