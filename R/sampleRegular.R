#' @include scidbst-class.R
#' @include readAll.R
#' @include getValues.R
NULL

#' Regular Sample
#'
#' Take a systematic sample from a scidbst object.
#'
#' @note This function is mostly applied when using 'spplot' in order to reduce the resolution for pure visualization purposes
#' @inheritParams raster::sampleRegular
#' @export
# copied from raster with small changes
# changed "names(outras) <- names(x)" to "names(outras) <- scidb_attributes(x)" to keep the correct attributes for raster functions
setMethod('sampleRegular', signature(x='scidbst'),
          function( x, size, ext=NULL, cells=FALSE, xy=FALSE, asRaster=FALSE, sp=FALSE, useGDAL=FALSE, ...) {
            stopifnot(hasValues(x))
            size <- round(size)
            stopifnot(size > 0)
            nl <- nlayers(x)
            rotated <- rotated(x)

            if (is.null(ext)) {
              if (size >= ncell(x)) {
                if (asRaster) {
                  if (!rotated) {
                    return(x)
                  }
                } else {
                  if (cells) {
                    return(cbind(1:ncell(x), values(x)))
                  } else {
                    return(values(x))
                  }
                }
              }
              rcut <- raster(x)
              firstrow <- 1
              lastrow <- nrow(rcut)
              firstcol <- 1
              lastcol <- ncol(rcut)

            } else {

              rcut <- crop(raster(x), ext)
              ext <- extent(rcut)
              if (size >= ncell(rcut)) {
                x <- crop(x, ext)
                if (asRaster) {
                  return(x)
                } else {
                  return(getValues(x))
                }
              }
              yr <- yres(rcut)
              xr <- xres(rcut)
              firstrow <- rowFromY(x, ext@ymax-0.5 *yr)
              lastrow <- rowFromY(x, ext@ymin+0.5*yr)
              firstcol <- colFromX(x, ext@xmin+0.5*xr)
              lastcol <- colFromX(x, ext@xmax-0.5*xr)
            }


            Y <- X <- sqrt(ncell(rcut)/size)
            nr <- max(1, floor((lastrow - firstrow + 1) / Y))
            nc <- max(1, floor((lastcol - firstcol + 1) / X))

            rows <- (lastrow - firstrow + 1)/nr * 1:nr + firstrow - 1
            rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
            cols <- (lastcol - firstcol + 1)/nc * 1:nc  + firstcol - 1
            cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)

            cols <- unique(round(cols))
            rows <- unique(round(rows))
            cols <- cols[cols>0]
            rows <- rows[rows>0]
            nr <- length(rows)
            nc <- length(cols)


            if (fromDisk(x)) {

              if (cells | any(rotated | raster:::.driver(x, FALSE) != 'gdal')) {
                useGDAL <- FALSE
              }
              if (useGDAL) {
                print("using GDAL")
                offs <- c(firstrow,firstcol)-1
                reg <- c(nrow(rcut), ncol(rcut))-1

                if (inherits(x, 'RasterStack')) {

                  v <- matrix(NA, ncol=nl, nrow=prod(nr, nc))

                  for (i in 1:nl) {
                    xx <- x[[i]]
                    con <- GDAL.open(xx@file@name, silent=TRUE)
                    band <- bandnr(xx)
                    vv <- getRasterData(con, band=band, offset=offs, region.dim=reg, output.dim=c(nr, nc))
                    closeDataset(con)
                    if (xx@data@gain != 1 | xx@data@offset != 0) {
                      vv <- vv * xx@data@gain + xx@data@offset
                    }
                    if (xx@file@nodatavalue < 0) {
                      vv[vv <= xx@file@nodatavalue] <- NA
                    } else {
                      vv[vv == xx@file@nodatavalue] <- NA
                    }
                    v[, i] <- vv
                  }

                } else {
                  if (nl == 1) {
                    band <- bandnr(x)
                  } else {
                    band <- NULL
                  }
                  con <- GDAL.open(x@file@name, silent=TRUE)
                  v <- getRasterData(con, band=band, offset=offs, region.dim=reg, output.dim=c(nr, nc))
                  closeDataset(con)

                  if (x@data@gain != 1 | x@data@offset != 0) {
                    v <- v * x@data@gain + x@data@offset
                  }

                  if (raster:::.naChanged(x)) {
                    if (x@file@nodatavalue < 0) {
                      v[v <= x@file@nodatavalue] <- NA
                    } else {
                      v[v == x@file@nodatavalue] <- NA
                    }
                  }
                }

                if (asRaster) {
                  if (is.null(ext))  {
                    outras <- raster(x)
                  } else {
                    outras <- raster(ext)
                  }
                  nrow(outras) <- nr
                  ncol(outras) <- nc
                  if (nl > 1) {
                    outras <- brick(outras, nl=nl)
                    outras <- setValues(outras, v)
                  } else {
                    outras <- setValues(outras, as.vector(v))
                  }
                  names(outras) <- scidb_attributes(x)
                  if (any(is.factor(x))) {
                    levels(outras) <- levels(x)
                  }
                  return(outras)

                } else {
                  if (cells) {
                    warning("'cells=TRUE' is ignored when 'useGDAL=TRUE'")
                  }
                  if (xy) {
                    warning("'xy=TRUE' is ignored when 'useGDAL=TRUE'")
                  }
                  if (sp) {
                    warning("'sp=TRUE' is ignored when 'useGDAL=TRUE'")
                  }
                  return( as.vector(v) )
                }
              }
            }

            # calculates the number of cells
            cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))


            if (asRaster) {
              if (rotated) {
                if (is.null(ext)) {
                  outras <- raster(extent(x))
                } else {
                  outras <- raster(ext)
                }
                ncol(outras) <- nc
                nrow(outras) <- nr
                xy <- xyFromCell(outras, 1:ncell(outras))
                m <- raster:::.xyValues(x, xy)

              } else {
                m <- raster:::.cellValues(x, cell)

                if (is.null(ext))  {
                  outras <- raster(x)
                } else {
                  outras <- raster(ext)
                }
                nrow(outras) <- nr
                ncol(outras) <- nc

              }
              if (nl > 1) {
                outras <- brick(outras, nl=nl)
              }
              outras <- setValues(outras, m)
              names(outras) <- scidb_attributes(x)
              if (any(is.factor(x))) {
                levels(outras) <- levels(x)
              }
              return(outras)

            } else {

              m <- NULL
              if (xy) {
                m <- xyFromCell(x, cell)
              }
              if (cells) {
                m <- cbind(m, cell=cell)
              }
              m <- cbind(m, raster:::.cellValues(x, cell))

              if (sp) {
                m <- SpatialPointsDataFrame(xyFromCell(x, cell), data.frame(m), proj4string=projection(x, asText=FALSE))
              }

              return(m)
            }
          }
)
