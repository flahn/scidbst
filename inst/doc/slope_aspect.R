## ----connecting----------------------------------------------------------
library(scidbst)
library(rgdal)
source("/home/lahn/GitHub/scidbst/vignettes/assets/credentials.R")
scidbconnect(host=host,port=port,user=user,password=password,protocol="https",auth_type = "digest")

## ---- eval=FALSE---------------------------------------------------------
#  srtm.ethiopia = scidbst("srtm_ethiopia")
#  
#  srtm.sub = subarray(srtm.ethiopia,limits=extent(33.5,35,6.5,8))
#  srtm.sub = scidbsteval(srtm.sub,"temp_srtm_sub")
#  estimateFileSize(srtm.sub, unit="MB")
#  
#  srtm.sub@proxy = scidb::repart(as(srtm.sub,"scidb"),upper=c(1801,1801),chunk=c(451,451),overlap=c(1,1))
#  srtm.sub = scidbsteval(srtm.sub, "srtm_sub_reparted")
#  
#  scidbrm("temp_srtm_sub",force=TRUE)
#  
#  srtm.ethiopia.prep = transform(srtm.sub, dimy="double(y)",dimx="double(x)",band1 = "double(band1)")
#  srtm.ethiopia.prep = scidbsteval(srtm.ethiopia.prep,"srtm_sub_reparted_prep")

## ---- eval=FALSE---------------------------------------------------------
#  slopeAspect = function(x, ...) {
#    data = x
#    xdim = "dimx"
#    ydim = "dimy"
#    heightAttr = "band1"
#  
#    coordinates(data) <- c(xdim,ydim)
#    gridded(data) <- TRUE
#    data = as(data, "RasterStack")
#    x_cellsize = 90
#    y_cellsize = 90
#  
#    dx = focal(subset(x=data,subset=heightAttr,drop=TRUE), w=matrix(c(-1,-2,-1, 0, 0, 0, 1, 2, 1)/(8*x_cellsize),nr = 3, nc=3))
#    dy = focal(subset(x=data,subset=heightAttr,drop=TRUE), w=matrix(c(-1,-2,-1, 0, 0, 0, 1, 2, 1)/(8*y_cellsize),nr = 3, nc=3,byrow = TRUE))
#    slope = atan(sqrt(dx^2+dy^2)) *180/pi
#    names(slope) = c("slope")
#    aspect = atan2(dy,-1 * dx)* 180/pi
#    names(aspect) = c("aspect")
#    data = stack(list(data,slope,aspect))
#    out = cbind(dimy=coordinates(data)[,"y"],dimx=coordinates(data)[,"x"],as.data.frame(data))
#    out = na.omit(out)
#    out = as.data.frame(out)
#  
#  
#    return(out)
#  }

## ---- eval=FALSE---------------------------------------------------------
#  #<band1:int16> [y=0:1801,451,1,x=0:1801,451,1]
#  system.time({
#  sa.array <- r.apply(x=srtm.ethiopia.prep,
#                     f = slopeAspect,
#                     array = "srtm_eth_slo_asp",
#                     packages = c("raster","sp","rgdal"),
#                     aggregates=c(),
#                     output = list(dimy="double",dimx="double",band1="double",slope="double",aspect="double"),
#                     dim = list(dimy="y",dimx = "x"),
#                     dim.spec=list(y=list(min=0,max=1801,chunk=451,overlap=1),x=list(min=0,max=1801,chunk=451,overlap=1))
#                     )
#  }) #~ 23s

## ---- include=FALSE------------------------------------------------------
sa.array = scidbst("srtm_eth_slo_asp")

## ------------------------------------------------------------------------
brick = as(sa.array,"RasterBrick")
spplot(subset(brick,"band1"),main="Heights in m")
spplot(subset(brick,"slope"),main="Slope in degree")
spplot(subset(brick,"aspect"),main="Aspect in degree")

## ---- eval=FALSE---------------------------------------------------------
#  srtm = scidbst("SRTM_AFRICA")
#  # estimateFileSize(srtm) ~ 72GB
#  
#  schema(as(srtm,"scidb"))
#  # <band1:int16> [y=-6001:84001,2048,0,x=-1:90002,2048,0]
#  
#  srtm@proxy = scidb::repart(as(srtm,"scidb"),upper=c(84001,90002),chunk=c(2048,2048),overlap=c(1,1))
#  srtm.prep = transform(srtm,dimy="double(y)",dimx="double(x)",band1="double(band1)")
#  
#  srtm.prep = scidbsteval(srtm.prep,"srtm_africa_prep")
#  

## ----runAnalysisComplete, eval=FALSE-------------------------------------
#  system.time({
#  sa.array <- r.apply(x=srtm.prep,
#                     f = slopeAspect,
#                     array = "srtm_africa_slope_asp",
#                     packages = c("raster","sp","rgdal"),
#                     aggregates=c(),
#                     output = list(dimy="double",dimx="double",band1="double",slope="double",aspect="double"),
#                     dim = list(dimy="y",dimx = "x"),
#                     dim.spec=list(y=list(min=-6001,max=84001,chunk=2048,overlap=1),x=list(min=-1,max=90002,chunk=2048,overlap=1))
#                     )
#  })

## ---- eval=FALSE---------------------------------------------------------
#  temp = scidb("__temp_srtm_africa_prep_120395534")
#  rename = transform(temp,y="int64(expr_value_0)",x="int64(expr_value_1)",height="expr_value_2",slope="expr_value_3",aspect="expr_value_4")
#  selection = scidb::project(rename,c("y","x","height","slope","aspect"))
#  schema = "<height:double,slope:double,aspect:double> [y=-6001:84001,2048,0,x=-1:90002,2048,0]"
#  redim = scidb::redimension(selection,schema=schema)
#  
#  srtm.africa.sa = scidbeval(redim,name="srtm_africa_slope_aspect")
#  
#  copySRS(srtm.africa.sa,srtm.ethiopia)

