## ---- include=FALSE------------------------------------------------------
library(scidb)
library(raster)
library(zoo)
library(xts)
library(knitr)
library(scidbst)

opts_chunk$set(collapse = T, comment = "#>")
opts_chunk$set(cache.extra = list(R.version, sessionInfo(), format(Sys.Date(), '%Y-%m')))
opts_chunk$set(fig.width=10, fig.height=8)
outputFolder = "/assests/"

## ----loadCredentials, include=FALSE--------------------------------------
source("/home/lahn/GitHub/scidbst/vignettes/assets/credentials.R")

## ----connect-------------------------------------------------------------
scidbconnect(host=host,port=port,user=user,password=password,protocol="https",auth_type = "digest")
array.list = scidbst.ls()
array.list

## ---- cache=1------------------------------------------------------------
l7_ethiopia = scidbst("L7_SW_ETHOPIA")
l7_ethiopia
extent(l7_ethiopia)
crs(l7_ethiopia)
textent(l7_ethiopia)
trs(l7_ethiopia)

## ----ehtiopiaSubset, cache=1---------------------------------------------
subset.extent = extent(35,36.5,6,8.5)
ethiopia.subset = crop(l7_ethiopia,subset.extent)
extent(ethiopia.subset)

## ----ethiopiaSlice, cache=1----------------------------------------------
ethiopia.subset = slice(ethiopia.subset,"t","2003-07-21")

## ----ethiopiaPlot, cache=1-----------------------------------------------
ethiopia.brick = as(ethiopia.subset,"RasterBrick")
spplot(ethiopia.brick)

## ----ndviNames, cache=1--------------------------------------------------
ls.brazil.name = "LS7_BRAZIL"
regrid.name = "LS7_BRAZIL_REGRID"
ndvi.name = "LS7_BRAZIL_REGRID_NDVI"

## ----ndviDeletes, include=FALSE------------------------------------------
if (any(array.list$name == regrid.name)) {
  scidbrm(regrid.name,force=TRUE)
}

if (any(array.list$name == ndvi.name)) {
  scidbrm(ndvi.name, force=TRUE)
}

## ----ndviResolution, cache=1---------------------------------------------
ls7_brazil = scidbst(ls.brazil.name)
xres(ls7_brazil)
yres(ls7_brazil)

## ----ndviRegrid, cache=1-------------------------------------------------
ls7_brazil_regrid = regrid(ls7_brazil,c(10,10,1),"avg(band1),avg(band3),avg(band4),avg(band5),avg(band8)")
xres(ls7_brazil_regrid)
yres(ls7_brazil_regrid)

## ----ndviEval, cache=1---------------------------------------------------
ls7_brazil_regrid = scidbsteval(ls7_brazil_regrid,regrid.name)

## ----ndviCalculation, cache=1--------------------------------------------
ls7_calc = transform(ls7_brazil_regrid, ndvi = "(band4_avg - band3_avg) / (band4_avg + band3_avg)", mdvi = "(band8_avg - band3_avg) / (band8_avg + band3_avg)")
ls7_calc = project(ls7_calc,c("ndvi","mdvi"))
ls7_calc = scidbsteval(ls7_calc,ndvi.name)

## ----ndviPlot, cache=1---------------------------------------------------
ndvi = slice(project(ls7_calc,c("ndvi")),"t","2001-088")
mdvi = slice(project(ls7_calc,c("mdvi")),"t","2001-088")
spplot(ndvi,main="NDVI calculation")
spplot(mdvi,main="MDVI calculation")

## ----cloudMask, cache=1--------------------------------------------------
ls7_brazil = scidbst("LS7_BRAZIL")
sliced.regridded = regrid(slice(ls7_brazil,"t",0),c(10,10),"avg(band1),avg(band2),avg(band3)")
brazil.brick = as(sliced.regridded,"RasterBrick")
plotRGB(brazil.brick,r=3,g=2,b=1)

transformed = transform(sliced.regridded,cloud = "iif(band1_avg >= 252 and band2_avg >= 252 and band3_avg >= 252, 1 , 0)")
p2 = project(transformed,c("cloud"))
clouds = subset(p2,"cloud = 1")

points = as(clouds,"SpatialPointsDataFrame")
plot(points)

brick = as(clouds,"RasterBrick")
spplot(brick)

## ----trmmAggregation, cache=1--------------------------------------------
trmm = scidbst("TRMM3B42_DAILY")
trmm.prec = project(trmm,"band5")

trmm.prec.crop = crop(trmm.prec,l7_ethiopia)
daily.avg.ethiopia = aggregate(trmm.prec.crop,list("t"),FUN="avg(band5)")

te = textent(as.POSIXct("2010-01-01"),as.POSIXct("2013-01-01"))
tsubset = subarray(daily.avg.ethiopia,limits=te,between=TRUE)
ts = as(tsubset,"xts")
plot(ts,major.ticks="months",minor.ticks=FALSE,main="Average precipitation for Ethiopia Landsat scene from 2010 to 2013")

## ----joinArraysPreperation, cache=1--------------------------------------
joined.name = "join_ls7ndvi_srtm_ethiopia"

ls7_ethiopia = scidbst("L7_SW_ETHOPIA")
srtm = scidbst("SRTM_AFRICA")
ls7.slice = slice(ls7_ethiopia,"t",7)

extent(ls7.slice)
extent(srtm)
srtm.sub = crop(srtm,ls7.slice)

## ----joinArraysResolution, cache=1---------------------------------------
res(ls7.slice)
res(srtm.sub)

## ----removeJoined, include=FALSE-----------------------------------------

if (any(array.list$name == joined.name)) {
  scidbrm(joined.name,force=TRUE)
}

## ----joinExecution, cache=1----------------------------------------------
joined.array = join(ls7.slice,srtm.sub,storeTemp=TRUE,name=joined.name)
joined.array.brick = as(joined.array,"RasterBrick")
# joined.array.brick = writeRaster(joined.array.brick,filename="joined_slice.tif",format="GTiff")

## ----joinVisual, cache=1-------------------------------------------------
spplot(subset(joined.array.brick,1)/max(values(subset(joined.array.brick,1)),na.rm=TRUE),main="Landsat NDVI (regridded)")
spplot(subset(joined.array.brick,2),main="SRTM heights in m")

