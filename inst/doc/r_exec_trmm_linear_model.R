## ----connecting, echo=TRUE, message=FALSE, warning=FALSE-----------------
library(scidbst)
library(rgdal)
library(plyr)
source("/home/lahn/GitHub/scidbst/vignettes/assets/credentials.R")
scidbconnect(host=host,port=port,user=user,password=password,protocol="https",auth_type = "digest")

## ---- eval=FALSE---------------------------------------------------------
#  dot.input = list(...)
#  
#  i <- 1
#  lapply(dot.input, function(x,y) {
#      assign(x=y[i],value=x,envir=parent.env(environment()))
#      i <<- i+1
#      x},
#      names(dot.input))
#  rm(i)

## ---- eval=FALSE---------------------------------------------------------
#   # helper function to add a certain time to a date
#  addTime = function(x, i, what) {
#      byText = paste(i,what)
#      seq(x,by=byText,length=2)[2]
#  }
#  
#  # determine the first and last index of the yearly subset in the chunk
#  first = 0
#  last = 0
#  
#  while (! any(first %in% x$dimt) ) {
#      indexDate = addTime(t0,first,tunit) # current date of first
#      first <- as.numeric(difftime(addTime(indexDate,1,"year"),t0)) # 1 year later of indexDate
#      last <- as.numeric(difftime(addTime(addTime(indexDate,2,"year"),-1,"day"),t0)) # 2 years later - 1 day from indexDate
#  }
#  
#  numberOfYear = as.POSIXlt(addTime(t0,first,tunit))$year - as.POSIXlt(t0)$year
#  

## ---- eval=FALSE---------------------------------------------------------
#  addNeighboringValues = function(x) {
#      t = unique(x$dimt)
#      x= x[,-which("dimt" == names(x))]
#      coordinates(x) = ~dimx+dimy
#      gridded(x) = TRUE
#      layer = as(x,"RasterLayer")
#  
#      p1 = focal(layer, w=matrix(c(1,0,0, 0, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
#      p2 = focal(layer, w=matrix(c(0,1,0, 0, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
#      p3 = focal(layer, w=matrix(c(0,0,1, 0, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
#      p4 = focal(layer, w=matrix(c(0,0,0, 1, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
#  
#      p6 = focal(layer, w=matrix(c(0,0,0, 0, 0, 1, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
#      p7 = focal(layer, w=matrix(c(0,0,0, 0, 0, 0, 1, 0, 0),nr = 3, nc=3,byrow=TRUE))
#      p8 = focal(layer, w=matrix(c(0,0,0, 0, 0, 0, 0, 1, 0),nr = 3, nc=3,byrow=TRUE))
#      p9 = focal(layer, w=matrix(c(0,0,0, 0, 0, 0, 0, 0, 1),nr = 3, nc=3,byrow=TRUE))
#  
#      stack = stack(list(orig.values=layer,p1=p1,p2=p2,p3=p3,p4=p4,p6=p6,p7=p7,p8=p8,p9=p9))
#      list(t=t,data=stack)
#  }
#  layer.timeline = dlply(x, .variables=c("dimt"), .fun = addNeighboringValues)

## ---- eval=FALSE---------------------------------------------------------
#  rebuildDataFrame = function(x) {
#      b = cbind(dimy=coordinates(x$data)[,"y"],dimx=coordinates(x$data)[,"x"],dimt=x$t,getValues(x$data))
#      as.data.frame(b)
#  }
#  neighbor.df = ldply(layer.timeline[], .fun = rebuildDataFrame)
#  neighbor.df = na.omit(neighbor.df)

## ---- eval=FALSE---------------------------------------------------------
#  lmCalculation = function(x,year) {
#      #sort by t ascending by time
#      x = arrange(x,dimt)
#  
#      model = lm(orig.values~p1+p2+p3+p4+p6+p7+p8+p9, x,na.action=na.omit)
#      c = coefficients(model)
#      s = summary(model)
#      data.frame(dimy=unique(x$dimy),
#                 dimx=unique(x$dimx),
#                 dimt=as(year*1.0,"double"),
#                 intercept = c[1],
#                 c_p1= c[2],
#                 c_p2= c[3],
#                 c_p3= c[4],
#                 c_p4= c[5],
#                 c_p6= c[6],
#                 c_p7= c[7],
#                 c_p8= c[8],
#                 c_p9= c[9],
#                 r_sq = s$r.squared,
#                 sum_res_sq = sum(s$residuals^2))
#  }
#  calculated.df = ddply(neighbor.df, .variables=c("dimy","dimx"), .fun = lmCalculation, year=numberOfYear)
#  calculated.df = na.omit(calculated.df)

## ---- eval=TRUE----------------------------------------------------------
linearRegressionNeighborhood = function(x, ...) {
    #make the dots named parameters available in this function
    dot.input = list(...)
    
    i <- 1
    lapply(dot.input, function(x,y) {
        assign(x=y[i],value=x,envir=parent.env(environment()))
        i <<- i+1
        x},
        names(dot.input))
    rm(i)
  
    cat("processing chunk",file="/tmp/r_exec/lahn/trmm_linear.log",append=TRUE,sep="\n")
    additionalParams = names(list(...))
    cat(paste("Using additional parameter through ...:"),file="/tmp/r_exec/lahn/trmm_linear.log",append=TRUE,sep="\n")
    cat(paste(additionalParams,collapse=", "),file="/tmp/r_exec/lahn/trmm_linear.log",append=TRUE,sep="\n")
    
    # helper function to add a certain time to a date
    addTime = function(x, i, what) {
        byText = paste(i,what)
        seq(x,by=byText,length=2)[2]
    }
  
    # determine the first and last index of the yearly subset in the chunk
    first = 0
    last = 0
    
    while (! any(first %in% x$dimt) ) {
        indexDate = addTime(t0,first,tunit) # current date of first
        first <- as.numeric(difftime(addTime(indexDate,1,"year"),t0)) # 1 year later of indexDate
        last <- as.numeric(difftime(addTime(addTime(indexDate,2,"year"),-1,"day"),t0)) # 2 years later - 1 day from indexDate
    }
    
    numberOfYear = as.POSIXlt(addTime(t0,first,tunit))$year - as.POSIXlt(t0)$year

    
    # create a subset to use only data from one year
    
    x = na.omit(x[which(x$dimt %in% first:last),])
    
    # add the neighboring values to each of the pixel values using raster operations
    addNeighboringValues = function(x) {
        t = unique(x$dimt)
        x= x[,-which("dimt" == names(x))]
        coordinates(x) = ~dimx+dimy
        gridded(x) = TRUE
        layer = as(x,"RasterLayer")
      
        p1 = focal(layer, w=matrix(c(1,0,0, 0, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
        p2 = focal(layer, w=matrix(c(0,1,0, 0, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
        p3 = focal(layer, w=matrix(c(0,0,1, 0, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
        p4 = focal(layer, w=matrix(c(0,0,0, 1, 0, 0, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
        
        p6 = focal(layer, w=matrix(c(0,0,0, 0, 0, 1, 0, 0, 0),nr = 3, nc=3,byrow=TRUE))
        p7 = focal(layer, w=matrix(c(0,0,0, 0, 0, 0, 1, 0, 0),nr = 3, nc=3,byrow=TRUE))
        p8 = focal(layer, w=matrix(c(0,0,0, 0, 0, 0, 0, 1, 0),nr = 3, nc=3,byrow=TRUE))
        p9 = focal(layer, w=matrix(c(0,0,0, 0, 0, 0, 0, 0, 1),nr = 3, nc=3,byrow=TRUE))
        
        stack = stack(list(orig.values=layer,p1=p1,p2=p2,p3=p3,p4=p4,p6=p6,p7=p7,p8=p8,p9=p9))
        list(t=t,data=stack)
    }
    layer.timeline = dlply(x, .variables=c("dimt"), .fun = addNeighboringValues)
    
    # create a data.frame from the RasterStacks and omit the NA values in the overlap border
    # careful x and y are the assigned names for coordinates in package raster
    rebuildDataFrame = function(x) {
        b = cbind(dimy=coordinates(x$data)[,"y"],dimx=coordinates(x$data)[,"x"],dimt=x$t,getValues(x$data))
        as.data.frame(b)
    }
    neighbor.df = ldply(layer.timeline[], .fun = rebuildDataFrame)
    neighbor.df = na.omit(neighbor.df)
    
    # fit linear regression models on each pixel, using year as parameter
    lmCalculation = function(x,year) {
        #sort by t ascending by time
        x = arrange(x,dimt)
      
        model = lm(orig.values~p1+p2+p3+p4+p6+p7+p8+p9, x,na.action=na.omit)
        c = coefficients(model)
        s = summary(model)
        data.frame(dimy=unique(x$dimy),
                   dimx=unique(x$dimx),
                   dimt=as(year*1.0,"double"),
                   intercept = c[1],
                   c_p1= c[2],
                   c_p2= c[3],
                   c_p3= c[4],
                   c_p4= c[5],
                   c_p6= c[6],
                   c_p7= c[7],
                   c_p8= c[8],
                   c_p9= c[9],
                   r_sq = s$r.squared,
                   sum_res_sq = sum(s$residuals^2))
    }
    calculated.df = ddply(neighbor.df, .variables=c("dimy","dimx"), .fun = lmCalculation, year=numberOfYear)
    
    calculated.df = na.omit(calculated.df)
    cat(paste("Finished year:",numberOfYear),file="/tmp/r_exec/lahn/trmm_linear.log",append=TRUE,sep="\n")
    return(calculated.df)
}

## ------------------------------------------------------------------------
name = "trmm_ethiopia_sub_reparted"
arrays = scidbls()

if (! name %in% arrays) {
  trmm = scidbst("TRMM3B42_DAILY_PREC")
  ex = extent(32,46,1,15)
  trmm.sub = subarray(trmm,ex,between=TRUE)
  trmm.sub = scidbsteval(trmm.sub,"trmm_ethiopia_sub")
  trmm.sub = transform(trmm.sub,dimy="double(y)",dimx="double(x)",dimt="double(t)")
  trmm.reparted = repart(trmm.sub,schema="<band1:double,dimy:double,dimx:double,dimt:double> [y=0:399,30,1,x=0:1439,30,1,t=0:*,365,6]")
  trmm.reparted = scidbsteval(trmm.reparted,"trmm_ethiopia_sub_reparted")
} else {
  trmm.reparted = scidbst("trmm_ethiopia_sub_reparted")
}


## ---- comment=""---------------------------------------------------------
script <- r.apply(x=trmm.reparted,
                   f = linearRegressionNeighborhood,
                   array = "trmm_yearly_linear_model",
                   packages = c("raster","sp","rgdal","plyr"),
                   aggregates=c(),
                   output = list(dimy="double",
                                 dimx="double",
                                 dimt="double",
                                 intercept="double",
                                 c_p1= "double",
                                 c_p2= "double",
                                 c_p3= "double",
                                 c_p4= "double",
                                 c_p6= "double",
                                 c_p7= "double",
                                 c_p8= "double",
                                 c_p9= "double",
                                 r_sq = "double",
                                 sum_res_sq = "double"),
                   eval=FALSE,
                   return="rscript"
                   )
cat(script)

## ---- eval=FALSE---------------------------------------------------------
#  sa.array <- r.apply(x=trmm.reparted,
#                     f = linearRegressionNeighborhood,
#                     array = "trmm_yearly_linear_model",
#                     packages = c("raster","sp","rgdal","plyr"),
#                     aggregates=c(),
#                     output = list(dimy="double",
#                                   dimx="double",
#                                   dimt="double",
#                                   intercept="double",
#                                   c_p1= "double",
#                                   c_p2= "double",
#                                   c_p3= "double",
#                                   c_p4= "double",
#                                   c_p6= "double",
#                                   c_p7= "double",
#                                   c_p8= "double",
#                                   c_p9= "double",
#                                   r_sq = "double",
#                                   sum_res_sq = "double")
#                     )

## ---- eval=FALSE,include=FALSE-------------------------------------------
#  trmm.reparted = scidbst("trmm_ethiopia_sub_reparted")
#  img.1 = as(aggregate(subset(trmm.reparted,"t>=0 and t < 365"),by=c("x","y"),"avg(band1)"),"RasterBrick")
#  img.2 = as(aggregate(subset(trmm.reparted,"t>=365 and t < 730"),by=c("x","y"),"avg(band1)"),"RasterBrick")
#  img.3 = as(aggregate(subset(trmm.reparted,"t>=730 and t < 1096"),by=c("x","y"),"avg(band1)"),"RasterBrick")
#  img.4 = as(aggregate(subset(trmm.reparted,"t>=1096 and t < 1461"),by=c("x","y"),"avg(band1)"),"RasterBrick")
#  
#  spplot(img)
#  
#  array = scidbst("trmm_yearly_linear_model")
#  estimateFileSize(array)
#  df = as(array,"data.frame")
#  createRasterLayer = function(x) {
#    x = suppressWarnings( x[,-which("i" == names(x))])
#    t = unique(x[,"dimt"])
#    coordinates(x) = ~dimx+dimy
#    gridded(x) <- TRUE
#    stack = as(x,"RasterStack")
#    list(t=t,data=stack)
#  }
#  l = dlply(df,.variables=c("dimt"),.fun=createRasterLayer)
#  
#  
#  spplot(stack(img.1,img.2,img.3,img.4))
#  spplot(stack(subset(l[[1]]$data,"sum_res_sq"),
#               subset(l[[2]]$data,"sum_res_sq"),
#               subset(l[[3]]$data,"sum_res_sq"),
#               subset(l[[4]]$data,"sum_res_sq")))
#  
#  aplot = function(l, var) {
#    spplot(stack(subset(l[[1]]$data,var),
#               subset(l[[2]]$data,var),
#               subset(l[[3]]$data,var),
#               subset(l[[4]]$data,var)))
#  }
#  
#  aplot(l, "c_p1")
#  aplot(l, "c_p2")
#  aplot(l, "c_p3")
#  aplot(l, "c_p4")
#  aplot(l, "c_p6")
#  aplot(l, "c_p7")
#  aplot(l, "c_p8")
#  aplot(l, "c_p9")
#  
#  
#  cp1.mod = cp1.3
#  cp1.mod[cp1.mod > 6] <- NA
#  spplot(cp1.mod)
#  cp1.3 = subset(l[[3]]$data,"c_p1")
#  
#  
#  # TODO
#  # visualize data
#  # perform redimensioning

