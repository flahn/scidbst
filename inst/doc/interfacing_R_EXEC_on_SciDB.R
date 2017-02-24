## ----connecting----------------------------------------------------------
library(scidbst)
library(rgdal)
source("/home/lahn/GitHub/scidbst/vignettes/assets/credentials2.R")
scidbconnect(host=host,port=port,user=user,password=password,protocol="https",auth_type = "digest")

## ----creating_demo_array-------------------------------------------------
EPSG = make_EPSG()
srid = 4326
proj4args = as.character(subset(EPSG,code==4326)$prj4)

aff = matrix(c(34.5,8.0, 0.01,0, 0,-0.01),nrow=2,ncol=3)
ex = extent(34.5,35.5,7,8)
tex = textent(as.POSIXlt("2003-07-28"),as.POSIXlt("2003-11-05"))
srs = SRS(proj4args,c("y","x"))
srs@authority = "EPSG"
srs@srid = as.integer(srid)
srs@srtext = showWKT(proj4args)

trs = TRS(dimension="t",t0="2003-07-28",tres=1,tunit="days")

sizex = 100
sizey = 100
sizet = 100

# random array
gr = expand.grid(0:(sizex-1),0:(sizey-1),0:(sizet-1))
gr = cbind(gr,runif(sizex*sizey*sizet,1,100))
colnames(gr) = c("y","x","t","val")

array = as.scidb(X=gr,name="tmp_arr",chunkSize=nrow(gr))
schema = sprintf("<val:double NULL> [y=0:%i,20,0,x=0:%i,20,0,t=0:%i,100,0]",(sizey-1),(sizex-1),(sizet-1))

st.arr = new("scidbst")
st.arr@title="random_st"
st.arr@srs = srs
st.arr@extent = ex
st.arr@trs = trs
st.arr@tExtent = tex
st.arr@affine = aff
st.arr@isSpatial = TRUE
st.arr@isTemporal = TRUE
st.arr@proxy = redimension(array,schema=schema)

st.arr = scidbsteval(st.arr,"random_st")
scidbrm("tmp_arr",force=T)

## ------------------------------------------------------------------------
arr.prep = transform(st.arr, dimx="double(x)",dimy="double(y)", dimt="double(t)")
arr.prep = scidbsteval(arr.prep,name="random_st_prep")
arr.prep@proxy

scidbrm("random_st",force=T)

## ------------------------------------------------------------------------
f <- function(x) {
  if (is.null(x)) {
    return(c(nt=0,var=0,median=0,mean=0))
  }
  t = x$dimt
  n = x$val

  return(c(nt=length(t),var=var(n),median=median(n),mean=mean(n)))
}

## ------------------------------------------------------------------------
aggregates=c("dimy","dimx")

## ------------------------------------------------------------------------
output=list(dimy="double",dimx="double",nt="double",var="double",median="double",mean="double")

## ------------------------------------------------------------------------
dim=list(dimy="y",dimx="x")
dim.spec=list(y=c(min=0,max=99,chunk=20,overlap=0),x=c(min=0,max=99,chunk=20,overlap=0))

## ------------------------------------------------------------------------
rexec.arr = r.apply(x=arr.prep,
                 f=f,
                 array="rexec_applied",
                 aggregates=aggregates,
                 output = output,
                 dim=dim,
                 dim.spec=dim.spec)

## ------------------------------------------------------------------------
rexec.arr

