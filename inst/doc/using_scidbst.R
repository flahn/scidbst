## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
outputFolder = "./assests/"

## ---- include=FALSE------------------------------------------------------
source("/home/lahn/GitHub/scidbst/vignettes/assets/credentials.R")

## ------------------------------------------------------------------------


scidbconnect(host=host,port=port,user=user,password=password,protocol="https",auth_type = "digest")
scidbst.ls()

## ------------------------------------------------------------------------
l7_ethiopia = scidbst("L7_SW_ETHOPIA")
l7_ethiopia
extent(l7_ethiopia)
crs(l7_ethiopia)
t.extent(l7_ethiopia)

