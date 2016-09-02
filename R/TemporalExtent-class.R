#' @exportClass TemporalExtent
.textent_class = setClass("TemporalExtent",
                          slots=c(
                            min="POSIXt",
                            max="POSIXt"
                          )
)

#' @export
textent = function(min, max) {
  out = .textent_class()
  if (max < min) {
    out@min = max
    out@max = min
  } else {
    out@min = min
    out@max = max
  }
  return(out)
}

if (!isGeneric("tmin")) {
  setGeneric("tmin",function(x) {
    standardGeneric("tmin")
  })
}
if (!isGeneric("tmax")) {
  setGeneric("tmax",function(x) {
    standardGeneric("tmax")
  })
}


#' @export
setMethod("tmin",signature(x="TemporalExtent"),function(x) {
  return(x@min)
})

#' @export
setMethod("tmax",signature(x="TemporalExtent"),function(x) {
  return(x@max)
})

#' @export
setMethod("show",signature(object="TemporalExtent"),function(object){
  cat("Class: TemporalExtent\n")
  cat(paste("From:\t",object@min,"\n",sep=""))
  cat(paste("To:\t",object@max,"\n",sep=""))
})
