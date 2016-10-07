if (!isGeneric("transfer")) {
  setGeneric("transfer",function(x,y,...) {
    standardGeneric("transfer")
  })
}

# x: scidbst; y: scidbst
.eo_over = function (x, y) {
  xdims = dimensions(x)
  ydims = dimensions(y)
  if (length(xdims) != length(ydims)) stop("Cannot perform eo_over. Unequal number of dimensions detected.")

  dim.match = paste(c("A","B"),rbind(dimensions(x),dimensions(x)),sep=".",collapse=",") # eo_over returns dimensions of x with
  # attributes over_* (* the dimension names in y)

  q.join = paste("cross_join(","eo_over(", scidb_op(x),", ", scidb_op(y) ,") as A,",  scidb_op(x)  ," as B," ,dim.match,")", sep="")
  join.ref = scidb(q.join)
  x@proxy = join.ref

  return(x)
}


#' Transfers dimension indices
#'
#' The function calculates for each cell in object x the coordinates in system of object y. The calculated values are added
#' as attributes on object x.
#'
#' @note It uses the "eo_over" function of the scidb4geo package in scidb.
#'
#' @param x the source object
#' @param y the target system
#' @return source object extended with its coordinates in the target system
#'
#' @export
setMethod("transfer",signature(x="scidbst",y="scidbst"),.eo_over)
