if (!isGeneric("transfer")) {
  setGeneric("transfer",function(x,y,...) {
    standardGeneric("transfer")
  })
}


.eo_over = function (x, y) {
  #TODO "join" might be deprecated in future releases of scidb... use cross_join
  q.join = paste("join(eo_over(", scidb_op(x),", ", scidb_op(y) ,"),",  scidb_op(x)  ,")", sep="")
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
