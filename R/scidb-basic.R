if (!isGeneric("dimensions")) {
  setGeneric("dimensions",function(obj){
    standardGeneric("dimensions")
  })
}

#' @export
setMethod("dimensions",signature(obj="scidbst"), function(obj) {
  return(scidb::dimensions(obj@proxy))
})


if (!isGeneric("scidb_attributes")) {
  setGeneric("scidb_attributes",function(x) {
    standardGeneric("scidb_attributes")
  })
}
#' @export
setMethod("scidb_attributes",signature(x="scidbst"), function(x) {
  return(scidb::scidb_attributes(x@proxy))
})

if (!isGeneric("scidb_coordinate_bounds")) {
  setGeneric("scidb_coordinate_bounds",function(x) {
    standardGeneric("scidb_coordinate_bounds")
  })
}

#' @export
setMethod("scidb_coordinate_bounds",signature(x="scidbst"), function(x) {
  return(scidb::scidb_coordinate_bounds(x@proxy))
})

if (!isGeneric("scidb_coordinate_start")) {
  setGeneric("scidb_coordinate_start",function(x) {
    standardGeneric("scidb_coordinate_start")
  })
}

#' @export
setMethod("scidb_coordinate_start",signature(x="scidbst"), function(x) {
  return(scidb::scidb_coordinate_start(x@proxy))
})

if (!isGeneric("scidb_coordinate_end")) {
  setGeneric("scidb_coordinate_end",function(x) {
    standardGeneric("scidb_coordinate_end")
  })
}

#' @export
setMethod("scidb_coordinate_end",signature(x="scidbst"), function(x) {
  return(scidb::scidb_coordinate_end(x@proxy))
})

if (!isGeneric("scidb_coordinate_overlap")) {
  setGeneric("scidb_coordinate_overlap",function(x) {
    standardGeneric("scidb_coordinate_overlap")
  })
}

#' @export
setMethod("scidb_coordinate_overlap",signature(x="scidbst"), function(x) {
  return(scidb::scidb_coordinate_overlap(x@proxy))
})

if (!isGeneric("scidb_coordinate_chunksize")) {
  setGeneric("scidb_coordinate_chunksize",function(x) {
    standardGeneric("scidb_coordinate_chunksize")
  })
}

#' @export
setMethod("scidb_coordinate_chunksize",signature(x="scidbst"), function(x) {
  return(scidb::scidb_coordinate_chunksize(x@proxy))
})
