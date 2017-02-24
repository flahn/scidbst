#' @include scidbst-class-decl.R
NULL

##############################
# scidb_op
##############################

#' Reports the current scidb operation cascade
#'
#' The internal scidb proxy object contains the current AFL query string. scidb_op returns the AFL query string.
#'
#' @param x scidb or scidbst object
#' @return character - AFL query
#' @export
scidb_op = function(x) {
  if (inherits(x,"scidbst")) {
    return(x@proxy@name)
  } else if (inherits(x,"scidb")) {
    return(x@name)
  } else {
    stop("parameter x is no scidb or scidbst object.")
  }
}

##############################
# scidb_attributes
##############################

if (!isGeneric("scidb_attributes")) {
  setGeneric("scidb_attributes",function(x) {
    standardGeneric("scidb_attributes")
  })
}

.get.attributes.wrapper = function(x) {
  .scidb = as(x,"scidb")
  return(scidb::scidb_attributes(.scidb))
}

#' @rdname schema-scidbst-methods
#' @export
setMethod("scidb_attributes",signature(x="scidbst"), .get.attributes.wrapper)

#' @export
setMethod("scidb_attributes",signature(x="scidb"), scidb::scidb_attributes)

##############################
# scidb_coordinate_bounds
##############################

if (!isGeneric("scidb_coordinate_bounds")) {
  setGeneric("scidb_coordinate_bounds",function(x) {
    standardGeneric("scidb_coordinate_bounds")
  })
}

.get.coordinate.bounds.wrapper = function(x) {
  .scidb = as(x,"scidb")
  return(scidb::scidb_coordinate_bounds(.scidb))
}

#' @rdname schema-scidbst-methods
#' @export
setMethod("scidb_coordinate_bounds",signature(x="scidbst"), .get.coordinate.bounds.wrapper)

#' @export
setMethod("scidb_coordinate_bounds",signature(x="scidb"),scidb::scidb_coordinate_bounds)


##############################
# scidb_coordinate_start
##############################

if (!isGeneric("scidb_coordinate_start")) {
  setGeneric("scidb_coordinate_start",function(x) {
    standardGeneric("scidb_coordinate_start")
  })
}

.get.coordinate.start.wrapper = function(x) {
  .scidb = as(x,"scidb")
  return(scidb::scidb_coordinate_start(.scidb))
}

#' @rdname schema-scidbst-methods
#' @export
setMethod("scidb_coordinate_start",signature(x="scidbst"), .get.coordinate.start.wrapper)

#' @export
setMethod("scidb_coordinate_start",signature(x="scidb"), scidb::scidb_coordinate_start)

##############################
# scidb_coordinate_end
##############################

if (!isGeneric("scidb_coordinate_end")) {
  setGeneric("scidb_coordinate_end",function(x) {
    standardGeneric("scidb_coordinate_end")
  })
}

.get.coordinate.end.wrapper = function(x) {
  .scidb = as(x,"scidb")
  return(scidb::scidb_coordinate_end(.scidb))
}

#' @rdname schema-scidbst-methods
#' @export
setMethod("scidb_coordinate_end",signature(x="scidbst"), .get.coordinate.end.wrapper)

#' @export
setMethod("scidb_coordinate_end",signature(x="scidb"), scidb::scidb_coordinate_end)

##############################
# scidb_coordinate_overlap
##############################

if (!isGeneric("scidb_coordinate_overlap")) {
  setGeneric("scidb_coordinate_overlap",function(x) {
    standardGeneric("scidb_coordinate_overlap")
  })
}

.get.coordinate.overlap.wrapper = function(x) {
  .scidb = as(x,"scidb")
  return(scidb::scidb_coordinate_overlap(.scidb))
}

#' @rdname schema-scidbst-methods
#' @export
setMethod("scidb_coordinate_overlap",signature(x="scidbst"), .get.coordinate.overlap.wrapper)

#' @export
setMethod("scidb_coordinate_overlap",signature(x="scidb"),scidb::scidb_coordinate_overlap)

##############################
# scidb_coordinate_chunksize
##############################

if (!isGeneric("scidb_coordinate_chunksize")) {
  setGeneric("scidb_coordinate_chunksize",function(x) {
    standardGeneric("scidb_coordinate_chunksize")
  })
}

.get.coordinate.chunksize.wrapper = function(x) {
  .scidb = as(x,"scidb")
  return(scidb::scidb_coordinate_chunksize(.scidb))
}

#' @rdname schema-scidbst-methods
#' @export
setMethod("scidb_coordinate_chunksize",signature(x="scidbst"), .get.coordinate.chunksize.wrapper)

#' @export
setMethod("scidb_coordinate_chunksize",signature(x="scidb"),scidb::scidb_coordinate_chunksize)

#######################
# schema
#######################

if (!isGeneric("schema")) {
  setGeneric("schema",function(x) {
    standardGeneric("schema")
  })
}

.get.schema.wrapper = function(x) {
  .scidb = as(x,"scidb")
  return(scidb::schema(.scidb))
}

#' Schema related functions
#'
#' The listed functions are wrapped from the scidb package. The intention of those wrapped functions is the direct access
#' to strucutural information about the scidb array, e.g. the used schema, overlap, start/end coordinates.
#'
#' @name schema,scidbst
#' @rdname schema-scidbst-methods
#' @aliases scidb_coordinate_bounds,scidbst scidb_attributes,scidbst scidb_coordinate_start,scidbst scidb_coordinate_end,scidbst
#' scidb_coordinate_overlap,scidbst scidb_coordinate_chunksize,scidbst
#' @param x scidbst array
#' @return list or vector of characters, numerics or a character string
#'
#' @examples
#' \dontrun{
#'  array = scidbst("some_st_array")
#'
#'  # information operations
#'  scidb_coordinate_bounds(array) # returns a list
#'  scidb_attributes(array) # return names of the attributes
#'
#'  scidb_coordinate_start(array)
#'  scidb_coordinate_end(array)
#'  scidb_coordinate_overlap(array)
#'  scidb_coordinate_chunksize(array)
#'  # the above return mostly numeric vectors, except the boundary is unbounded with "*" (then a vector of characters)
#'
#'  schema(array) #returns a string
#' }
#' @seealso \link{dimensions,scidbst}, \link[scidb]{scidb_coordinate_bounds}, \link[scidb]{scidb_attributes}, \link[scidb]{scidb_coordinate_start}
#' \link[scidb]{scidb_coordinate_end}, \link[scidb]{scidb_coordinate_overlap}, \link[scidb]{scidb_coordinate_chunksize},
#' \link[scidb]{schema}
#' @export
setMethod("schema",signature(x="scidbst"), .get.schema.wrapper)

#' @export
setMethod("schema",signature(x="scidb"),scidb::schema)

