if (!isGeneric("repart")) {
  setGeneric("repart",function(x,...) {
    standardGeneric("repart")
  })
}

.repart.scidbst = function(x, schema, upper, chunk, overlap) {
  .scidb = as(x,"scidb")

  if(!missing(schema))
  {
    .scidb = scidb::repart(.scidb,schema)
  }
  if(missing(upper)) upper = scidb_coordinate_end(.scidb)
  if(missing(chunk)) chunk = scidb_coordinate_chunksize(.scidb)
  if(missing(overlap)) overlap = scidb_coordinate_overlap(.scidb)
  # a = scidb:::build_attr_schema(.scidb)

  .scidb = scidb::repart(x=.scidb, upper=upper,chunk=chunk,overlap=overlap)

  x@proxy = .scidb
  return(x)
}

#' Repartion a scidbst array
#'
#' Repartions a scidb array with new chunk and overlap sizes. It will move the values into new chunks.
#'
#' @note This function might take a while, because all data values need to be moved (restored) accordingly. You might either state
#' a valid schema parameter or build a schema from the parameters "upper", "chunk" and "overlap".
#'
#' @name repart,scidbst
#' @param x the scidbst array
#' @param schema The new schema as a string
#' @param upper A vector of upper boundaries for the array
#' @param chunk A vector of chunksizes for each dimension
#' @param overlap A vector of chunk overlaps for the repartitioned array
#' @return scidbst array with modified chunks
#'
#' @examples
#' \dontrun{
#'    scidbconnect()
#'    trmm.sub = scidbst("trmm_sub")
#'
#'    # repartition the array by stating a new schema with the same attributes and dimensions
#'    trmm.reparted = repart(trmm.sub,schema="<band1:double,dimy:double,dimx:double,dimt:double>[y=0:399,30,1,x=0:1439,30,1,t=0:*,365,6]")
#' }
#' @seealso \code{\link[scidb]{repart}}
#' @export
setMethod("repart",signature(x="scidbst"), .repart.scidbst)
