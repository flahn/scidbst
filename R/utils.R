# function to calculate the dimension index by a given time string
# x: scidbst object
# time: POSIXlt or string in POSIXlt format
# returns temporal index
.calcTDimIndex = function (x, time) {
  if (is.character(time)) {
    time = as.POSIXlt(.getDateTime(time,tunit(x)))
  } else {
    #is this called?
    time = as.POSIXlt(time) #assuming valid POSIX string
  }

  if (time >= tmin(x) && time <= tmax(x)) {
    t0 = t0(x)
    dt = tres(x)
    unit = tunit(x)
    index  = floor(as.numeric(difftime(time,t0,unit))/dt)

    return(index)
  } else {
    stop("time statement is out of bounds")
  }
}

setGeneric("tindex",function(x,...) {
  standardGeneric("tindex")
})

#' Calculates the temporal index of a date time string or POSIXt object
#'
#' @param x scidbst array
#' @param time a character string representing a valid date time or a POSIXt object
#' @return integer the nearest temporal index for specified date time
#'
#' @export
setMethod("tindex",signature(x="scidbst"),function(x,time) {
  if (!x@isTemporal) {
    stop("Cannot calculate temporal index, because the array has no temporal reference.")
  }
  if (missing(time)) {
    stop("Cannot find time parameter to calculate the temporal index from.")
  }
  return(.calcTDimIndex(x,time))
})

# Tries to match the temporal resolution string against some regexpressions in order to find
# the correct POSIX time resolution for functions like 'difftime'
#
# res: temporal resolution string like P16D
.findTUnit = function(res) {
  days = "P(\\d)+D"
  months = "P(\\d)+M"
  years = "P(\\d)+Y"
  weeks = "P(\\d)+W"
  hours = "P(\\d)+h"
  minutes = "P(\\d)+m"
  seconds = "P(\\d)+s"

  if (grepl(days,res)) {
    return("days")
  }
  if (grepl(months,res)) {
    return("months")
  }
  if (grepl(years,res)) {
    return("years")
  }
  if (grepl(weeks,res)) {
    return("weeks")
  }
  if (grepl(hours,res)) {
    return("hours")
  }
  if (grepl(minutes,res)) {
    return("mins")
  }
  if (grepl(seconds,res)) {
    return("secs")
  }
}


# transforms a string by a given temporal unit into a valid POSIX string
# currently limited to days... TODO extend
.getDateTime = function (str, unit) {
  if (unit == "days") {
    tmp = strptime(str, "%Y-%m-%d") #day in month of year
    if (is.na(tmp)) {
      tmp = strptime(str, "%Y-%j") #day of year
    }
    return(tmp)
  }
  stop("Cannot extract start time of the time series")
}

#checks if all entries in a matrix are NA
.isMatrixEmpty = function (m) {
  return(all(is.na(m)) == 1 )
}

# creates a affine transformation matrix from a string that is received for example as part of the response of 'eo_getsrs'
.createAffineTransformation = function(srs) {
  .res_matrix = matrix(ncol=3,nrow=2)

  .vec = as.numeric(as.matrix(as.data.frame(strsplit(strsplit(srs$A,"\\s")[[1]],"="))[2,]))
  .res_matrix[,1] = .vec[1:2]
  .res_matrix[,2] = .vec[c(3,5)]
  .res_matrix[,3] = .vec[c(6,4)]

  return(.res_matrix)
}

# Transforms coordinates given in dimension indices to worldly coordinates
# trans: affine transformation matrix
# x: x coordinate (West-East)
# y: y coordinate (North-South)
.transformToWorld = function(trans,x,y) {
  return(trans %*% c(1,x,y))
}

# Calculates dimension indices from spatial real world coordinates given by an extent
# object: scidbst object
# extent: extent object
#
# returns extent object
.calculateDimIndices = function(object, extent) {
  ll = c(xmin(extent),ymin(extent))
  ur = c(xmax(extent),ymax(extent))

  origin = affine(object)[,1]
  sub = affine(object)[,2:3]

  img1 = (solve(sub) %*% (ll - origin))
  img2 = (solve(sub) %*% (ur - origin))

  indices = extent(c(range(img1[1],img2[1]),range(img1[2],img2[2])))
  # use inner bounding box in case of doubt => means that there will be values!
  # xmin(indices) = floor(xmin(indices))
  # ymin(indices) = floor(ymin(indices))
  # xmax(indices) = ceiling(xmax(indices))
  # ymax(indices) = ceiling(ymax(indices))

  xmin(indices) = round(xmin(indices))
  ymin(indices) = round(ymin(indices))
  xmax(indices) = round(xmax(indices))
  ymax(indices) = round(ymax(indices))
  return(indices)
}

# Returns the lengths of each individual dimension
# obj: scidbst object
#
# returns: numeric vector with dimension names and lengths
.getLengths = function(obj) {
  .dimnames = dimensions(obj)
  .dimbounds = scidb_coordinate_bounds(obj)
  v = as.numeric(.dimbounds$length)
  names(v) = .dimnames

  return(v)
}



# Calculates POSIX time from an temporal index
# x: scidbst object
# n: (numeric) the temporal index
.calculatePOSIXfromIndex = function(x,n) {
  baseTime = 0

  if (tunit(x) == "weeks") {
    baseTime = 7*24*60*60
  } else if (tunit(x)  == "days") {
    baseTime = 24*60*60
  } else if (tunit(x)  == "hours") {
    baseTime = 60 * 60
  } else if (tunit(x)  == "mins") {
    baseTime = 60
  } else if (tunit(x)  == "secs") {
    baseTime = 1
  } else {
    stop("currently no other temporal unit supported")
  }

  val = round(as.POSIXlt(as.character(t0(x) + n * tres(x) * baseTime)),units=tunit(x))
  return(val)
}

setGeneric("datetime",function(x,...) {
  standardGeneric("datetime")
})

#' Calculates a POSIXt from a time index
#'
#' The function calculates the appropriate POSIXt object given an scidbst array and a time index.
#'
#' @param x scidbst object
#' @param n integer value
#' @return POSIXt object for the time index n
#'
#' @export
setMethod("datetime",signature(x="scidbst"), function(x, n) {
  if (!x@isTemporal) {
    stop("Scidbst array is not temporal. Cannot find a time dimension.")
  }
  if (missing(n) || !is.numeric(n)) {
    stop("No time index to translate.")
  }

  return(.calculatePOSIXfromIndex(x,n))
})


#' Lists all scidb arrays with a dimension reference
#'
#' This function will list all the SciDB arrays that have a special reference on one or more dimensions. It will also list the type of
#' array: 's' for spatial, 't' for temporal, st for the combination of both
#'
#' @return data.frame with columns "name" and "setting"
#'
#' @seealso \code{\link{scidb::scidbls}}
#' @export
scidbst.ls = function() {
  result=iquery("eo_arrays()",return=TRUE)
  result = result[,-which("i"==names(result))]
  names(result) = c("name","type")
  return(result)
}

#' Transform all spatial indices to spatial coordinates
#'
#' @param obj The scidbst object
#' @param df A data.frame derived from an scidbst object with indices as dimension values
#'
#' @return data.frame with spatial coordinates
#' @examples
#' \dontrun{
#'  x = scidbst(arrayname)
#'  df = as(x,"data.frame") # executes iquery and returns a data.frame with dimension indices and attribute values
#'  coordinates = transformAllSpatialIndices(x,df) # returns only the spatial coordinates of scidbst object
#' }
#' @export
transformAllSpatialIndices = function(obj,df) {
  .data = df
  from = obj

  coords = rbind(rep(1,nrow(.data)),.data[,xdim(from)],.data[,ydim(from)])
  res = t(affine(from) %*% coords) #(x,y)
  .data[,xdim(from)] = res[,1]
  .data[,ydim(from)] = res[,2]

  return(.data)
}

#' Transforms a given data.frame with temporal dimension into dates by a given scidbst object
#'
#' This function transforms the temporal indices of a data.frame that was derived from a scidbst object into the actual
#' dates.
#'
#' @param obj The scidbst object
#' @param df A data.frame derived from an scidbst object
#' @return data.frame with temporal coordinates
#'
#' @examples
#' \dontrun{
#'  x = scidbst(arrayname)
#'  df = as(x,"data.frame") # executes iquery and returns a data.frame with dimension indices and attribute values
#'  times = transformAllTemporalIndices(x,df) # returns only the unique temporal coordinates of scidbst object
#' }
#' @export
transformAllTemporalIndices = function(obj,df) {
  .data=df
  from=obj

  tdim = tdim(from)
  tindex = unique(.data[,tdim])
  dates = lapply(tindex,function(x,y) {
    as.Date(.calculatePOSIXfromIndex(y,x))
  },y=from)
  time = .data[,tdim]

  for (i in tindex) {
    pos = which(tindex==i)
    time[time==i] = dates[[pos]]
  }
  time = as.Date(time,origin="1970-01-01")

  .data[,tdim] = time
  return(.data)
}

.downloadData = function(object) {
  cat("Downloading data...\n")
  .data = iquery(object@proxy@name,return=T) #query scidb for data
  cat("Done.\n")
  return(.data)
}

# Calculates the amount of values that can be expected if the attribute data is downloaded
#x: scidbst object
.countValues = function(x) {
  attr.statement = paste("count(",scidb_attributes(x),")",sep="",collapse=",")
  agg.cmd = paste("aggregate(",scidb_op(x),", ",attr.statement,")",sep="")
  values = as.double(iquery(agg.cmd,return=TRUE))
  values = values[-1]
  names(values) = scidb_attributes(x)

  return(values)
}

# x: scidbst object
.memorySize = function(x,unit="MB") {
  unit = toupper(unit)
  if (!unit %in% c("KB","MB","GB")) stop("Unrecognized data storage unit.")

  # number of dimension x 64 Bit (stored as int64) ~ do use R positive 52-bit integer values as rough equivalent
  size = length(dimensions(x))*52
  attr.count = .countValues(x)
  types = scidb_types(as(x,"scidb"))
  types[types == "double"] = 53
  types[types == "unit8" | types == "int8" | types == "int16" | types == "uint16"] = 32
  types = as.numeric(types)
  avg.count = mean(attr.count)

  dim.space = avg.count * size
  attr.space = sum(types * attr.count)
  total=dim.space + attr.space # in Bit

  if (unit == "KB") {
    return(total/(8*1024))
  }
  if (unit == "MB") {
    return(total/(8*1024*1024))
  }
  if (unit == "GB") {
    return(total/(8*1024*1024*1024))
  }

}

setGeneric("estimateFileSize", function(x, ...) {
  standardGeneric("estimateFileSize")
})

#' Estimates the file size for a scidbst array
#'
#' This function uses the meta information on a scidbst array to estimate the potential file size.
#'
#' @param x scidbst array
#' @param unit (optional) one of c("KB","MB","GB") to specify the digital storage unit. Default is MB
#' @return numeric the estimated file size in the unit specified
#' @export
setMethod("estimateFileSize",signature(x="scidbst"), function(x, unit="MB") {
  return(.memorySize(x, unit))
})

# recalculates the temporal resolution in seconds
.tres2seconds = function(A) {
  if (!A@isTemporal) {
    stop("Array has no time dimension")
  }
  unit = tunit(A)
  res = tres(A)
  allUnits = c("secs", "mins", "hours", "days", "weeks")
  factors = c(1, 60, 60*60, 60*60*24, 60*60*24*7)
  if (!unit %in% allUnits) {
    stop("Cannot convert time resolution into seconds. Temporal Unit is not supported")
  } else {
    res = res * factors[allUnits == unit]

    #note: might be a  problem with daylight saving times -> will be off about 1 hour if it changes
  }

  return(res)
}

# returns a vector of names for potential temporary arrays
# x: scidbst, n: number of names to be created
.getTempNames = function(x,n) {
  if (!is.null(x@temps)) {
    usedIDs = as.integer(regmatches(x@temps,regexpr("(\\d)+$",x@temps)))
    ids = sample.int(2147483647,n,replace=FALSE)

    # rule out that there are any double ids
    loglist = any(ids %in% usedIDs)
    startExpr = loglist
    while(startExpr) {
      doublets = ids[startExpr]
      amount = length(doublets)
      newIds = sample.int(2147483647,amount,replace=FALSE)
      ids[startExpr] = newIds
      startExpr = any(ids %in% usedIDs)
    }
  } else {
    ids = sample.int(2147483647,n,replace=FALSE)
  }

  names = paste("__temp_",x@title,"_",ids,sep="")
  return(names)
}

.rescaleAffine = function(x, xresfac, yresfac) {
  scale_matrix = matrix(c(1,0,0, 0,xresfac,0, 0,0,yresfac),ncol=3)
  #scale
  scaled_matrix = affine(x) %*% scale_matrix

  #retrieve array origin and transform it to real world coordinate
  .starts = as.numeric(scidb_coordinate_start(x))
  names(.starts) = dimensions(x)
  origin.rw = .transformToWorld(affine(x),.starts[xdim(x)],.starts[ydim(x)])

  #create temporary matrix to calculate the new origin
  help.matrix = cbind(origin.rw,-(scaled_matrix[,2:3]))
  new.origin = help.matrix %*% c(1,.starts[xdim(x)],.starts[ydim(x)])

  #bind new origin and the scaling parameter
  x@affine = cbind(new.origin,scaled_matrix[,2:3])

  return(x)
}
