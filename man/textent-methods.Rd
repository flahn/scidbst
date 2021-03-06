% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textent.R
\docType{methods}
\name{textent,scidbst}
\alias{textent,POSIXt-method}
\alias{textent,scidbst}
\title{Getter and constructor for a TemporalExtent}
\usage{
\S4method{textent}{scidbst}(x)

\S4method{textent}{POSIXt}(x, y)
}
\arguments{
\item{x}{a scidbst object or a POSIXt derived object for the minimum inclusive boundary}

\item{y}{A POSIXt derived object for the maximum inclusive boundary}
}
\value{
The temporal extent as a \code{\link{TemporalExtent}} object
}
\description{
This constructor function creates a TemporalExtent object that defines an interval on the temporal dimension with a minimum
and maximum POSIXt value.
This method will return the stored temporal extent that contains the minimum and maximum date values in a list. The list will be
empty if the array has no temporal dimension.
}
\examples{
# manually creating a TemporalExtent object
tmin = as.POSIXlt("2016-01-01")
tmax = strptime("01-02-2016",format="\%d-\%m-\%Y")
ext = textent(tmin,tmax)

\dontrun{
# Get the temporal extent from a scidbst object
st.arr = scidbst("st_array")
ext = textent(st.arr)
}
}

