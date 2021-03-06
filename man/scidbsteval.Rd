% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scidbsteval.R
\docType{methods}
\name{scidbsteval}
\alias{scidbsteval}
\title{Executes cascaded operations and stores data in scidb under a given name}
\usage{
\S4method{scidbsteval}{scidbst,character}(expr, name, eval = TRUE,
  gc = TRUE, temp = FALSE, drop = TRUE)
}
\arguments{
\item{expr}{The scidbst object}

\item{name}{The name of the target array in which the data is stored (required).}

\item{eval}{A flag whether or not the commands shall be executed in scidb}

\item{gc}{A flag whether or not the result should be tied to the R garbage collector}

\item{temp}{A flag wheter or not the resulting scidb array is temporary}

\item{drop}{Whether or not to drop spatial or temporal references, when dimension is removed from array}
}
\value{
The modified scidbst object
}
\description{
The function works in the similar way as \link{scidbeval}. As an expression this function requires a 'scidbst' object,
which can be modified by various scidb operations. By calling this function the actual commands are executed in the
SciDB cluster. The result will be stored under the given 'name' parameter. In addition to the original function, the
evaluation of a scidbst object will also set the current spatial and/or temporal reference.
}
\note{
Using the similar function \code{scidbeval} function will also perform the storing operation, but it will not transfer
the dimension references for space and/or time. Also, unbounded dimensions that are not dropped will be created
as bounded dimensions by its minimum/maximum dimension value.

In contrast to \code{\link[scidb]{scidbeval}} the parameter name is required. The creation of arrays with a random name in scidb are
not supported by \code{scidbsteval}.
}
\examples{
\dontrun{
scidbconnect(...)
scidbst.obj = scidbst(array_name) # array with spatial and temporal dimensions

#array renaming
scidbsteval(expr=scidbst.obj,name=new_name)

# slicing and storing
sliced = slice(scidbst.obj,"t","2016-05-03")
scidbsteval(sliced,new_name)

# aggregation over space and storing
agg.t = aggregate(x=scidbst.obj,by=list("t"),FUN="avg(attribute1)")
scidbsteval(agg.t,name=new_name)
}
}
\seealso{
\code{\link{scidbeval}}
}

