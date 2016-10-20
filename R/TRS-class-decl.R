#' Temporal Reference System
#'
#' This object holds information regarding a temporal reference system. In particular this reference system consists of a datum (t0), a
#' resolution (tres), a dimension name (dimname) and a measurement unit (tUnit) which is connected to the resolution.
#'
#' @rdname TRS-class
#' @name TRS
#' @slot t0 POSIXt derivative as the datum for this TRS
#' @slot tUnit character - The unit definition as used in \code{\link{difftime}}
#' @slot tResolution numeric - The amount of tUnits that make up the resolution
#' @slot dimname character - The dimension name

#'
#' @details The tUnit is mostly used in cases where the function \code{\link{difftime}} is called. Therefore tUnit can either be "secs", "mins",
#' "hours", "days" or "weeks".
#'
#' @exportClass TRS
.trs_class = setClass("TRS",
                      representation(t0="POSIXt",
                                     tUnit = "character",
                                     tResolution = "numeric",
                                     dimname = "character")
)
