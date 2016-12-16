if(!isGeneric("stream")) {
  setGeneric("stream",function(x,f,...){
    standardGeneric("stream")
  })
}

# x: scidbst
# packages: vector of strings with the package names
# logging: boolean, whether or not to log information
# logfile: string, the path to the log file, necessary if logging enabled
.stream.install.r.pkg = function(x,packages,logging=FALSE,logfile) {
  if (logging && (missing(logfile) || is.null(logfile) || !is.character(logfile)))
  .install.scidbstrm = "if (!require(\"scidbstrm\") {
        install.packages(\"jsonlite\")
        devtools::install_github('paradigm4/stream', subdir='r_pkg')
        require(\"scidbstrm\")
      }"

  if (!missing(packages) || !is.null(packages)) {
    if (logging) {
      .install.packages = .appendUserDefinedRequiredPackages(.install.scidbstrm, packages, logfile)
    } else {
      .install.packages = .appendUserDefinedRequiredPackages(.install.scidbstrm, packages)
    }
  } else {
    .install.packages = .install.scidbstrm
  }
  .identity.function = function (x) {
    data.frame(x=x)
  }
  .install.script = .appendUserDefinedFunctionCode(.install.packages, .identity.function)
  .install.script = append("map(f)")

  .install.query = sprintf("stream(%s, R --slave -e %s, 'format=df','names=%s')",
                           x@proxy@name,
                           .install.script,
                           paste(scidb_attributes(x),collapse=","))
  return(.install.query)
}

.stream.script = function(x,f,array,packages,parallel=FALSE,cores=1,aggregates,output, logfile, ...) {
  if (missing(packages)) packages = c()

  .required.pkgs = c("plyr","foreach","doParallel")
  logging = !missing(logfile) || !is.null(logfile)
  #"stream(ARRAY, 'COMMAND', 'format=df', 'types=int32,int32,string', ADDITIONAL_ARRAY)"

  parallel = parallel && cores > 1 # don't use parallel if the cores are set to 1
  if (parallel) {
    if (!("plyr" %in% packages)) {
      packages = append(packages,"plyr")
    }
    if (!("foreach" %in% packages)) {
      packages = append(packages,"foreach")
    }
    if (!("doParallel" %in% packages)) {
      packages = append(packages,"doParallel")
    }
  }



  # make sure the packages are installed at each instance by passing the data thorugh without change
  if(logging) {
    .stream.installer = .stream.install.r.pkg(x,packages,logging,logfile)
  } else {
    .stream.installer = .stream.install.r.pkg(x,packages)
  }
  # .stream.installer contains now the iquery string to perform an installation of the required

  .arr = NULL


  #create r script according to the
  #TODO use also function for final


}

#' @export
setMethod("stream",signature(x="scidbst",f="function"), .stream.script)
