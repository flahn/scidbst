if(!isGeneric("stream")) {
  setGeneric("stream",function(x,f,...){
    standardGeneric("stream")
  })
}

# x: scidbst
# packages: vector of strings with the package names
# logging: boolean, whether or not to log information
# logfile: string, the path to the log file, necessary if logging enabled
# returns a string command stack
.stream.install.r.pkg = function(packages,logging=FALSE,logfile) {
  # if (logging && (missing(logfile) || is.null(logfile) || !is.character(logfile)))
  .install.scidbstrm = "if (! require(\"scidbstrm\")) { if (! require(\"jsonlite\")) { install.packages(\"jsonlite\",repos=\"https://cloud.r-project.org/\") }; devtools::install_github(\"paradigm4/stream\", subdir=\"r_pkg\"); require(\"scidbstrm\") };"

  if (!missing(packages) || !is.null(packages)) {
    if (logging) {
      .install.packages = .appendUserDefinedRequiredPackages(.install.scidbstrm, packages, logfile)
    } else {
      .install.packages = .appendUserDefinedRequiredPackages(.install.scidbstrm, packages)
    }
  } else {
    .install.packages = .install.scidbstrm
  }
  return(.install.packages)

  # .identity.function = function (x) {
  #   data.frame(x=x)
  # }
  # .install.script = .appendUserDefinedFunctionCode(.install.packages, .identity.function)
  # .install.script = append("map(f)")
  #
  # .install.query = sprintf("stream(%s, R --slave -e %s, 'format=df','names=%s')",
  #                          x@proxy@name,
  #                          .install.script,
  #                          paste(scidb_attributes(x),collapse=","))
  # return(.install.query)
}

.stream.script = function(x,f,packages,parallel=FALSE,cores=1,aggregates,output, logfile, ...) {
  commands = c()
  logging = !missing(logfile) && !is.null(logfile)
  ### 1. require/install packages ###
  if (!logging) {
    logfile = NULL
  }
  if (missing(packages)) packages = c()

  logging = !missing(logfile) || !is.null(logfile)


  if (!("plyr" %in% packages)) {
    packages = append(packages,"plyr")
  }
  if (!("foreach" %in% packages)) {
    packages = append(packages,"foreach")
  }

  parallel = parallel && cores > 1 # don't use parallel if the cores are set to 1
  if (parallel) {
    if (!("doParallel" %in% packages)) {
      packages = append(packages,"doParallel")
    }
  }

  # make sure the packages are installed at each instance by passing the data thorugh without change
  if(logging) {
    .stream.installer = .stream.install.r.pkg(packages,logging,logfile)
  } else {
    .stream.installer = .stream.install.r.pkg(packages)
  }
  # .stream.installer contains now the R code to require/install the listed packages. We need this as the first building
  # block of our script template
  commands = append(commands,.stream.installer)

  ### 2. additional declarations (functions, constants, etc.) ###
  # version 1: store the definition of parameter 'f'
  commands = .appendUserDefinedFunctionCode(commands,f)
  # version 2: generic
  #TODO

  ### 3. instance function declaration ###
  # function that is applied on each chunk
  # version 1: f now is the function that is passed on to ddply
  ddply = paste(.createDDPLYCommand(aggregates=aggregates,
                      parallel=parallel,
                      df.name="x",
                      logging=logging,
                      logfile = logfile,
                      output=output),collapse="; ",sep="")
  commands = append(commands,sprintf("scidb.instance.function <- function(x) { %s }",ddply))
  #version 2: generic
  #TODO

  ### 4. optional final function ###
  # that is executed at the end, when all chunks are gathered. (Probably requires the use of _sg operator)
  #TODO

  ### 5. run the map command of scidbstrm ###
  commands = append(commands,"map(scidb.instance.function)")



  #create r script according to the
  #TODO use also function for final
  stream.script = paste(commands,sep="",collapse=";")

  # clean accidential ; and ;;
  stream.script = gsub(pattern="\\)\\s*(;)\\s*\\{",replacement=") {",x=stream.script)
  stream.script = gsub(pattern="(;;)",replacement=";",x=stream.script)
  stream.script = gsub(pattern="(\\{\\s*;)",replacement="{",x=stream.script)
  stream.script = gsub(pattern="\\;\\s*\\;",replacement="; ",x=stream.script)

  #prepare for URL encoding
  # stream.script = URLencode(stream.script,reserved=TRUE)


  return(stream.script)
}

# x: scidbst array
# f: user defined function
# array: the array name to store the results under
# packages: the names of the R packages that are used during the function call
# parallel: property in ddply to perform parallel processing
# cores: number of cores used for parallel processing on a single instance
# aggregates: the attribute names which are used to group by
# output: a list with the name and the data type in SciDB as key-value pairs
# logfile: the file path used to log during the processing
.iquery.stream.script = function(x,f,array,packages=c(),parallel=FALSE,cores=1,aggregates=c(),output, logfile=NULL, ...) {

  if (class(x) == "scidbst") {
    nrep = length(scidb_attributes(x))
  } else if (class(x)=="scidb"){
    nrep = length(scidb::scidb_attributes(x))
  } else {
    stop("Parameter 'x' in of the query call is no scidb or scidbst array.")
  }

  if (missing(output)) {

    output = as.list(rep("double",nrep))
  }

  # create the r script that is passed as an expression to the command line call of each scidb instance
  .rscript = .stream.script(x=x@proxy,
                            f=f,
                            packages=packages,
                            parallel=parallel,
                            cores=cores,
                            aggregates=aggregates,
                            output=output,
                            logfile=logfile, ...)

  #create scidb output data.frame types from output
  types = paste(output[],collapse=",")

  #prepare the iquery command like this:   stream(ARRAY, 'COMMAND', 'format=df', 'types=int32,int32,string', ADDITIONAL_ARRAY)
  .rscript = gsub(pattern="\\\"",replacement="\\\\\"",x=.rscript)
  encodedScript = URLencode(sprintf("R --slave -e \"%s\"",.rscript),reserved=T)
  # encodedScript = sprintf("R --slave -e \"%s\"",.rscript)
  iquery.cmd = sprintf("store(stream(%s, '%s', 'format=df', 'types=%s'),%s)",x@proxy@name, encodedScript,types,array)

  return(iquery.cmd)
}

# #' Apply a R script on all chunks of an array
# #'
# #' This function will execute a R function within a ddply call to perform higher level analysis using the [streaming interface of SciDB](https://github.com/Paradigm4/stream).
# #' @aliases stream,scidbst
# #' @param  x scidbst array
# #' @param array the array name to store the results under
# #' @param packages the names of the R packages that are used during the function call
# #' @param parallel property in ddply to perform parallel processing
# #' @param cores number of cores used for parallel processing on a single instance
# #' @param aggregates the attribute names which are used to group by
# #' @param output a list with the name and the data type in SciDB as key-value pairs
# #' @param logfile the file path used to log during the processing
# #' @seealso \link[scidbst]{r.apply}
# #'
# setMethod("stream",signature(x="scidbst",f="function"), .iquery.stream.script)
