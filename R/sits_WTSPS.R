#' @title Provides information about WTSPS service
#' @name sits_infoWTSPS
#'
#' @description uses the WTSPS services to print information about WTSPS
#'
#' @param URL             the URL for the WTSPS service
#' @return wtsps.obj       an R object containing the information about the WTSPS server
#' @export
sits_infoWTSPS <- function (URL = "http://www.dpi.inpe.br/tws/wtsps") {

     # obtains information about the WTSS service
     wtsps.obj         <- wtsps.R::WTSPS(URL)
     show(wtsps.obj) # prints wtsps.obj

     return (invisible(wtsps.obj))
}

#' @title Provides information about one algorithm of the WTSPS service
#' @name sits_algorithmWTSPS
#'
#' @description uses the WTSPS services to print information about a
#' chosen algorithm:
#'  name
#'  input_parametes
#'  output
#'  description
#'
#' @param URL        the URL for the WTSPS service
#' @param algorithm   the name of the algorithm
#' @export
sits_algorithmWTSPS <- function (URL = "http://www.dpi.inpe.br/tws/wtsps", algorithm = NULL) {

     # describe the algorithm
     algorithm.obj    <- wtsps.R::describeAlgorithm(URL, algorithm)
     show(algorithm.obj) # prints algorithm.obj

     return (invisible(algorithm.obj))
}

#' @title Runs a process in the WTSPS service
#' @name sits_runProcess_WTSPS
#'
#' @description uses the WTSPS services to run a process in the server
#'
#' @param URL        the URL for the WTSPS service
#' @param ...       algorithm name and its parameters
#' @export
sits_runProcess_WTSPS <- function (URL = "http://www.dpi.inpe.br/tws/wtsps", ...) {

     # runs process
     proc.obj <- wtsps.R::runProcess(URL, ...)
     show(proc.obj) # prints process obj

     return (invisible(proc.obj))
}

#' @title Provides information about a process running in the WTSPS service
#' @name sits_statusProcess_WTSPS
#'
#' @description uses the WTSPS services to get the status of a running process in the server
#'
#' @param URL        the URL for the WTSPS service
#' @param uuid         the id of the process
#' @export
sits_statusProcess_WTSPS <- function (URL = "http://www.dpi.inpe.br/tws/wtsps", uuid = NULL) {

     # get status from a process
     proc.obj <- wtsps.R::statusProcess(URL, uuid)
     show(proc.obj)  # prints process obj

     return (invisible(proc.obj))
}

#' @title Cancels a process running in the WTSPS service
#' @name sits_cancelProcess_WTSPS
#'
#' @description uses the WTSPS services to cancel a process in the server
#'
#' @param URL        the URL for the WTSPS service
#' @param uuid         the id of the process
#' @export
sits_cancelProcess_WTSPS <- function (URL = "http://www.dpi.inpe.br/tws/wtsps", uuid = NULL) {

     # cancel a process
     proc.obj <- wtsps.R::cancelProcess(URL, uuid)
     show(proc.obj)  # prints process obj

     return (invisible(proc.obj))
}
