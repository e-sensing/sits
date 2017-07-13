# create constructor class
setClass(

     # set the name for the class
     Class = "WTSPS",

     # define the slots
     slots = c(
          serverURL = "character",
          listAlgorithms = "character"
     ),

     # set the default values for the slots.
     prototype = list(),

     # test if the data is consistent. Only called if no initialize function is defined.
     validity = function(object) {

          if (length(object@serverURL) != 1) {
               stop("[WTSPS: validation] Invalid server URL.")
          }

          return (TRUE)

     }

)

# set constructor
setMethod(

     # initialize function
     f = "initialize",

     # Method signature
     signature = "WTSPS",

     # Function definition
     definition = function(.Object, serverURL) {

          # check whether the URL is missing or not
          if (!missing(serverURL)) {
               .Object@serverURL <- serverURL
               arrays <- listAlgorithms(.Object)
               methods::validObject(.Object)
          }
          else {
               .Object@serverURL <- character(0)
          }

          return(.Object)
     }

)

# shorcut function name for creating a new WTSPS instance
WTSPS <- function(serverURL) { new (Class = "WTSPS", serverURL = serverURL)}

# method for customizing print
setMethod(

     # name of the function
     f = "show",

     # method signature
     signature = "WTSPS",

     # stylish print of the objects
     definition = function(object) {

          # initial message
          cat(paste("Object of Class WTSPS\n\n"))

          # print serverURL
          cat(paste("serverURL: ", paste(object@serverURL),  "\n"))

          # print listAlgorithms
          cat("listAlgorithms: ")
          cat(paste(object@listAlgorithms), " ")

          return(invisible())
     }
)

#' Returns the WTSPS object's server URL
#'
#' @param object A WTSPS object
#' @return serverURL an URL character of a WTSPS server
#' @export
setGeneric(name = "getServerURL",

           def = function(object) {

                standardGeneric ("getServerURL")
           }
)

#' @rdname getServerURL
setMethod(f = "getServerURL",

          signature = "WTSPS",

          definition = function(object) {

               # return serverURL
               return(object@serverURL)

          }
)

#' Set WTSPS object's server URL
#'
#' @param object A WTSPS object
#' @param serverURL URL character
#' @export
setGeneric(name = "setServerURL",

           def = function(object, serverURL) {

                standardGeneric ("setServerURL")
           }
)

#' @rdname setServerURL
setMethod(f = "setServerURL",

          signature = "WTSPS",

          definition = function(object, serverURL) {

               # assign server
               object@serverURL <- serverURL

          }
)

#' Returns a list of algorithms contained in WTSPS
#'
#' @param object A WTSPS object
#' @export
setGeneric(name = "listAlgorithms",

           def = function(object) {

                standardGeneric ("listAlgorithms")
           }
)

#' @rdname listAlgorithms
setMethod(f = "listAlgorithms",

          signature = "WTSPS",

          definition = function(object) {

               # get server url from object WTSPS
               serverURL <- getServerURL(object)

               # concat URL to list_algorithm operator
               requestHTTP <- paste(serverURL, "list_algorithms", sep="")

               # send requestHTTP and parse JSON
               responseHTTP <- .parseJSON(.sendRequest(requestHTTP))
          }
)

#' Returns a description of a queried algorithm contained in WTSPS
#'
#' @param object a WTSPS object
#' @param algorithm a specific algorithm
#' @export
setGeneric(name = "describeAlgorithm",

           def = function(object, algorithm) {

                standardGeneric ("describeAlgorithm")
           }
)

#' @rdname describeAlgorithm
setMethod(f = "describeAlgorithm",

          signature = "WTSPS",

          definition = function(object, algorithm) {

               # get server url from object WTSPS
               serverURL <- getServerURL(object)

               # concat describe_algorithm
               requestHTTP <- paste(serverURL, "describe_algorithm?name=", algorithm, sep="")

               # send HTTP request and parse json response
               responseHTTP <- .parseJSON(.sendRequest(requestHTTP))
          }
)

#' Returns the status of a processing algorithm
#'
#' @param object A WTSPS object
#' @param id a process identifier
#' @export
setGeneric(name = "statusProcessing",

           def = function(object, id) {

                standardGeneric("statusProcessing")
           }
)

#' @rdname statusProcessing
setMethod(f = "statusProcessing",

          signature = "WTSPS",

          definition = function(object, id) {

               # get server url from object WTSPS
               serverURL <- getServerURL(object)

               # build algorithm request URL
               requestHTTP <- paste(serverURL, "statusProcessing?uuid=", id, sep="")

               # send HTTP request and parse json response
               responseHTTP <- .parseJSON(.sendRequest(requestHTTP))

          }
)

#' Returns the status of a processing algorithm
#'
#' @param object A WTSPS object
#' @param query query with algorithm and required parameters
#' @export
setGeneric(name = "executeAlgorithm",

           def = function(object, query) {

                standardGeneric("executeAlgorithm")
           }
)

#' @rdname executeAlgorithm
setMethod(f = "executeAlgorithm",

          signature = "WTSPS",

          definition = function(object, query) {

               # get server url from object WTSPS
               serverURL <- getServerURL(object)

               # build algorithm request URL
               requestHTTP <- paste(serverURL,"executeAlgorithm?", query, sep="")

               # send HTTP request and parse json response
               responseHTTP <- .parseJSON(.sendRequest(requestHTTP))

          }
)

.sendRequest <- function(request) {

     # check if URL exists and the web service is running
     tryCatch(response <- RCurl::getURL(request), error = function(e) {
          e$message <- paste("HTTP request failed. The URL server may be incorrect or the service may be temporarily unavailable.");
          stop(e);
     })

     return(response)

}

.parseJSON <- function(response) {

     # validate json
     if (jsonlite::validate(response)) {
          json_response <- jsonlite::fromJSON(response)
          if("exception" %in% names(json_response))
               stop(json_response)
          return(json_response)
     }
     else
          stop(response)

}
