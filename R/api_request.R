#' @title Check for request package availability
#' @name .request_check_package
#' @keywords internal
#' @noRd
#'
#' @return name of the package.
.request_check_package <- function() {
    pkg_class <- .conf_request_pkg()
    class(pkg_class) <- pkg_class

    UseMethod(".request_check_package", pkg_class)
}

#' @title Perform a request
#' @name .request
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param req_obj  A request object.
#' @param ...      Additional parameters to be passed to httr2 package
#'
#' @return A response object.
.request <- function(req_obj, ...) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".request", pkg_class)
}

#' @title Retry a GET requisition
#' @name .retry_request
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param url      A character with URL.
#' @param n_tries  A integer with the number with tried requisitions.
#' @param sleep    A integer with sleep time in seconds.
#' @param ...      Additional parameters to be passed to httr2 package
#'
#' @return A response object returned by the requisition package
.retry_request <- function(url, n_tries = 10L, sleep = 10L, ...) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".retry_request", pkg_class)
}

#' @title GET requistion
#' @name .get_request
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param url      A character with URL.
#' @param query    A named list with values to be passed in query.
#' @param headers  A named list with values to be passed to headers.
#' @param ...      Additional parameters to be passed to the requisition
#'                 package, including \code{timeout}, the seconds the
#'                 request may take, and \code{path}, a file the body is
#'                 streamed to
#'
#' @return A response object returned by the requisition package
.get_request <- function(url, query = NULL, headers = NULL, ...) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".get_request", pkg_class)
}

#' @title HEAD requisition
#' @name .head_request
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param url      A character with URL.
#' @param headers  A named list with values to be passed to headers.
#' @param ...      Additional parameters to be passed to the requisition
#'                 package, including \code{timeout}, the seconds the
#'                 request may take
#'
#' @return A response object returned by the requisition package
.head_request <- function(url, headers = NULL, ...) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".head_request", pkg_class)
}

#' @title Add query values into a request object
#' @name .request_query
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param req_obj  A request object.
#' @param query    A named list with values to be passed in query.
#'
#' @return A request object returned by the requisition package.
.request_query <- function(req_obj, query) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".request_query", pkg_class)
}

#' @title Add headers values into a request object
#' @name .request_headers
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param req_obj  A request object.
#' @param header   A named list with values to be passed in headers.
#'
#' @return A request object returned by the requisition package.
.request_headers <- function(req_obj, header) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".request_headers", pkg_class)
}

#' @title Get response content from object
#' @name .response_content
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A response object.
#'
#' @return A list with content values returned by the response.
.response_content <- function(resp_obj) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".response_content", pkg_class)
}

#' @title Get response body as raw bytes
#' @name .response_body_raw
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param resp_obj  A response object.
#'
#' @return A raw vector.
.response_body_raw <- function(resp_obj) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".response_body_raw", pkg_class)
}

#' @title Get one response header
#' @name .response_header
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param resp_obj  A response object.
#' @param header    Header name, case does not matter.
#'
#' @return A character value, or NULL if the header is absent.
.response_header <- function(resp_obj, header) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".response_header", pkg_class)
}

#' @title Get response status from object
#' @name .response_status
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A response object.
#'
#' @return A integer value returned by the response.
.response_status <- function(resp_obj) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".response_status", pkg_class)
}

#' @title Get TRUE/FALSE response status from object
#' @name .response_is_error
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A response object.
#'
#' @return A logical value returned by the response.
.response_is_error <- function(resp_obj) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".response_is_error", pkg_class)
}

#' @title A response checker status from object
#' @name .response_status
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A response object.
#'
#' @return An invisible logical or an error.
.response_check_status <- function(resp_obj) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".response_check_status", pkg_class)
}

#' @title Get response type from object
#' @name .response_content_type
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A response object.
#'
#' @return An character with response type.
.response_content_type <- function(resp_obj) {
    # check package
    pkg_class <- .request_check_package()

    # call function
    UseMethod(".response_content_type", pkg_class)
}
