#' @title Check for request package availability
#' @name .request_check_package
#' @keywords internal
#' @noRd
#'
#' @return name of the package.
#' @export
.request_check_package.httr2 <- function() {
    # package namespace
    pkg_name <- "httr2"

    # check if terra package is available
    .check_require_packages(pkg_name)

    class(pkg_name) <- pkg_name

    return(invisible(pkg_name))
}

#' @title Perform a request using httr2 package
#' @name .request
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param req_obj  A request object.
#' @param ...      Additional parameters to be passed to httr2 package
#'
#' @return A httr2 response object.
#' @export
.request.httr2 <-  function(req_obj, ...) {
    httr2::req_perform(req_obj, ...)
}

#' @title Retry a GET requisition with httr2
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
#' @return A httr2 response object.
#' @export
.retry_request.httr2 <- function(url, n_tries = 10, sleep = 10, ...) {
    while (n_tries > 0) {
        out <- .get_request(url = url, ...)
        if (!.response_is_error(out)) {
            return(out)
        }
        n_tries <- n_tries - 1
        Sys.sleep(sleep)
    }
    return(out)
}

#' @title Make a GET requisition
#' @name .get_request
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param url      A character with URL.
#' @param query    A named list with values to be passed in query.
#' @param headers  A named list with values to be passed to headers.
#' @param ...      Additional parameters to be passed to httr2 package.
#' @param quiet    Quiet requisition? Default is TRUE.
#'
#' @return A httr2 response object.
#' @export
.get_request.httr2 <- function(url, query = NULL, headers = NULL, ...,
                               quiet = TRUE) {
    req_obj <- httr2::request(url)
    # Prepare query
    req_obj <- .prepare_null(query, .request_query(req_obj, query), req_obj)
    # Prepare headers
    req_obj <- .prepare_null(
        headers, .request_headers(req_obj, headers), req_obj
    )
    # Quiet requisition? zero verbosity means quiet request
    quiet <- .prepare_lgl(quiet, 0, 1)
    # Perform request
    .request(req_obj, verbosity = quiet, ...)
}

#' @title Add query values into a request httr2 object
#' @name .request_query
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param req_obj  A request object.
#' @param query    A named list with values to be passed in query.
#'
#' @return A httr2 request object.
#' @export
.request_query.httr2 <- function(req_obj, query) {
    # Force to a named list
    query <- as.list(query)
    # Append query into requisition
    httr2::req_url_query(req_obj, !!!query)
}

#' @title Add headers values into a request httr2 object
#' @name .request_headers
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param req_obj  A request object.
#' @param header   A named list with values to be passed in headers.
#'
#' @return A httr2 request object.
#' @export
.request_headers.httr2 <- function(req_obj, header) {
    # Force to a named list
    header <- as.list(header)
    # Create a default header
    default_value <- list(
        "User-Agent" =  "SITS-R-PACKAGE (github.com/e-sensing/sits)",
        "Accept" =  "*/*",
        "Connection" = "keep-alive"
    )

    header_values <- utils::modifyList(
        x = header,
        val = default_value
    )
    # Append headers into requisition
    httr2::req_headers(req_obj, !!!header_values)
}

#' @title Get response content from httr2 object
#' @name .response_content
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A httr2 response object.
#'
#' @return A list with content values returned by the response.
#' @export
.response_content.httr2 <- function(resp_obj) {
    content_fn <- .switch_content(
        resp_obj,
        "application/json" = httr2::resp_body_json,
        "application/x-www-form-urlencoded" = httr2::resp_body_html,
        "application/xml","text/xml" = httr2::resp_body_xml,
        default = httr2::resp_body_json
    )
    content_fn(resp_obj)
}

#' @title Get response status from httr2 object
#' @name .response_status
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A httr2 response object.
#'
#' @return A integer value returned by the response.
#' @export
.response_status.httr2 <- function(resp_obj) {
    httr2::resp_status(resp_obj)
}

#' @title Get TRUE/FALSE response status from httr2 object
#' @name .response_is_error
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A httr2 response object.
#'
#' @return A logical value returned by the response.
#' @export
.response_is_error.httr2 <- function(resp_obj) {
    httr2::resp_is_error(resp_obj)
}

#' @title A response checker status from httr2 object
#' @name .response_check_status
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A httr2 response object.
#'
#' @return An invisible logical or an error.
#' @export
.response_check_status.httr2 <- function(resp_obj) {
    httr2::resp_check_status(resp_obj)
}

#' @title Get response type from httr2 object
#' @name .response_content_type
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param resp_obj  A httr2 response object.
#'
#' @return An character with response type.
#' @export
.response_content_type.httr2 <- function(resp_obj) {
    httr2::resp_content_type(resp_obj)
}

#' @title Parse an URL
#' @name .url_parse
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param url A character with URL.
#'
#' @return An character vector with parsed URL.
.url_parse <- function(url) {
    httr2::url_parse(url)
}

#' @title Parse URL
#' @name .url_parse_query
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param url A character with URL query string.
#'
#' @return An character vector with parsed URL query string.
.url_parse_query <- function(url) {
    httr2::url_parse(url)
}

#' @title Build an URL
#' @name .url_build
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param url A character vector with URL.
#'
#' @return An character vector with created URL.
.url_build <- function(url) {
    httr2::url_build(url)
}

.switch_content <- function(resp_obj, ...) {
    switch(.response_content_type(resp_obj), ...)
}
