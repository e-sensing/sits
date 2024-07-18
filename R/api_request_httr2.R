.request <-  function(req_obj, ...) {
    httr2::req_perform(req_obj, verbosity = 0, ...)
}

.retry_request <- function(url, n_tries = 10, sleep = 10, ...) {
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

.get_request <- function(url, query = NULL, headers = NULL, ...) {
    req_obj <- httr2::request(url)
    # Prepare query
    req_obj <- .prepare(query, .request_query(req_obj, query), req_obj)
    # Prepare headers
    req_obj <- .prepare(headers, .request_headers(req_obj, headers), req_obj)
    # Perform request
    .request(req_obj, ...)
}

.request_query <- function(req_obj, query) {
    httr2::req_url_query(req_obj, !!!query)
}

.request_options <- function(req_obj, options) {
    httr2::req_options(req_obj, !!!options)
}

.request_headers <- function(req_obj, ...) {
    dots <- as.list(...)
    default_value <- list(
        "User-Agent" =  "SITS-R-PACKAGE (github.com/e-sensing/sits)",
        "Accept" =  "*/*",
        "Connection" = "keep-alive"
    )

    header_values <- modifyList(
        x = dots,
        val = default_value
    )

    httr2::req_headers(req_obj, !!!header_values)
}

.switch_content <- function(resp_obj, ...) {
    switch(.response_content_type(resp_obj), ...)
}

.response_content <- function(resp_obj) {
    content_fn <- .switch_content(
        resp_obj,
        "application/json" = httr2::resp_body_json,
        "application/x-www-form-urlencoded" = httr2::resp_body_html,
        "application/xml","text/xml" = httr2::resp_body_xml,
        default = httr2::resp_body_json
    )
    content_fn(resp_obj)
}

.response_status <- function(resp_obj) {
    httr2::resp_status(resp_obj)
}

.response_is_error <- function(resp_obj) {
    httr2::resp_is_error(resp_obj)
}

.response_check_status <- function(resp_obj) {
    httr2::resp_check_status(resp_obj)
}

.response_content_type <- function(resp_obj) {
    httr2::resp_content_type(resp_obj)
}

.url_parse <- function(url) {
    httr2::url_parse(url)
}

.url_build <- function(url) {
    httr2::url_build(url)
}
