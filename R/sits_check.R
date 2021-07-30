#' @title Check access rights on AWS
#' @name  .aws_check_environment
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param source           data source
#' @param collection ...
#'
#' @return  TRUE if access is granted
.check_aws_environment <- function(source, collection) {

    # check "AWS_ACCESS_KEY_ID" - mandatory one per user
    aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
    assertthat::assert_that(
        nchar(aws_access_key_id) > 0,
        msg = paste(".sits_aws_check_access: AWS_ACCESS_KEY_ID environment",
                    "variable missing")
    )

    # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
    aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    assertthat::assert_that(
        nchar(aws_secret_access_key) > 0,
        msg = paste(".sits_aws_check_access: AWS_SECRET_ACCESS_KEY",
                    "environment variable missing")
    )

    # check "AWS_DEFAULT_REGION" - if not available, use the default
    aws_region <- Sys.getenv("AWS_DEFAULT_REGION")
    if (nchar(aws_region) == 0 ||
        !aws_region %in% .config_aws_default_region(source, collection)) {

        # get default aws region in config file
        aws_region <- .config_aws_default_region(source, collection)
        Sys.setenv(AWS_DEFAULT_REGION = aws_region)
    }

    # check "AWS_S3_ENDPOINT" - if not available, use the default
    aws_endpoint <- Sys.getenv("AWS_S3_ENDPOINT")
    if (nchar(aws_endpoint) == 0 ||
        !aws_endpoint %in% .config_aws_endpoint(source, collection)) {

        # get default aws endpoint in config file
        aws_endpoint <- .config_aws_endpoint(source, collection)
        Sys.setenv(AWS_S3_ENDPOINT = aws_endpoint)
    }

    # check "AWS_REQUEST_PAYER" - if not available, use the default
    aws_request_payer <- Sys.getenv("AWS_REQUEST_PAYER")
    if (nchar(aws_request_payer) == 0 ||
        !aws_request_payer %in% .config_aws_request_payer(source, collection)) {

        # get default aws request payer in config file
        aws_request_payer <- .config_aws_request_payer(source, collection)
        Sys.setenv(AWS_REQUEST_PAYER = aws_request_payer)
    }

    return(invisible(NULL))
}

#' @title Auxiliary check functions
#' @keywords internal
#'
#' @name check_functions
#'
#' @description Functions to check parameters from a specific type, for example,
#' for \code{.check_chr} functions the check is exclusive for character
#' type.
#'
#' @param allow_empty   A \code{logical} indicating if the check permits empty
#' list in check. Default is FALSE.
#' @param allow_na      A \code{logical} indicating if the check permits empty
#' NA values in check. Default is FALSE.
#' @param allow_null    A \code{logical} indicating if the check permits empty
#' NULL values in check. Default is FALSE.
#' @param allow_unnamed A \code{logical} indicating if the check permits empty
#' unnamed list in check. Default is FALSE.
#' @param choices       A atomic \code{vector} of characters indicating the
#' choices of user can provide in function parameter. Only works for character
#' check.
#' @param min           A atomic \code{vector} of numeric indicating the
#' minimum value that the user can provide in function parameter. Only works for
#' numeric check. By default is \code{-Inf}.
#' @param max           A atomic \code{vector} of numeric indicating the
#' maximum value that the user can provide in function parameter. Only works for
#' numeric check. By default is \code{Inf}.
#' @param min_len       A \code{numeric} indicating the minimum length of vector
#' or list users provides for functions. Default is \code{0}.
#' @param max_len       A \code{numeric} indicating the maximum length of vector
#' or list users provides for functions. Default is \code{2^31}.
#' @param msg           A \code{character} with the error message that will show
#' to the user.
#' @param x             A \code{object} that will be check. That can be a
#' \code{numeric} or \code{character} vectors or a \code{list}.
NULL

#' @rdname check_functions
.check_num <- function(x,
                       allow_na = FALSE,
                       min = -Inf,
                       max = Inf,
                       min_len = 0,
                       max_len = 2^31,
                       allow_null = FALSE,
                       msg = NULL) {

    if (allow_null && is.null(x))
        return(invisible(NULL))

    call_name <- as.character(sys.call(-1))
    if (is.null(call_name))
        call_name <- as.character(sys.call(0))

    if (!is.null(msg))
        msg <- sprintf("%s: %s (%%s).", call_name, msg)

    if (is.null(msg))
        msg <- sprintf("%s: %%s.", call_name)

    local_msg <- "value is not a number"
    assertthat::assert_that(
        is.numeric(x),
        msg = sprintf(msg, local_msg)
    )

    local_msg <- sprintf("length %s is not allowed", length(x))
    assertthat::assert_that(
        min_len <= length(x) && length(x) <= max_len,
        msg = sprintf(msg, local_msg)
    )

    # exit if value has length zero (all checks were done)
    if (length(x) == 0)
        return(invisible(NULL))

    if (!allow_na) {
        local_msg <- "NA value is not allowed"
        assertthat::assert_that(
            !any(is.na(x)),
            msg = sprintf(msg, local_msg)
        )
    }

    local_msg <- "value is out of range"
    assertthat::assert_that(
        all(min <= x) && all(x <= max),
        msg = sprintf(msg, local_msg)
    )

    return(invisible(NULL))
}

#' @rdname check_functions
.check_chr <- function(x,
                       allow_na = FALSE,
                       allow_empty = TRUE,
                       choices = NULL,
                       min_len = 0,
                       max_len = 2^31,
                       allow_null = FALSE,
                       msg = NULL) {

    if (allow_null && is.null(x))
        return(invisible(NULL))

    call_name <- as.character(sys.call(-1))
    if (is.null(call_name))
        call_name <- as.character(sys.call(0))

    if (!is.null(msg))
        msg <- sprintf("%s: %s (%%s).", call_name, msg)

    if (is.null(msg))
        msg <- sprintf("%s: %%s.", call_name)

    local_msg <- "value is not character type."
    assertthat::assert_that(
        is.character(x),
        msg = sprintf(msg, local_msg)
    )

    local_msg <- sprintf("length %s is not allowed", length(x))
    assertthat::assert_that(
        min_len <= length(x) && length(x) <= max_len,
        msg = sprintf(msg, local_msg)
    )

    # exit if value has length zero (all checks were done)
    if (length(x) == 0)
        return(invisible(NULL))

    if (!allow_na) {
        local_msg <- "NA value is not allowed"
        assertthat::assert_that(
            !any(is.na(x)),
            msg = sprintf(msg, local_msg)
        )
    }

    if (!allow_empty) {
        local_msg <- "empty value is not allowed"
        assertthat::assert_that(
            all(nchar(x) > 0),
            msg = sprintf(msg, local_msg)
        )
    }

    if (!is.null(choices)) {
        local_msg <- sprintf("value must be one of %s",
                             paste0("'", choices, "'", collapse = ", "))
        assertthat::assert_that(
            all(x %in% choices),
            msg = sprintf(msg, local_msg)
        )
    }

    return(invisible(NULL))
}

#' @rdname check_functions
.check_lst <- function(x,
                       allow_unnamed = FALSE,
                       min_len = 0,
                       max_len = 2^31,
                       allow_null = FALSE,
                       msg = NULL) {

    if (allow_null && is.null(x))
        return(invisible(NULL))

    call_name <- as.character(sys.call(-1))
    if (is.null(call_name))
        call_name <- as.character(sys.call(0))

    if (!is.null(msg))
        msg <- sprintf("%s: %s (%%s).", call_name, msg)

    if (is.null(msg))
        msg <- sprintf("%s: %%s.", call_name)

    local_msg <- "value is not a list type."
    assertthat::assert_that(
        is.list(x),
        msg = sprintf(msg, local_msg)
    )

    local_msg <- sprintf("length %s is not allowed", length(x))
    assertthat::assert_that(
        min_len <= length(x) && length(x) <= max_len,
        msg = sprintf(msg, local_msg)
    )

    # exit if value has length zero (all checks were done)
    if (length(x) == 0)
        return(invisible(NULL))

    if (!allow_unnamed) {
        local_msg <- "list must have named values"
        assertthat::assert_that(
            !is.null(names(x)) || !any(is.na(names(x))),
            msg = sprintf(msg, local_msg)
        )
    }

    return(invisible(NULL))
}
