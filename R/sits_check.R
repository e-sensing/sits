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


.check_that <- function(x, ...,
                        local_msg = NULL,
                        msg = NULL) {

    if (!is.logical(x))
        stop(".check_that: expression must be logical.", call. = TRUE)

    if (!x) {

        # get caller function name
        calls <- sys.calls()
        calls <- gsub(pattern = "^(.*)\\(.*$",
                      replacement = "\\1",
                      x = paste(calls))
        checks <- grepl(pattern = "^\\.check_", calls)
        index <- 1
        if (!all(checks))
            index <- which.max(!checks)

        # format error message
        if (is.null(msg))
            msg <- sprintf("%s: %%s.", calls[[index]])
        else
            msg <- sprintf("%s: %s (%%s).", calls[[index]], msg)

        if (is.null(local_msg)) {
            expr <- deparse(substitute(expr = x, env = environment()))
            local_msg <- sprintf("%s is not TRUE", expr)
        }

        stop(sprintf(msg, local_msg), call. = FALSE)
    }

    return(invisible(NULL))
}

.check_null <- function(x, ...,
                        allow_null = FALSE,
                        msg = NULL) {

    if (!allow_null)
        .check_that(
            !is.null(x),
            local_msg = "NULL value is not allowed",
            msg = msg
        )

    return(invisible(NULL))
}

.check_na <- function(x, ...,
                      allow_na = FALSE,
                      msg = NULL) {

    if (!allow_na)
        .check_that(
            !any(is.na(x)),
            local_msg = "NA value is not allowed",
            msg = msg
        )

    return(invisible(NULL))
}


.check_names <- function(x, ...,
                         is_named = TRUE,
                         msg = NULL) {

    if (is_named)
        .check_that(
            !is.null(names(x)) || !any(is.na(names(x))),
            local_msg = "value must have names",
            msg = msg
        )
    else
        .check_that(
            is.null(names(x)),
            local_msg = "value must be unnamed",
            msg = msg
        )

    return(invisible(NULL))
}

.check_length <- function(x, ...,
                          len_min = 0,
                          len_max = 2^31,
                          msg = NULL) {

    if (len_min == len_max)
        local_msg <- sprintf("length must be == %s", len_min)
    else if (missing(len_min) && missing(len_max))
        local_msg <- "invalid length" # never throws an error in this case!
    else if (missing(len_max))
        local_msg <- sprintf("length must be >= %s", len_min)
    else if (missing(len_min))
        local_msg <- sprintf("length must be <= %s", len_max)
    else
        local_msg <- sprintf("length must be between %s and %s",
                             len_min, len_max)

    .check_that(
        len_min <= length(x) && length(x) <= len_max,
        local_msg = local_msg,
        msg = msg
    )
}

.check_apply <- function(x, fn_check, ...,
                         msg = NULL) {

    if (!is.function(fn_check))
        stop(".check_apply: fn_check must be a function.", call. = TRUE)

    values <- tryCatch({
        all(vapply(x, fn_check, logical(1), ...))
    }, error = function(e) {
        .check_that(FALSE,
                    local_msg = e$message,
                    msg = msg)
    })

    .check_that(all(values),
                local_msg = "not all values passed in the test",
                msg = msg)

    return(invisible(NULL))
}

.check_lgl_type <- function(x, ...,
                            msg = NULL) {

    .check_that(
        is.logical(x),
        local_msg = "value is not logical",
        msg = msg
    )
}

.check_num_type <- function(x, ...,
                            msg = NULL) {

    .check_that(
        is.numeric(x),
        local_msg = "value is not a number",
        msg = msg
    )
}

.check_int_type <- function(x, ...,
                            msg = NULL) {

    .check_that(
        is.numeric(x) && x == as.integer(x),
        local_msg = "value is not integer",
        msg = msg
    )
}

.check_num_range <- function(x, ...,
                             min = -Inf,
                             max = Inf,
                             allow_zero = TRUE,
                             msg = NULL) {

    # check type
    .check_num_type(x)

    .check_that(
        all(min <= x) && all(x <= max),
        local_msg = "value out of range",
        msg = msg
    )

    if (!allow_zero)
        .check_that(
            all(x != 0),
            local_msg = "value cannot be zero",
            msg = msg
        )

}

.check_chr_type <- function(x, ...,
                            msg = NULL) {

    .check_that(
        is.character(x),
        local_msg = "value is not character type",
        msg = msg
    )
}

.check_chr_empty <- function(x, ...,
                             msg = NULL) {

    # check type
    .check_chr_type(x, msg = msg)

    .check_that(
        all(nchar(x) > 0),
        local_msg = "empty value is not allowed",
        msg = msg
    )
}

.check_chr_choices <- function(x,
                               choices, ...,
                               msg = NULL) {

    if (!is.character(choices))
        stop(".check_chr_choices: choices must be character.", call. = TRUE)

    # check type
    .check_chr_type(x, msg = msg)

    if (length(choices) > 0)
        local_msg <- sprintf("value must be one of %s",
                             paste0("'", choices, "'", collapse = ", "))
    else
        local_msg <- sprintf("value cannot be %s",
                             paste0("'", x, "'", collapse = ", "))
    .check_that(
        all(x %in% choices),
        local_msg = local_msg,
        msg = msg
    )
}

.check_lst_type <- function(x, ...,
                            msg = NULL) {

    .check_that(
        is.list(x),
        local_msg = "value is not a list",
        msg = msg
    )
}

.check_lgl <- function(x, ...,
                       allow_na = FALSE,
                       len_min = 0,
                       len_max = 2^31,
                       allow_null = FALSE,
                       is_named = FALSE,
                       msg = NULL) {

    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x))
        return(invisible(NULL))

    # check NULL
    .check_null(x, allow_null = allow_null, msg = msg)

    # check type
    .check_lgl_type(x, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    .check_na(x, allow_na = allow_na, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    return(invisible(NULL))
}

.check_num <- function(x, ...,
                       allow_na = FALSE,
                       min = -Inf,
                       max = Inf,
                       allow_zero = TRUE,
                       len_min = 0,
                       len_max = 2^31,
                       allow_null = FALSE,
                       is_integer = FALSE,
                       is_named = FALSE,
                       msg = NULL) {

    # check for NULL and exit if it is allowed
    if (allow_null && is.null(x))
        return(invisible(NULL))

    # check NULL
    .check_null(x, allow_null = allow_null, msg = msg)

    # check type
    if (is_integer)
        .check_int_type(x, msg = msg)
    else
        .check_num_type(x, msg = msg)

    # check length
    .check_length(x, len_min = len_min, len_max = len_max, msg = msg)

    # check NA
    .check_na(x, allow_na = allow_na, msg = msg)

    # check range
    .check_num_range(x, min = min, max = max, allow_zero = allow_zero,
                     msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    return(invisible(NULL))
}

.check_chr <- function(x, ...,
                       allow_na = FALSE,
                       allow_empty = TRUE,
                       choices = NULL,
                       len_min = 0,
                       len_max = 2^31,
                       allow_null = FALSE,
                       is_named = FALSE,
                       msg = NULL) {

    # check for null and exit if it is allowed
    if (allow_null && is.null(x))
        return(invisible(NULL))

    # check NULL
    .check_null(x, allow_null = allow_null, msg = msg)

    # check type
    .check_chr_type(x, msg = msg)

    # check length
    .check_length(x, msg = msg)

    # check NA
    .check_na(x, allow_na = allow_na, msg = msg)

    # check empty
    if (!allow_empty)
        .check_chr_empty(x, msg = msg)

    if (!is.null(choices))
        .check_chr_choices(x, choices = choices, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    return(invisible(NULL))
}

.check_lst <- function(x, ...,
                       min_len = 0,
                       max_len = 2^31,
                       allow_null = FALSE,
                       is_named = TRUE,
                       fn_check = NULL,
                       msg = NULL) {

    if (allow_null && is.null(x))
        return(invisible(NULL))

    # check NULL
    .check_null(x, allow_null = allow_null, msg = msg)

    # check type
    .check_lst_type(x, msg = msg)

    # check length
    .check_length(x, msg = msg)

    # check names
    .check_names(x, is_named = is_named, msg = msg)

    # check using function
    if (!is.null(fn_check))
        .check_apply(x, fn_check = fn_check, msg = msg, ...)

    return(invisible(NULL))
}

.check_file <- function(x, ...,
                        msg = NULL) {

    # check type
    .check_chr(x, allow_empty = FALSE, len_min = 1,
               len_max = 1, allow_null = FALSE, msg = msg)

    .check_that(
        file.exists(x),
        local_msg = sprintf("file %s does not exist", x),
        msg = msg
    )
}

