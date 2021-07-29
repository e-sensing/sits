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

.check_num <- function(x,
                       allow_na = FALSE,
                       min = -Inf,
                       max = Inf,
                       len_min = 0,
                       len_max = 2^31,
                       msg = NULL) {

    call_name <- as.character(sys.call(-1))
    if (is.null(call_name))
        call_name <- as.character(sys.call(0))

    if (!is.null(msg))
        msg <- sprintf("%s: %s\n", call_name, msg)

    if (is.null(msg))
        msg <- sprintf("%s: ", call_name)

    assertthat::assert_that(
        is.numeric(x),
        msg = sprintf("%svalue is not a number.", msg)
    )

    assertthat::assert_that(
        min_len <= length(x) && length(x) <= max_len,
        msg = sprintf("%slength %s is not allowed [%s, %s].", msg, length(x),
                      len_min, len_max)
    )

    if (length(x) == 0)
        return(invisible(NULL))

    if (!allow_na)
        assertthat::assert_that(
            !any(is.na(x)),
            msg = sprintf("%sNA value is not allowed.",msg)
        )

    assertthat::assert_that(
        all(min <= x) && all(x <= max),
        msg = sprintf("%svalue is out of range [%s, %s].", msg, min, max)
    )

}

.check_chr <- function(x,
                       allow_na = FALSE,
                       allow_empty = TRUE,
                       choices = NULL,
                       min_len = 0,
                       max_len = 2^31,
                       msg = NULL) {

    call_name <- as.character(sys.call(-1))
    if (is.null(call_name))
        call_name <- as.character(sys.call(0))

    assertthat::assert_that(
        is.character(x),
        msg = sprintf("%svalue is not character type.", msg)
    )

    assertthat::assert_that(
        min_len <= length(x) && length(x) <= max_len,
        msg = sprintf("%slength %s is not allowed [%s, %s].", msg, length(x),
                      len_min, len_max)
    )

    if (length(x) == 0)
        return(invisible(NULL))

    if (!allow_na)
        assertthat::assert_that(
            !any(is.na(x)),
            msg = sprintf("%sNA value is not allowed.", msg)
        )

    if (!allow_empty)
        assertthat::assert_that(
            all(nchar(x) > 0),
            msg = sprintf("%sempty value is not allowed.", msg)
        )

    if (!is.null(choices))
        assertthat::assert_that(
            all(x %in% choices),
            msg = sprintf("%svalue %s is invalid.", msg, x[[1]])
        )
}
