#' @title Check access rights on AWS
#' @name  .sits_aws_check_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param type           cube type
#'
#' @return  TRUE if access is granted
#'
.sits_aws_check_access <- function(type) {

    # require package
    if (!requireNamespace("aws.s3", quietly = TRUE)) {
        stop("Please install package aws.s3", call. = FALSE)
    }
    # check "AWS_ACCESS_KEY_ID" - mandatory one per user
    aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
    assertthat::assert_that(nchar(aws_access_key_id) > 0,
            msg = "AWS_ACCESS_KEY_ID environment variable missing"
    )
    # check "AWS_SECRET_ACCESS_KEY" - mandatory one per user
    aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    assertthat::assert_that(nchar(aws_secret_access_key) > 0,
            msg = "AWS_SECRET_ACCESS_KEY environment variable missing"
    )
    # check "AWS_DEFAULT_REGION" - if not available, use the default
    aws_region <- Sys.getenv("AWS_DEFAULT_REGION")
    if (nchar(aws_region) == 0) {
        aws_region <- .sits_config_aws_default_region(type)
        Sys.setenv(AWS_DEFAULT_REGION = aws_region)
    }
    # check "AWS_ENDPOINT" - if not available, use the default
    aws_endpoint <- Sys.getenv("AWS_ENDPOINT")
    if (nchar(aws_endpoint) == 0) {
        aws_endpoint <- .sits_config_aws_endpoint(type)
        Sys.setenv(AWS_ENDPOINT = aws_endpoint)
    }
    # check "AWS_REQUEST_PAYER" - if not available, use the default
    aws_request_payer <- Sys.getenv("AWS_REQUEST_PAYER")
    if (nchar(aws_request_payer) == 0) {
        aws_request_payer <- .sits_config_aws_request_payer(type)
        Sys.setenv(AWS_REQUEST_PAYER = aws_request_payer)
    }
    test_file <- .sits_config_test_file(type)

    # are the files accessible?
    tryCatch({
        .sits_raster_api_check_access(test_file)
    },
    error = function(e) {
        msg <- paste0("Error in accessing AWS files")
        message(msg)
    }
    )
    return(TRUE)
}
