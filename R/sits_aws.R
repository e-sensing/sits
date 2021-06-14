#' @title Check access rights on AWS
#' @name  .sits_aws_check_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param source           data source
#'
#' @return  TRUE if access is granted
#'
.sits_aws_check_access <- function(source) {

    # require package
    if (!requireNamespace("aws.s3", quietly = TRUE)) {
        stop(".sits_aws_check_access: please install package aws.s3",
             call. = FALSE)
    }

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
    if (nchar(aws_region) == 0) {
        aws_region <- .sits_config_aws_default_region(source)
        Sys.setenv(AWS_DEFAULT_REGION = aws_region)
    }

    # check "AWS_S3_ENDPOINT" - if not available, use the default
    aws_endpoint <- Sys.getenv("AWS_S3_ENDPOINT")
    if (nchar(aws_endpoint) == 0) {
        aws_endpoint <- .sits_config_aws_endpoint(source)
        Sys.setenv(AWS_S3_ENDPOINT = aws_endpoint)
    }

    # check "AWS_REQUEST_PAYER" - if not available, use the default
    aws_request_payer <- Sys.getenv("AWS_REQUEST_PAYER")
    if (nchar(aws_request_payer) == 0) {
        aws_request_payer <- .sits_config_aws_request_payer(source)
        Sys.setenv(AWS_REQUEST_PAYER = aws_request_payer)
    }
    return(TRUE)
}
