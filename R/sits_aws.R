#' @title Check access rights on AWS
#' @name  .sits_aws_check_access
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param type           cube type
#' @param access_key     AWS access key
#' @param secret_key     AWS secret key
#' @param region         AWS region
#'
#' @return  TRUE if access is granted
#'
.sits_aws_check_access <- function(type,
                                   access_key = NULL,
                                   secret_key = NULL,
                                   region = NULL) {

    # require package
    if (!requireNamespace("aws.s3", quietly = TRUE)) {
        stop("Please install package aws.s3", call. = FALSE)
    }
    if (purrr::is_null(access_key)) {
        env_access_key <- Sys.getenv("AWS_ACCESS_KEY_ID")
        assertthat::assert_that(nchar(env_access_key) > 1,
                                msg = "AWS access key needs to be provided"
        )
    }
    else {
        Sys.setenv("AWS_ACCESS_KEY_ID", access_key)
    }

    if (purrr::is_null(secret_key)) {
        env_secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
        assertthat::assert_that(nchar(env_secret_key) > 1,
                                msg = "AWS secret key needs to be provided"
        )
    }
    else {
        Sys.setenv("AWS_SECRET_ACCESS_KEY", secret_key)
    }

    if (purrr::is_null(region)) {
        env_region <- Sys.getenv("AWS_DEFAULT_REGION")
        assertthat::assert_that(nchar(env_region) > 1,
                                msg = "AWS region needs to be provided"
        )
    }
    else {
        Sys.setenv("AWS_DEFAULT_REGION", region)
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
