#' @title Check access rigths on BDC
#' @name  .sits_bdc_access_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param access_key     BDC access key
#'
#' @return  access_key
#'
.sits_bdc_access_check <- function(access_key = NULL) {

    if (purrr::is_null(access_key)) {
        access_key <- Sys.getenv("BDC_SECRET_ACCESS_KEY")
        assertthat::assert_that(nchar(access_key) > 1,
                                msg = "BDC access key needs to be provided"
        )
    }
    else {
        Sys.setenv("BDC_SECRET_ACCESS_KEY" = access_key)
    }

    test_file <- paste0(.sits_config_test_file("BDC"),
                        "?access_token=", access_key
    )

    # are the files accessible?
    tryCatch({
        .sits_raster_api_check_access(test_file)
    },
    error = function(e) {
        msg <- paste0("Error in accessing cloud files")
        message(msg)
    }
    )
    return(access_key)
}
#' @title Include BDC access info
#' @name  .sits_bdc_access_info
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           BDC data cube
#' @param access_key     BDC access key
#'
#' @return  cube with additional information
#'
.sits_bdc_access_info <- function(cube, access_key) {

    # get the file information
    cube$file_info <- cube$file_info %>%
        purrr::map(function(file_info) {

            # append access token to path
            file_info$path <- paste0(file_info$path,"?access_token=",
                                     access_key)
            file_info
        })

    return(cube)
}
