#' @title Check access rigths on BDC
#' @name  .sits_bdc_access_check
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return  access_key
#'
.sits_bdc_access_check <- function() {

    # Try to find the access key as an environment variable
    bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
    assertthat::assert_that(nchar(bdc_access_key) != 0,
                            msg = "BDC_ACCESS_KEY needs to be provided"
    )
    # test file access to BDC
    test_file <- paste0(.sits_config_test_file("BDC"),
                        "?access_token=", bdc_access_key
    )

    # are the files accessible?
    tryCatch({
        .sits_raster_api_check_access(test_file)
    },
    error = function(e) {
        msg <- paste0("Error in accessing BDC")
        message(msg)
    }
    )

    return(bdc_access_key)
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

    # append access token to path
    cube <- slider::slide_dfr(cube, function(tile){
        tile$file_info[[1]]$path <- paste0(tile$file_info[[1]]$path,
                                           "?access_token=",access_key)
        tile
    })

    return(cube)
}
