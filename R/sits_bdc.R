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
    cube <- slider::slide_dfr(cube, function(tile) {
        tile$file_info[[1]]$path <- paste0(tile$file_info[[1]]$path,
                                           "?access_token=", access_key)
        return(tile)
    })

    return(cube)
}
