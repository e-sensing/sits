#' @name .message_invalid_param
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param param parameter name
#' @return A valid message
#' @keywords internal
#' @noRd
.message_invalid_param <- function(param) {
    # make default message
    paste0("invalid ", param, " parameter")
}
