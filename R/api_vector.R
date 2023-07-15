#' @title Supported raster packages
#' @keywords internal
#' @noRd
#' @return   Names of raster packages supported by sits
.vector_supported_packages <- function() {
    return("sf")
}

#' @title Check for raster package availability
#' @name .raster_check_package
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @return name of the package.
.vector_check_package <- function() {
    pkg_class <- .conf_vector_pkg()
    class(pkg_class) <- pkg_class

    UseMethod(".vector_check_package", pkg_class)
}

.vector_open_vec <- function(file, layer = NULL) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_open_vec", pkg_class)
}

