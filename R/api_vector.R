#' @title Supported vector packages
#' @keywords internal
#' @noRd
#' @return   Names of vector packages supported by sits
.vector_supported_packages <- function() {
    return("sf")
}

#' @title Check for vector package availability
#' @name .vector_check_package
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

.vector_open_vec <- function(file, ...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_open_vec", pkg_class)
}

.vector_read_vec <- function(file, ...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_read_vec", pkg_class)
}

.vector_write_vec <- function(v_obj, file, ...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_write_vec", pkg_class)
}

#' @title Raster package internal create raster object function
#' @name .vector_new_vec
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param ...           additional parameters to be passed to raster package
#'
#' @return               A vector object.
.vector_new_vec <- function(...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".raster_new_rast", pkg_class)
}

#' @name .vector_crs
#' @keywords internal
#' @noRd
.vector_crs <- function(v_obj, wkt = TRUE, ...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_crs", pkg_class)
}

#' @name .vector_bbox
#' @keywords internal
#' @noRd
.vector_bbox <- function(v_obj, ...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_bbox", pkg_class)
}

#' @name .vector_reproject
#' @keywords internal
#' @noRd
.vector_reproject <- function(v_obj, crs, ...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_reproject", pkg_class)
}
