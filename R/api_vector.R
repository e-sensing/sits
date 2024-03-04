#' @title Vector package
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @description  API to access vector files
#' @keywords internal
#' @noRd
#' @param file_path          Path to vector file.
#' @param v_obj              sf object
#' @param wkt                Information in WKT format?
#' @param crs                Desired CRS for object to be reprojected
#' @param ... Additional parameters.
NULL
#' @title Open a vector file
#' @noRd
#' @returns An sf object to vector file
.vector_open_vec <- function(file_path, ...) {
    sf::st_read(dsn = file_path, ..., quiet = TRUE)
}
#' @title Read a vector file
#' @noRd
#' @returns An sf object to vector file
.vector_read_vec <- function(file_path, ...) {
    sf::read_sf(dsn = file_path, ..., quiet = TRUE)
}
#' @title Write a vector file
#' @noRd
#' @returns NULL (called for side effects)
.vector_write_vec <- function(v_obj, file_path, ...) {
    sf::st_write(obj = v_obj, dsn = file_path, quiet = TRUE, ...)
}
#' @title Retrieve the CRS of a vector object
#' @noRd
.vector_crs <- function(v_obj, wkt = TRUE, ...) {
    crs <- sf::st_crs(v_obj, ...)
    if (wkt) {
        return(crs[["wkt"]])
    }
    crs
}

#' @title Retrieve the bbox of a vector object
#' @keywords internal
#' @noRd
.vector_bbox <- function(v_obj, ...) {
    sf::st_bbox(v_obj, ...)
}
