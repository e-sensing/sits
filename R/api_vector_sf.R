#' @keywords internal
#' @noRd
#' @export
.vector_check_package.sf <- function() {
    # package namespace
    pkg_name <- "sf"

    # check if terra package is available
    .check_require_packages(pkg_name)

    class(pkg_name) <- pkg_name

    return(invisible(pkg_name))
}

.vector_open_vec.sf <- function(file_path, ...) {
    sf::st_read(dsn = file_path, ..., quiet = TRUE)
}

.vector_read_vec.sf <- function(file_path, ...) {
    sf::read_sf(dsn = file_path, ..., quiet = TRUE)
}

.vector_write_vec <- function(v_obj, file_path, ...) {
    sf::st_write(obj = v_obj, dsn = file_path, quiet = TRUE, ...)
}

.vector_bbox.sf <- function(v_obj, ...) {
    sf::st_bbox(v_obj, ...)
}

.vector_crs.sf <- function(v_obj, wkt = TRUE, ...) {
    crs <- sf::st_crs(v_obj, ...)
    if (wkt) {
        return(crs[["wkt"]])
    }
    crs
}

.vector_reproject.sf <- function(v_obj, crs, ...) {
    sf::st_transform(x = v_obj, crs = crs, ...)
}
