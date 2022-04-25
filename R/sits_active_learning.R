#' @title Suggest samples for enhancing classification accuracy
#'
#' @name sits_suggest_samples
#'
#' @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Suggest points with higher chances of increasing classification accuracy.
#' These points don't have labels and are meant to be labelled by experts
#' outside R.
#'
#' @param cube A `sits`uncertaintly cube. See `sits_uncertainty`.
#' @param n    Maximum number of suggested points.
#'
#' @return     A data.frame
#'
sits_suggest_samples <- function(cube, n = 100) {

    .check_that(inherits(cube, what = "uncertainty_cube"),
                msg = "Cube is not an sits_uncertaintly cube")
    .check_that(n > 0,
                msg = "Invalid number of new samples")

    infos <- cube[["file_info"]]
    paths <- vapply(infos, function(x) {x[["path"]]}, character(1))
    rasters <- lapply(paths, terra::rast)

    # Extract the top values from a raster.
    .get_top_values <- function(raster, n) {
        x <- terra::values(raster, mat = FALSE)
        qx <- stats::quantile(x, probs = c(1 - (2 * n / length(x)), 1))
        x[x >= qx[1] & x <= qx[2]] <- NA
        pos <- which(!is.na(x), arr.ind = TRUE)
        xy <- terra::xyFromCell(raster, pos)
        var <- terra::extract(raster, xy, xy = TRUE)
        var <- var[order(var[, 1], decreasing = TRUE), ]
        var <- var[1:n, ]
        var_sf <- sf::st_as_sf(var, coords = c("x", "y"),
                               crs = terra::crs(raster),
                               dim = "XY", remove = TRUE)
        var_sf <- sf::st_transform(var_sf, crs = 4326)
        var_sf <- cbind(sf::st_coordinates(var_sf), var_sf)
        return(var_sf)
    }

    top_values <- lapply(rasters, .get_top_values, n = n)
    top_values <- lapply(top_values, sf::st_drop_geometry)
    top_values <- do.call(rbind, top_values)
    top_values <- top_values[order(top_values[, "lyr1"], decreasing = TRUE), ]
    top_values <- top_values[1:n, c("X", "Y")]
    colnames(top_values) <- c("longitude", "latitude")
    # NOTE: All the cube's uncertainty images have the same start & end dates.
    top_values["start_date"]  <- infos[[1]]["start_date"][[1]]
    top_values["end_date"]    <- infos[[1]]["end_date"][[1]]
    top_values["label"]       <- "NoClass"

    invisible(top_values)
}

