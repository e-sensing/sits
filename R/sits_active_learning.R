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
#' @param cube            A `sits`uncertainty cube. See `sits_uncertainty`.
#' @param n               Maximum number of suggested points.
#' @param min_dist_pixels Mininum distance among suggested points (in pixels).
#' @param confidence      Level of confidence in the samples. Either 'low' or
#' 'high'. See details.
#'
#' @details
#' 'confidence' determines the level of confidence in the suggested samples. A
#' low level of confidence suggest samples on which the probabilities of each
#' label are similar, hence, it is hard to choose the most likely sample label.
#' These suggested samples are meant to be classified by human experts before
#' adding them to the training set. On the other hand, suggested samples with a
#' high level of confidence are those where the difference in probabilities
#' clearly allow selecting a label. These samples can be added straigh to the
#' training set.
#'
#' @return     A data.frame
#'
sits_suggest_samples <- function(cube, n = 100,
                                 min_dist_pixels = 10,
                                 confidence = "low") {

    .check_that(inherits(cube, what = "uncertainty_cube"),
                msg = "Cube is not an sits_uncertainty cube")
    .check_that(n > 0,
                msg = "Invalid number of new samples")
    .check_that(min_dist_pixels > 0,
                msg = "Invalid minimum distance.")
    .check_that(confidence %in% c("low", "high"),
                msg = "Invalid confidence level")

    infos <- cube[["file_info"]]
    paths <- vapply(infos, function(x) {x[["path"]]}, character(1))
    rasters <- lapply(paths, terra::rast)
    conf_order <- TRUE
    if (confidence == "high")
        conf_order <- FALSE

    # Extract the top values from a raster.
    .get_top_values <- function(raster, n) {
        x <- terra::values(raster, mat = FALSE)
        qx <- stats::quantile(x, probs = c(1 - (2 * n / length(x)), 1))
        x[x >= qx[1] & x <= qx[2]] <- NA
        pos <- which(!is.na(x), arr.ind = TRUE)
        xy <- terra::xyFromCell(raster, pos)
        var <- terra::extract(raster, xy, xy = TRUE)
        # Pre-filter values to speed distance matrix computation.
        var <- var[order(var[, 1], decreasing = conf_order), ]
        var <- var[1:(2 * n), ]
        var_sf <- sf::st_as_sf(var, coords = c("x", "y"),
                               crs = terra::crs(raster),
                               dim = "XY", remove = TRUE)
        # Guarantee a minimum distance among points.
        dist_mt <- sf::st_distance(var_sf, var_sf)
        dist_mt[upper.tri(dist_mt, diag = TRUE)] <- Inf
        dist_vc <- apply(dist_mt, MARGIN = 1, FUN = min)
        min_dist <- min_dist_pixels * sqrt(sum(terra::res(raster)^2))
        dist_vc[dist_vc > min_dist] <- NA
        var_sf <- var_sf[is.na(dist_vc), ]
        # Filter the values.
        var_sf <- var_sf[1:n, ]
        # Transform to WGS84.
        var_sf <- sf::st_transform(var_sf, crs = 4326)
        var_sf <- cbind(sf::st_coordinates(var_sf), var_sf)
        return(var_sf)
    }

    top_values <- lapply(rasters, .get_top_values, n = n)
    top_values <- lapply(top_values, sf::st_drop_geometry)
    top_values <- do.call(rbind, top_values)
    top_values <- top_values[order(top_values[, "lyr1"],
                                   decreasing = conf_order), ]
    top_values <- top_values[1:n, c("X", "Y")]
    colnames(top_values) <- c("longitude", "latitude")
    # NOTE: All the cube's uncertainty images have the same start & end dates.
    top_values["start_date"]  <- infos[[1]]["start_date"][[1]]
    top_values["end_date"]    <- infos[[1]]["end_date"][[1]]
    top_values["label"]       <- "NoClass"

    invisible(top_values)
}

