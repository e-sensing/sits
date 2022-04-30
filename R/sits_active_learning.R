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
#' outside R and then used to increase the classification's training set.
#'
#' @param cube            A `sits` uncertainty cube. See `sits_uncertainty`.
#' @param n               Number of suggested points.
#' @param min_dist_pixels Mininum distance among suggested points (in pixels).
#'
#' @return     A data.frame with longitude & latitude in WGS84.
#'
sits_suggest_samples <- function(cube, n = 100,
                                 min_dist_pixels = 10) {

    .check_that(inherits(cube, what = "uncertainty_cube"),
                msg = "Cube is not an sits_uncertainty cube")
    .check_that(n > 0,
                msg = "Invalid number of new samples")
    .check_that(min_dist_pixels >= 0,
                msg = "Invalid minimum distance.")

    infos <- cube[["file_info"]]
    paths <- vapply(infos, function(x) {x[["path"]]}, character(1))
    rasters <- lapply(paths, terra::rast)

    top_values <- lapply(rasters, .get_values, n = n,
                         min_dist_pixels = min_dist_pixels,
                         top = TRUE)
    top_values <- lapply(top_values, sf::st_drop_geometry)
    top_values <- do.call(rbind, top_values)
    top_values <- top_values[order(top_values[, "lyr1"],
                                   decreasing = TRUE), ]
    top_values <- top_values[1:min(n, nrow(top_values)), c("X", "Y")]
    colnames(top_values) <- c("longitude", "latitude")
    # NOTE: All the cube's uncertainty images have the same start & end dates.
    top_values["start_date"]  <- infos[[1]]["start_date"][[1]]
    top_values["end_date"]    <- infos[[1]]["end_date"][[1]]
    top_values["label"]       <- "NoClass"
    if (nrow(top_values) < n)
        warning(sprintf(paste0("Unable to suggest %s samples.",
                               "Try an smaller min_dist_pixels"),
                        n))

    invisible(top_values)
}



#' @title Suggest high confidence samples to increase the training set.
#'
#' @name sits_increase_samples
#'
#' @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Suggest points for increasing the training set. These points are labelled
#' with high confidence so they can be added to the training set.
#'
#' @param u_cube          A `sits` uncertainty cube. See `sits_uncertainty`.
#' @param c_cube          A `sits` classified cube. See
#' `sits_label_classification`.
#' @param n               Number of suggested points.
#' @param min_dist_pixels Mininum distance among suggested points (in pixels).
#'
#' @return     A data.frame with longitude & latitude in WGS84.
#'
sits_increase_samples <- function(u_cube,
                                  c_cube,
                                  n = 100,
                                  min_dist_pixels = 10) {

    .check_that(inherits(u_cube, what = "uncertainty_cube"),
                msg = "Cube is not an sits_uncertainty cube")
    .check_that(inherits(c_cube, what = "classified_image"),
                msg = "Cube is not an sits_label_classification cube")
    .check_that(n > 0,
                msg = "Invalid number of new samples")
    .check_that(min_dist_pixels >= 0,
                msg = "Invalid minimum distance.")

    infos <- u_cube[["file_info"]]
    paths <- vapply(infos, function(x) {x[["path"]]}, character(1))
    rasters <- lapply(paths, terra::rast)

    bottom_values <- lapply(rasters, .get_values, n = n,
                            min_dist_pixels = min_dist_pixels,
                            top = FALSE)
    bottom_values <- lapply(bottom_values, sf::st_drop_geometry)
    bottom_values <- do.call(rbind, bottom_values)
    bottom_values <- bottom_values[order(bottom_values[, "lyr1"],
                                         decreasing = FALSE), ]
    bottom_values <- bottom_values[1:min(n, nrow(bottom_values)), c("X", "Y")]
    colnames(bottom_values) <- c("longitude", "latitude")
    # NOTE: All the cube's uncertainty images have the same start & end dates.
    bottom_values["start_date"]  <- infos[[1]]["start_date"][[1]]
    bottom_values["end_date"]    <- infos[[1]]["end_date"][[1]]

    # Get the labels of the samples.
    infos <- c_cube[["file_info"]]
    paths <- vapply(infos, function(x) {x[["path"]]}, character(1))
    rasters <- lapply(paths, terra::rast)

    # Get the label id of points_df from the given raster.
    # NOTE: points_df's coordinates must be given in WGS84.
    .get_labels <- function(raster, points_df){
        points_sf <- sf::st_as_sf(points_df,
                                  coords = c("longitude", "latitude"),
                                  crs = 4326, dim = "XY", remove = FALSE)
        points_sf <- sf::st_transform(points_sf,
                                      crs = terra::crs(raster))
        label_id <- terra::extract(raster, y = terra::vect(points_sf),
                                   xy = FALSE, cells = FALSE)
        points_df <- cbind(points_df, label_id[,2])
        colnames(points_df) <- c(head(colnames(points_df), -1), "label")
        return(points_df)
    }

    label_ls <- lapply(rasters, .get_labels,
                       points_df = bottom_values)

    samples_df <- do.call(rbind, label_ls)
    # NOTE: Assuming all the rasters in the classification cube have the same
    #       labels.
    label_ch <- c_cube[["labels"]][[1]]
    samples_df["label"] <- label_ch[samples_df[["label"]]]

    if (nrow(samples_df) < n)
        warning(sprintf(paste0("Unable to increment %s samples.",
                               "Try an smaller min_dist_pixels"),
                        n))

    invisible(samples_df)
}



# @title Get top or bottom values of a raster.
#
# @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#
# @description
# Get either the top or bottom values of a raster as a point `sf` object. The
# values locations  are guaranteed to be separated by a certain number of
# pixels.
#
# @param raster          A `terra` raster object.
# @param n               Number of values to extract.
# @param min_dist_pixels Mininum distance among values (in pixels).
# @param top             Do we get the top values or the bottom values?
#
# @return                A point `sf` object.
#
.get_values <- function(raster, n, min_dist_pixels, top) {

    x <- terra::values(raster, mat = FALSE)

    # Pre-filter values to speed distance matrix computation.
    if (top) {
        qx <- stats::quantile(x, probs = c(1 - (2 * n / length(x)), 1))
    } else{
        qx <- stats::quantile(x, probs = c(0, 2 * n / length(x)))
    }
    x[!(x >= qx[1] & x <= qx[2])] <- NA

    # Get the values' positions.
    pos <- which(!is.na(x), arr.ind = TRUE)
    xy <- terra::xyFromCell(raster, pos)
    var <- terra::extract(raster, xy, xy = TRUE)

    # Guarantee a minimum distance among points.
    var <- var[order(var[, 1], decreasing = top), ]
    var_sf <- sf::st_as_sf(var, coords = c("x", "y"),
                           crs = terra::crs(raster),
                           dim = "XY", remove = TRUE)
    dist_mt <- sf::st_distance(var_sf, var_sf)
    dist_mt[upper.tri(dist_mt, diag = TRUE)] <- Inf
    dist_vc <- apply(dist_mt, MARGIN = 1, FUN = min)
    min_dist <- min_dist_pixels * sqrt(sum(terra::res(raster)^2))
    var_sf <- var_sf[dist_vc > min_dist, ]

    # Get the top values.
    var_sf <- var_sf[1:min(n, nrow(var_sf)), ]

    # Transform to WGS84.
    var_sf <- sf::st_transform(var_sf, crs = 4326)
    var_sf <- cbind(sf::st_coordinates(var_sf), var_sf)

    return(var_sf)
}

