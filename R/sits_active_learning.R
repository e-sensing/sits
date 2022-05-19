#' @title Suggest samples for enhancing classification accuracy
#'
#' @name sits_uncertainty_sampling
#'
#' @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Suggest samples for regions of high uncertainty as predicted by the model.
#' The function selects data points that have confused an algorithm.
#' These points don't have labels and need be manually labelled by experts
#' and then used to increase the classification's training set.
#'
#' This function is best used in the following context
#' \itemize{
#'    \item{1. }{Select an initial set of samples.}
#'    \item{2. }{Train a machine learning model.}
#'    \item{3. }{Build a data cube and classify it using the model.}
#'    \item{4. }{Run a Bayesian smoothing in the resulting probability cube.}
#'    \item{5. }{Create an uncertainty cube.}
#'    \item{6. }{Perform uncertainty sampling.}
#' }
#' The Bayesian smoothing procedure will reduce the classification outliers
#' and thus increase the likelihood that the resulting pixels with high
#' uncertainty have meaningful information.
#'
#' @param cube            A `sits` uncertainty cube. See `sits_uncertainty`.
#' @param n               Number of suggested points.
#' @param min_dist_pixels Minimum distance among suggested points (in pixels).
#'
#' @return     A data.frame with longitude & latitude in WGS84 with locations
#'             which have high uncertainty and meet the minimum distance
#'             criteria.
#'
#'
#' @references
#' Robert Monarch, "Human-in-the-Loop Machine Learning: Active learning
#' and annotation for human-centered AI". Manning Publications, 2021.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # build a random forest model
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'     # classify the cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # run a Bayesian smoothing
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # create an uncertainty cube
#'     uncert_cube <- sits_uncertainty(bayes_cube)
#'     # obtain a new set of samples for active learning
#'     # the samples are located in uncertain places
#'     new_samples <- sits_uncertainty_sampling(uncert_cube)
#' }
#'
#' @export
#'
sits_uncertainty_sampling <- function(cube,
                                      n = 100,
                                      min_dist_pixels = 10) {
    .check_chr_contains(
        x = class(cube),
        contains = "uncertainty_cube"
    )
    .check_num(
        x = n,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1
    )
    .check_num(
        x = min_dist_pixels,
        min = 0,
        len_min = 1,
        len_max = 1
    )
    paths <- slider::slide(cube, function(row) {
        fi <- .file_info(row)
        return(fi[["path"]])
    })
    # get the raster objects associated to
    rasters <- purrr::map(paths, function(p) {
        .raster_open_rast(p)
    })
    # get a list of values of high uncertainty
    top_values <- purrr::map_dfr(rasters, function(r) {
        tv <- .get_values(r,
            n = n,
            min_dist_pixels = min_dist_pixels,
            top = TRUE
        )
    })
    top_values <- top_values[
        1:min(n, nrow(top_values)),
        c("longitude", "latitude")
    ]
    # All the cube's uncertainty images have the same start & end dates.
    fi <- .file_info(cube[1, ])
    top_values["start_date"] <- as.Date(fi$start_date)
    top_values["end_date"] <- as.Date(fi$end_date)
    top_values["label"] <- "NoClass"
    if (nrow(top_values) < n) {
        warning(sprintf(
            paste0(
                "Unable to suggest %s samples.",
                "Try an smaller min_dist_pixels"
            ),
            n
        ))
    }

    return(top_values)
}

#' @title Suggest high confidence samples to increase the training set.
#'
#' @name sits_confidence_sampling
#'
#' @author Alber Sanchez, \email{alber.sanchez@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Suggest points for increasing the training set. These points are labelled
#' with high confidence so they can be added to the training set.
#' They need to have a satisfactory margin of confidence to be selected.
#' The input is a probability cube. For each label, the algorithm finds out
#' location where the machine learning model has high confidence in choosing
#' this label compared to all others. The algorithm also considers a
#' minimum distance between new labels, to minimize spatial autocorrelation
#' effects.
#'
#' This function is best used in the following context
#' \itemize{
#'    \item{1. }{Select an initial set of samples.}
#'    \item{2. }{Train a machine learning model.}
#'    \item{3. }{Build a data cube and classify it using the model.}
#'    \item{4. }{Run a Bayesian smoothing in the resulting probability cube.}
#'    \item{5. }{Create an uncertainty cube.}
#'    \item{6. }{Perform confidence sampling.}
#' }
#' The Bayesian smoothing procedure will reduce the classification outliers
#' and thus increase the likelihood that the resulting pixels with provide
#' good quality samples for each class.
#'
#' @param probs_cube      A `sits` probability cube. See `sits_classify`.
#' @param n               Number of suggested points per class.
#' @param min_margin      Minimum margin of confidence to select a sample
#' @param min_dist_pixels Minimum distance among suggested points (in pixels).
#'
#' @return     A data.frame with longitude & latitude in WGS84 of locations
#'             for each class that meet the criteria of minimum margin of
#'             confidence and minimum geographical distance between them.
#'
#' if (sits_run_examples()) {
#'     # create a data cube
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # build a random forest model
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI")
#'     rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'     # classify the cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # run a Bayesian smoothing
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # obtain a new set of samples for active learning
#'     # the samples are located in uncertain places
#'     new_samples <- sits_confidence_sampling(bayes_cube)
#' }
#' @export
sits_confidence_samples <- function(probs_cube,
                                    n = 20,
                                    min_margin = .90,
                                    min_dist_pixels = 10) {
    .check_that(
        x = inherits(probs_cube, what = "probs_cube"),
        local_msg = "please run sits_classify() first",
        msg = "input cube is not a probability cube"
    )
    .check_num(
        x = n,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1
    )
    .check_num(
        x = min_dist_pixels,
        min = 0,
        len_min = 1,
        len_max = 1
    )

    paths_prob <- slider::slide(probs_cube, function(row) {
        fi <- .file_info(row)
        return(fi[["path"]])
    })
    # get the raster objects associated to
    rasters <- purrr::map(paths_prob, function(p) {
        .raster_open_rast(p)
    })
    labels <- sits_labels(probs_cube)

    # get a list of top values
    new_samples <- purrr::map2_dfr(labels, seq_along(labels), function(lab, i) {
        label_samples <- purrr::map_dfr(rasters, function(r) {
            # for each raster, get the probs cube associated to a label
            # get the best values which are apart from the neighbors
            tvs_label <- .get_values(r[[i]],
                n = n,
                min_dist_pixels = min_dist_pixels,
                top = TRUE
            )
            # calculate prob margin for each tentative label point
            tvs_valid <- slider::slide_dfr(tvs_label, function(tv) {
                xy <- as.matrix(tv[c("X", "Y")])
                # extract the probabilities for each tentative label point
                probs <- unlist(.raster_extract(r, xy))
                # is the label the most probable one?
                if (max(probs) != probs[[i]]) {
                    return(NULL)
                }
                # sort the probabilities
                probs <- sort(probs, decreasing = TRUE)
                # check probability margin between tentative label
                # and labels with second best confidence
                if ((probs[1] - probs[2]) <
                    (min_margin / .config_get("probs_cube_scale_factor"))) {
                    return(NULL)
                }
                return(tv)
            })
            return(tvs_valid)
        })
        label_samples$label <- lab
        label_samples <- label_samples[
            1:min(n, nrow(label_samples)),
            c("longitude", "latitude", "label")
        ]
        return(label_samples)
    })
    fi <- .file_info(probs_cube[1, ])
    new_samples["start_date"] <- as.Date(fi$start_date)
    new_samples["end_date"] <- as.Date(fi$end_date)
    row.names(new_samples) <- NULL

    labels_low_samples <- new_samples %>%
        dplyr::count(.data[["label"]]) %>%
        dplyr::filter(.data[["n"]] < !!n)
    slider::slide(labels_low_samples, function(lls) {
        warning(paste0(
            "found only ", lls[["n"]],
            " samples for label ", lls[["label"]]
        ), call. = FALSE)
    })

    return(new_samples)
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
    } else {
        qx <- stats::quantile(x, probs = c(0, 2 * n / length(x)))
    }
    x[!(x >= qx[1] & x <= qx[2])] <- NA

    # Get the values' positions.
    pos <- which(!is.na(x), arr.ind = TRUE)
    xy <- terra::xyFromCell(raster, pos)
    var <- terra::extract(raster, xy, xy = TRUE)

    # Guarantee a minimum distance among points.
    var <- var[order(var[, 1], decreasing = top), ]
    var_sf <- sf::st_as_sf(var,
        coords = c("x", "y"),
        crs = terra::crs(raster),
        dim = "XY", remove = TRUE
    )
    dist_mt <- sf::st_distance(var_sf, var_sf)
    dist_mt[upper.tri(dist_mt, diag = TRUE)] <- Inf
    dist_vc <- apply(dist_mt, MARGIN = 1, FUN = min)
    min_dist <- min_dist_pixels * sqrt(sum(terra::res(raster)^2))
    var_sf <- var_sf[dist_vc > min_dist, ]

    # Get the top values.
    var_sf <- var_sf[1:min(n, nrow(var_sf)), ]

    # Transform to WGS84.
    var_sf <- cbind(sf::st_coordinates(var_sf), var_sf)
    var_sf <- sf::st_transform(var_sf, crs = 4326)
    tmp_sf <- sf::st_coordinates(var_sf)
    colnames(tmp_sf) <- c("longitude", "latitude")
    var_sf <- sf::st_drop_geometry(var_sf)
    var_sf <- cbind(var_sf, tmp_sf)
    return(var_sf)
}
