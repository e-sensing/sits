#' @title Suggest samples for enhancing classification accuracy
#'
#' @name sits_uncertainty_sampling
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
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
#' @param uncert_cube     An uncertainty cube. See \code{sits_uncertainty}.
#' @param n               Number of suggested points.
#' @param min_uncert      Minimum uncertainty value to select a sample.
#' @param sampling_window Window size for collecting points (in pixels).
#'                        The minimum window size is 10.
#'
#' @return
#' A tibble with longitude and latitude in WGS84 with locations
#' which have high uncertainty and meet the minimum distance
#' criteria.
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
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # build a random forest model
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'     # classify the cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # create an uncertainty cube
#'     uncert_cube <- sits_uncertainty(probs_cube)
#'     # obtain a new set of samples for active learning
#'     # the samples are located in uncertain places
#'     new_samples <- sits_uncertainty_sampling(uncert_cube)
#' }
#'
#' @export
#'
sits_uncertainty_sampling <- function(uncert_cube,
                                      n = 100,
                                      min_uncert = 0.4,
                                      sampling_window = 10) {

    .check_set_caller("sits_uncertainty_sampling")

    # Pre-conditions
    .check_that(
        x = inherits(uncert_cube, what = "uncertainty_cube"),
        local_msg = "please run sits_uncertainty() first",
        msg = "input cube is not an uncertainty cube"
    )
    .check_num(
        x = n,
        min = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid n parameter"
    )
    .check_num(
        x = sampling_window,
        min = 10,
        len_min = 1,
        len_max = 1,
        msg = "invalid sampling_window parameter"
    )

    # Slide on cube tiles
    samples_tb <- slider::slide_dfr(uncert_cube, function(tile) {
        path <- .file_info_path(tile)
        # Get a list of values of high uncertainty
        top_values <- .raster_open_rast(path) %>%
            .sits_get_top_values(
                band = 1,
                n = n,
                sampling_window = sampling_window
            ) %>%
            dplyr::mutate(
                value = .data[["value"]] *
                    .config_get("probs_cube_scale_factor")
            ) %>%
            dplyr::filter(
                .data[["value"]] >= min_uncert
            ) %>%
            dplyr::select(dplyr::matches(
                c("longitude", "latitude", "value")
            )) %>%
            tibble::as_tibble()
        # All the cube's uncertainty images have the same start & end dates.
        top_values[["start_date"]] <- .file_info_start_date(tile)
        top_values[["end_date"]] <- .file_info_end_date(tile)
        top_values[["label"]] <- "NoClass"

        return(top_values)
    })

    # Slice result samples
    result_tb <- samples_tb %>%
        dplyr::slice_max(
            order_by = .data[["value"]], n = n,
            with_ties = FALSE
        ) %>%
        dplyr::transmute(
            longitude = .data[["longitude"]],
            latitude = .data[["latitude"]],
            start_date = .data[["start_date"]],
            end_date = .data[["end_date"]],
            label = .data[["label"]],
            uncertainty = .data[["value"]]
        )

    # Warn if it cannot suggest all required samples
    if (nrow(result_tb) < n)
        warning(paste("Unable to suggest", n, "samples.",
                      "Try a smaller sampling_window parameter."))

    class(result_tb) <- c("sits_uncertainty", "sits", class(result_tb))
    return(result_tb)
}

#' @title Suggest high confidence samples to increase the training set.
#'
#' @name sits_confidence_sampling
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
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
#' @param probs_cube      A probability cube. See \code{sits_classify}.
#' @param n               Number of suggested points per class.
#' @param min_margin      Minimum margin of confidence to select a sample
#' @param sampling_window Window size for collecting points (in pixels).
#'                        The minimum window size is 10.
#'
#' @return
#' A tibble with longitude and latitude in WGS84 with locations
#' which have high uncertainty and meet the minimum distance
#' criteria.
#'
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
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # build a random forest model
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'     # classify the cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # obtain a new set of samples for active learning
#'     # the samples are located in uncertain places
#'     new_samples <- sits_confidence_sampling(probs_cube)
#' }
#' @export
sits_confidence_sampling <- function(probs_cube,
                                     n = 20,
                                     min_margin = .90,
                                     sampling_window = 10) {

    .check_set_caller("sits_confidence_sampling")

    # Pre-conditions
    .check_that(
        x = inherits(probs_cube, what = "probs_cube"),
        local_msg = "please run sits_classify() first",
        msg = "input cube is not a probability cube"
    )
    .check_num(
        x = n,
        min = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid n parameter"
    )
    .check_num(
        x = min_margin,
        exclusive_min = 0,
        max = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid min_margin parameter"
    )
    .check_num(
        x = sampling_window,
        min = 10,
        len_min = 1,
        len_max = 1,
        msg = "invalid sampling_window parameter"
    )
    # get labels
    labels <- sits_labels(probs_cube)

    # Slide on cube tiles
    samples_tb <- slider::slide_dfr(probs_cube, function(tile) {
        # Open raster
        r_obj <- .raster_open_rast(.file_info_path(tile))

        # Get samples for each label
        purrr::map2_dfr(labels, seq_along(labels), function(lab, i) {

            # Get a list of values of high confidence & apply threshold
            top_values <- r_obj %>%
                .sits_get_top_values(
                    band = i,
                    n = n,
                    sampling_window = sampling_window
                ) %>%
                dplyr::mutate(
                    value = .data[["value"]] *
                        .config_get("probs_cube_scale_factor")
                ) %>%
                dplyr::filter(
                    .data[["value"]] >= min_margin
                ) %>%
                dplyr::select(dplyr::matches(
                    c("longitude", "latitude", "value")
                )) %>%
                tibble::as_tibble()

            # All the cube's uncertainty images have the same start &
            # end dates.
            top_values[["start_date"]] <- .file_info_start_date(tile)
            top_values[["end_date"]] <- .file_info_end_date(tile)
            top_values[["label"]] <- lab

            return(top_values)
        })
    })

    # Slice result samples
    result_tb <- samples_tb %>%
        dplyr::group_by(.data[["label"]]) %>%
        dplyr::slice_max(
            order_by = .data[["value"]], n = n,
            with_ties = FALSE
        ) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
            longitude = .data[["longitude"]],
            latitude = .data[["latitude"]],
            start_date = .data[["start_date"]],
            end_date = .data[["end_date"]],
            label = .data[["label"]],
            confidence = .data[["value"]]
        )

    # Warn if it cannot suggest all required samples
    incomplete_labels <- result_tb %>%
        dplyr::count(.data[["label"]]) %>%
        dplyr::filter(.data[["n"]] < !!n) %>%
        dplyr::pull(.data[["label"]])

    if (length(incomplete_labels) > 0)
        warning(sprintf(
            paste("Unable to suggest %s samples for label(s) %s.",
                  "Try a smaller sampling_window or a",
                  "smaller min_margin parameter."),
            n, paste0("'", incomplete_labels, "'", collapse = ", ")
        ), call. = FALSE)

    class(result_tb) <- c("sits_confidence", "sits", class(result_tb))
    return(result_tb)
}

#' @title Get top values of a raster.
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description
#' Get the top values of a raster as a point `sf` object. The values
#' locations are guaranteed to be separated by a certain number of pixels.
#'
#' @param r_obj           A raster object.
#' @param band            A numeric band index used to read bricks.
#' @param n               Number of values to extract.
#' @param sampling_window Window size to collect a point (in pixels).
#'
#' @return                A point `tibble` object.
#'
.sits_get_top_values <- function(r_obj,
                                 band,
                                 n,
                                 sampling_window) {

    # Pre-conditions
    .check_num(
        x = band,
        min = 1,
        max = .raster_nlayers(r_obj),
        len_min = 1,
        len_max = 1,
        msg = "invalid band parameter"
    )
    .check_num(
        x = sampling_window,
        min = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid sampling_window parameter"
    )

    # Get top values
    samples_tb <- terra::values(r_obj, mat = TRUE) %>%
        max_sampling(
            band = band - 1,
            img_nrow = terra::nrow(r_obj),
            img_ncol = terra::ncol(r_obj),
            window_size = sampling_window
        ) %>%
        dplyr::slice_max(
            .data[["value"]],
            n = n,
            with_ties = FALSE
        )

    # Get the values' positions.
    result_tb <- r_obj %>%
        terra::xyFromCell(
            cell = samples_tb[["cell"]]
        ) %>%
        tibble::as_tibble() %>%
        sf::st_as_sf(
            coords = c("x", "y"),
            crs = terra::crs(r_obj),
            dim = "XY",
            remove = TRUE
        ) %>%
        sf::st_transform(crs = 4326) %>%
        sf::st_coordinates() %>%
        magrittr::set_colnames(
            value = c("longitude", "latitude")
        ) %>%
        dplyr::bind_cols(samples_tb)

    return(result_tb)
}
