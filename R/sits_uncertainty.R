#' @title Estimate classification uncertainty based on probs cube
#'
#' @name  sits_uncertainty
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @param  cube         Probability data cube.
#' @param  ...         Other parameters for specific functions.
#' @param  type         Method to measure uncertainty. See details.
#' @param  multicores   Number of cores to run the function.
#' @param  memsize      Maximum overall memory (in GB) to run the function.
#' @param  output_dir   Output directory for image files.
#' @param  version      Version of resulting image (in the case of
#'                      multiple tests).
#' @return An uncertainty data cube
#'
#' @description Calculate the uncertainty cube based on the probabilities
#' produced by the classifier. Takes a probability cube as input.
#' The uncertainty measure is relevant in the context of active leaning,
#' and helps to increase the quantity and quality of training samples by
#' providing information about the confidence of the model.
#' The supported types of uncertainty are 'entropy', 'least', and 'margin'.
#' 'entropy' is the difference between all predictions expressed as
#' entropy, 'least' is the difference between 1.0 and most confident
#' prediction, and 'margin' is the difference between the two most confident
#' predictions.
#'
#' @references Monarch, Robert Munro. Human-in-the-Loop Machine Learning:
#' Active learning and annotation for human-centered AI. Simon and Schuster,
#' 2021.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # calculate uncertainty
#'     uncert_cube <- sits_uncertainty(probs_cube, output_dir = tempdir())
#'     # plot the resulting uncertainty cube
#'     plot(uncert_cube)
#' }
#' @export
sits_uncertainty <-  function(cube, ...) {
    # Dispatch
    UseMethod("sits_uncertainty", cube)
}
#' @rdname sits_uncertainty
#' @export
sits_uncertainty.probs_cube <- function(
        cube, ...,
        type = "entropy",
        multicores = 2,
        memsize = 4,
        output_dir,
        version = "v1") {
    # Check if cube has probability data
    .check_raster_cube_files(cube)
    # Check memsize
    .check_num_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_num_parameter(multicores, min = 1, max = 2048)
    # check output dir
    .check_output_dir(output_dir)
    # check version
    version <- .check_version(version)
    # version is case-insensitive in sits
    version <- tolower(version)

    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0),
        npaths = length(.tile_labels(cube)) + 1,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )

    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Define the class of the smoothing
    uncert_fn <- switch(
        type,
        least   = .uncertainty_fn_least(),
        margin  = .uncertainty_fn_margin(),
        entropy = .uncertainty_fn_entropy()
    )
    # Compute uncertainty
    uncert_cube <- .uncertainty_raster_cube(
        cube = cube,
        band = type,
        uncert_fn = uncert_fn,
        output_dir = output_dir,
        version = version
    )
    return(uncert_cube)
}
#' @rdname sits_uncertainty
#' @export
sits_uncertainty.probs_vector_cube <- function(
        cube, ...,
        type = "entropy",
        multicores = 2,
        memsize = 4,
        output_dir,
        version = "v1") {
    # Check if cube has probability data
    .check_raster_cube_files(cube)
    # Check memsize
    .check_int_parameter(memsize, min = 1, max = 16384)
    # Check multicores
    .check_int_parameter(multicores, min = 1, max = 2048)
    # check output dir
    .check_output_dir(output_dir)
    # check version
    version <- .check_version(version)
    # Compute uncertainty
    uncert_cube <- .uncertainty_vector_cube(
        cube = cube,
        band = type,
        output_dir = output_dir,
        version = version
    )
    return(uncert_cube)
}
#' @rdname sits_uncertainty
#' @export
sits_uncertainty.default <- function(cube, ...) {
    stop(.conf("messages", "sits_uncertainty_default"))
}
#' @title Suggest samples for enhancing classification accuracy
#'
#' @name sits_uncertainty_sampling
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Suggest samples for regions of high uncertainty as predicted by the model.
#' The function selects data points that have confused an algorithm.
#' These points don't have labels and need be manually labelled by experts
#' and then used to increase the classification's training set.
#'
#' This function is best used in the following context:
#'  1. Select an initial set of samples.
#'  2. Train a machine learning model.
#'  3. Build a data cube and classify it using the model.
#'  4. Run a Bayesian smoothing in the resulting probability cube.
#'  5. Create an uncertainty cube.
#'  6. Perform uncertainty sampling.
#'
#' The Bayesian smoothing procedure will reduce the classification outliers
#' and thus increase the likelihood that the resulting pixels with high
#' uncertainty have meaningful information.
#'
#' @param uncert_cube     An uncertainty cube.
#'                        See \code{\link[sits]{sits_uncertainty}}.
#' @param n               Number of suggested points to be sampled per tile.
#' @param min_uncert      Minimum uncertainty value to select a sample.
#' @param sampling_window Window size for collecting points (in pixels).
#'                        The minimum window size is 10.
#' @param multicores      Number of workers for parallel processing
#'                        (integer, min = 1, max = 2048).
#' @param memsize         Maximum overall memory (in GB) to run the
#'                        function.
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
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # build a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor())
#'     # classify the cube
#'     probs_cube <- sits_classify(
#'         data = cube, ml_model = rfor_model, output_dir = tempdir()
#'     )
#'     # create an uncertainty cube
#'     uncert_cube <- sits_uncertainty(probs_cube,
#'         type = "entropy",
#'         output_dir = tempdir()
#'     )
#'     # obtain a new set of samples for active learning
#'     # the samples are located in uncertain places
#'     new_samples <- sits_uncertainty_sampling(
#'         uncert_cube,
#'         n = 10, min_uncert = 0.4
#'     )
#' }
#'
#' @export
sits_uncertainty_sampling <- function(uncert_cube,
                                      n = 100L,
                                      min_uncert = 0.4,
                                      sampling_window = 10L,
                                      multicores = 1L,
                                      memsize = 1L) {
    .check_set_caller("sits_uncertainty_sampling")
    # Pre-conditions
    .check_is_uncert_cube(uncert_cube)
    .check_int_parameter(n, min = 1)
    .check_num_parameter(min_uncert, min = 0.0, max = 1.0)
    .check_int_parameter(sampling_window, min = 1L)
    .check_int_parameter(multicores, min = 1)
    .check_int_parameter(memsize, min = 1)
    # Slide on cube tiles
    samples_tb <- slider::slide_dfr(uncert_cube, function(tile) {
        # open spatial raster object
        rast <- .raster_open_rast(.tile_path(tile))
        # get the values
        values <- .raster_get_values(rast)
        # sample the maximum values
        samples_tile <- C_max_sampling(
            x = values,
            nrows = nrow(rast),
            ncols = ncol(rast),
            window_size = sampling_window
        )
        # get the top most values
        samples_tile <- samples_tile |>
            # randomly shuffle the rows of the dataset
            dplyr::sample_frac() |>
            dplyr::slice_max(
                .data[["value"]],
                n = n,
                with_ties = FALSE
            )
        # transform to tibble
        tb <- rast |>
            .raster_xy_from_cell(
                cell = samples_tile[["cell"]]
            ) |>
            tibble::as_tibble()
        # find NA
        na_rows <- which(is.na(tb))
        # remove NA
        if (length(na_rows) > 0) {
            tb <- tb[-na_rows, ]
            samples_tile <- samples_tile[-na_rows, ]
        }
        # Get the values' positions.
        result_tile <- tb |>
            sf::st_as_sf(
                coords = c("x", "y"),
                crs = .raster_crs(rast),
                dim = "XY",
                remove = TRUE
            ) |>
            sf::st_transform(crs = "EPSG:4326") |>
            sf::st_coordinates()

        colnames(result_tile) <- c("longitude", "latitude")
        result_tile <- result_tile |>
            dplyr::bind_cols(samples_tile) |>
            dplyr::mutate(
                value = .data[["value"]] *
                    .conf("probs_cube_scale_factor")
            ) |>
            dplyr::filter(
                .data[["value"]] >= min_uncert
            ) |>
            dplyr::select(dplyr::matches(
                c("longitude", "latitude", "value")
            )) |>
            tibble::as_tibble()

        # All the cube's uncertainty images have the same start & end dates.
        result_tile[["start_date"]] <- .tile_start_date(uncert_cube)
        result_tile[["end_date"]] <- .tile_end_date(uncert_cube)
        result_tile[["label"]] <- "NoClass"
        return(result_tile)
    })
    samples_tb <- dplyr::rename(samples_tb, uncertainty = value)

    return(samples_tb)
}
