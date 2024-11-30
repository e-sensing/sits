#' @title Sample a percentage of a time series
#' @name sits_sample
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new tibble contains a percentage
#'              of the total number of samples per group.
#'              If frac > 1 , all sampling will be done with replacement.
#'
#' @param  data       Sits time series tibble (class = "sits")
#' @param  frac       Percentage of samples to extract
#'                    (range: 0.0 to 2.0, default = 0.2)
#' @param  oversample Logical: oversample classes with small number of samples?
#'                    (TRUE/FALSE)
#' @return            A sits tibble with a fixed quantity of samples.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the labels of the resulting tibble
#' summary(cerrado_2classes)
#' # Sample by fraction
#' data_02 <- sits_sample(cerrado_2classes, frac = 0.2)
#' # Print the labels
#' summary(data_02)
#' @export
sits_sample <- function(data,
                        frac = 0.2,
                        oversample = TRUE) {
    # set caller to show in errors
    .check_set_caller("sits_sample")
    # verify if data and frac are valid
    .check_samples_ts(data)
    # check frac parameter
    .check_num_parameter(frac, min = 0.0, max = 2.0)
    # check oversample
    .check_lgl_parameter(oversample)
    # group the data by label
    groups <- by(data, data[["label"]], list)
    # for each group of samples, obtain the required subset
    result <- .map_dfr(groups, function(class_samples) {
        result_class <- dplyr::slice_sample(
            class_samples,
            prop = frac,
            replace = oversample
        )
        return(result_class)
    })
    return(result)
}
#' @title Reduce imbalance in a set of samples
#' @name sits_reduce_imbalance
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Takes a sits tibble with different labels and
#' returns a new tibble. Deals with class imbalance
#' using the synthetic minority oversampling technique (SMOTE)
#' for oversampling. Undersampling is done using the SOM methods available in
#' the sits package.
#'
#' @param  samples              Sample set to rebalance
#' @param  n_samples_over       Number of samples to oversample
#'                              for classes with samples less than this number.
#' @param  n_samples_under      Number of samples to undersample
#'                              for classes with samples more than this number.
#' @param  multicores           Number of cores to process the data (default 2).
#'
#' @return A sits tibble with reduced sample imbalance.
#'
#' @references
#' The reference paper on SMOTE is
#' N. V. Chawla, K. W. Bowyer, L. O.Hall, W. P. Kegelmeyer,
#' “SMOTE: synthetic minority over-sampling technique,”
#' Journal of artificial intelligence research, 321-357, 2002.
#'
#' Undersampling uses the SOM map developed by Lorena Santos and co-workers
#' and used in the sits_som_map() function.
#' The SOM map technique is described in the paper:
#' Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli,
#' Rolf Simoes, “Quality control and class noise reduction of satellite
#' image time series”. ISPRS Journal of Photogrammetry and Remote Sensing,
#' vol. 177, pp 75-88, 2021. https://doi.org/10.1016/j.isprsjprs.2021.04.014.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # print the labels summary for a sample set
#'     summary(samples_modis_ndvi)
#'     # reduce the sample imbalance
#'     new_samples <- sits_reduce_imbalance(samples_modis_ndvi,
#'         n_samples_over = 200,
#'         n_samples_under = 200,
#'         multicores = 1
#'     )
#'     # print the labels summary for the rebalanced set
#'     summary(new_samples)
#' }
#' @export
sits_reduce_imbalance <- function(samples,
                                  n_samples_over = 200,
                                  n_samples_under = 400,
                                  multicores = 2) {
    # set caller to show in errors
    .check_set_caller("sits_reduce_imbalance")
    # pre-conditions
    .check_samples_train(samples)
    .check_int_parameter(n_samples_over)
    .check_int_parameter(n_samples_under)

    # check if number of required samples are correctly entered
    .check_that(n_samples_under >= n_samples_over,
                msg = .conf("messages", "sits_reduce_imbalance_samples")
    )
    # get the bands and the labels
    bands <- .samples_bands(samples)
    labels <- .samples_labels(samples)
    # params of output tibble
    lat <- 0.0
    long <- 0.0
    start_date <- samples[["start_date"]][[1]]
    end_date <- samples[["end_date"]][[1]]
    cube <- samples[["cube"]][[1]]
    timeline <- .samples_timeline(samples)
    # get classes to undersample
    classes_under <- samples |>
        summary() |>
        dplyr::filter(.data[["count"]] >= n_samples_under) |>
        dplyr::pull("label")
    # get classes to oversample
    classes_over <- samples |>
        summary() |>
        dplyr::filter(.data[["count"]] <= n_samples_over) |>
        dplyr::pull("label")
    # create an output tibble
    new_samples <- .tibble()
    # under sampling
    if (length(classes_under) > 0) {
        # for each class, select some of the samples using SOM
        samples_under_new <- purrr::map(classes_under, function(cls) {
            # select the samples for the class
            samples_cls <- dplyr::filter(samples, .data[["label"]] == cls)
            # set the dimension of the SOM grid
            grid_dim <- ceiling(sqrt(n_samples_under / 4))
            # build the SOM map
            som_map <- suppressWarnings(
                sits_som_map(
                    samples_cls,
                    grid_xdim = grid_dim,
                    grid_ydim = grid_dim,
                    distance = "euclidean",
                    rlen = 10,
                    mode = "pbatch"
                )
            )
            # select samples on the SOM grid using the neurons
            samples_under <- som_map[["data"]] |>
                dplyr::group_by(.data[["id_neuron"]]) |>
                dplyr::slice_sample(n = 4, replace = TRUE) |>
                dplyr::ungroup()
            return(samples_under)
        })
        # bind undersample results
        samples_under_new <- dplyr::bind_rows(samples_under_new)
        new_samples <- dplyr::bind_rows(new_samples, samples_under_new)
    }
    # oversampling
    if (length(classes_over) > 0) {
        .parallel_start(workers = multicores)
        on.exit(.parallel_stop())
        # for each class, build synthetic samples using SMOTE
        samples_over_new <- .parallel_map(classes_over, function(cls) {
            # select the samples for the class
            samples_bands <- purrr::map(bands, function(band) {
                # selection of band
                dist_band <- samples |>
                    sits_select(bands = band) |>
                    dplyr::filter(.data[["label"]] == cls) |>
                    .predictors()
                dist_band <- dist_band[-1]
                # oversampling of band for the class
                dist_over <- .smote_oversample(
                    data = dist_band,
                    cls = cls,
                    cls_col = "label",
                    m = n_samples_over
                )
                # put the oversampled data into a samples tibble
                samples_band <- slider::slide_dfr(dist_over, function(row) {
                    time_series <- tibble::tibble(
                        Index = as.Date(timeline),
                        values = unname(as.numeric(row[-1]))
                    )
                    colnames(time_series) <- c("Index", band)
                    tibble::tibble(
                        longitude = long,
                        latitude = lat,
                        start_date = as.Date(start_date),
                        end_date = as.Date(end_date),
                        label = row[["label"]],
                        cube = cube,
                        time_series = list(time_series)
                    )
                })
                class(samples_band) <- c("sits", class(samples_band))
                return(samples_band)
            })
            tb_class_new <- samples_bands[[1]]
            for (i in seq_along(samples_bands)[-1]) {
                tb_class_new <- sits_merge(tb_class_new, samples_bands[[i]])
            }
            return(tb_class_new)
        })
        # bind oversampling results
        samples_over_new <- dplyr::bind_rows(samples_over_new)
        new_samples <- dplyr::bind_rows(new_samples, samples_over_new)
    }
    # keep classes (no undersampling nor oversampling)
    classes_ok <- labels[!(labels %in% classes_under |
                               labels %in% classes_over)]
    if (length(classes_ok) > 0) {
        samples_classes_ok <- dplyr::filter(
            samples,
            .data[["label"]] %in% classes_ok
        )
        new_samples <- dplyr::bind_rows(new_samples, samples_classes_ok)
    }
    # remove SOM additional columns
    colnames_sits <- setdiff(colnames(new_samples), c("id_neuron", "id_sample"))
    # return new sample set
    return(new_samples[, colnames_sits])
}
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
            terra::xyFromCell(
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
#' This function is best used in the following context:
#'  1. Select an initial set of samples.
#'  2. Train a machine learning model.
#'  3. Build a data cube and classify it using the model.
#'  4. Run a Bayesian smoothing in the resulting probability cube.
#'  5. Perform confidence sampling.
#'
#' The Bayesian smoothing procedure will reduce the classification outliers
#' and thus increase the likelihood that the resulting pixels with provide
#' good quality samples for each class.
#'
#' @param probs_cube      A smoothed probability cube.
#'                        See \code{\link[sits]{sits_classify}} and
#'                        \code{\link[sits]{sits_smooth}}.
#' @param n               Number of suggested points per class.
#' @param min_margin      Minimum margin of confidence to select a sample
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
#'     # obtain a new set of samples for active learning
#'     # the samples are located in uncertain places
#'     new_samples <- sits_confidence_sampling(probs_cube)
#' }
#' @export
sits_confidence_sampling <- function(probs_cube,
                                     n = 20L,
                                     min_margin = 0.90,
                                     sampling_window = 10L,
                                     multicores = 1L,
                                     memsize = 1L) {
    .check_set_caller("sits_confidence_sampling")
    # Pre-conditions
    .check_is_probs_cube(probs_cube)
    .check_int_parameter(n, min = 20)
    .check_num_parameter(min_margin, min = 0.01, max = 1.0)
    .check_int_parameter(sampling_window, min = 10)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_int_parameter(memsize, min = 1, max = 16384)
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(probs_cube)))
    # Overlapping pixels
    overlap <- ceiling(sampling_window / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        npaths = sampling_window,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_memsize = job_memsize,
        block = block,
        image_size = .tile_size(.tile(probs_cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # get labels
    labels <- .cube_labels(probs_cube)
    # Slide on cube tiles
    samples_tb <- slider::slide_dfr(probs_cube, function(tile) {
        # Create chunks as jobs
        chunks <- .tile_chunks_create(
            tile = tile,
            overlap = overlap,
            block = block
        )
        # Tile path
        tile_path <- .tile_path(tile)
        # Get a list of values of high uncertainty
        # Process jobs in parallel
        .jobs_map_parallel_dfr(chunks, function(chunk) {
            # Get samples for each label
            purrr::map2_dfr(labels, seq_along(labels), function(lab, i) {
                # Get a list of values of high confidence & apply threshold
                top_values <- .raster_open_rast(tile_path) |>
                    .raster_get_top_values(
                        block = .block(chunk),
                        band = i,
                        n = n,
                        sampling_window = sampling_window
                    ) |>
                    dplyr::mutate(
                        value = .data[["value"]] *
                            .conf("probs_cube_scale_factor")
                    ) |>
                    dplyr::filter(
                        .data[["value"]] >= min_margin
                    ) |>
                    dplyr::select(dplyr::matches(
                        c("longitude", "latitude", "value")
                    )) |>
                    tibble::as_tibble()

                # All the cube's uncertainty images have the same start &
                # end dates.
                top_values[["start_date"]] <- .tile_start_date(tile)
                top_values[["end_date"]] <- .tile_end_date(tile)
                top_values[["label"]] <- lab

                return(top_values)
            })
        })
    })
    # Slice result samples
    result_tb <- samples_tb |>
        dplyr::group_by(.data[["label"]]) |>
        dplyr::slice_max(
            order_by = .data[["value"]], n = n,
            with_ties = FALSE
        ) |>
        dplyr::ungroup() |>
        dplyr::transmute(
            longitude = .data[["longitude"]],
            latitude = .data[["latitude"]],
            start_date = .data[["start_date"]],
            end_date = .data[["end_date"]],
            label = .data[["label"]],
            confidence = .data[["value"]]
        )

    # Warn if it cannot suggest all required samples
    incomplete_labels <- result_tb |>
        dplyr::count(.data[["label"]]) |>
        dplyr::filter(.data[["n"]] < !!n) |>
        dplyr::pull("label")

    if (length(incomplete_labels) > 0) {
        warning(.conf("messages", "sits_confidence_sampling_window"),
                toString(incomplete_labels),
                call. = FALSE
        )
    }

    class(result_tb) <- c("sits_confidence", "sits", class(result_tb))
    return(result_tb)
}

#' @title Allocation of sample size to strata
#' @name sits_sampling_design
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Takes a class cube with different labels and allocates a number of
#' sample sizes per strata to obtain suitable values of error-adjusted area,
#' providing five allocation strategies.
#'
#' @param  cube                 Classified cube
#' @param  expected_ua          Expected values of user's accuracy
#' @param  alloc_options        Fixed sample allocation for rare classes
#' @param  std_err              Standard error we would like to achieve
#' @param  rare_class_prop      Proportional area limit for rare classes
#'
#'
#' @return A matrix with options to decide allocation
#' of sample size to each class. This matrix uses the same format as
#' Table 5 of Olofsson et al.(2014).
#'
#' @references
#' [1] Olofsson, P., Foody, G.M., Stehman, S.V., Woodcock, C.E. (2013).
#' Making better use of accuracy data in land change studies: Estimating
#' accuracy and area and quantifying uncertainty using stratified estimation.
#' Remote Sensing of Environment, 129, pp.122-131.
#'
#' @references
#' [2] Olofsson, P., Foody G.M., Herold M., Stehman, S.V.,
#' Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change.
#' Remote Sensing of Environment, 148, pp. 42-57.
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
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     # estimated UA for classes
#'     expected_ua <- c(Cerrado = 0.75, Forest = 0.9,
#'                      Pasture = 0.8, Soy_Corn = 0.8)
#'     sampling_design <- sits_sampling_design(label_cube, expected_ua)
#' }
#' @export
sits_sampling_design <- function(cube,
                                 expected_ua = 0.75,
                                 alloc_options = c(100, 75, 50),
                                 std_err = 0.01,
                                 rare_class_prop = 0.1) {
    .check_set_caller("sits_sampling_design")
    # check the cube is valid
    .check_that(inherits(cube, "class_cube") ||
                    inherits(cube, "class_vector_cube"))
    # get the labels
    labels <- .cube_labels(cube)
    n_labels <- length(labels)
    if (length(expected_ua) == 1) {
        expected_ua <- rep(expected_ua, n_labels)
        names(expected_ua) <- labels
    }
    # check names of labels
    .check_that(all(names(expected_ua) %in% labels))
    # get cube class areas
    class_areas <- .cube_class_areas(cube)
    # define which classes from the selected ones are available in the cube.
    available_classes <- intersect(names(expected_ua), names(class_areas))
    # inform user about the available classes
    if (!all(names(expected_ua) %in% available_classes)) {
        message(.conf("messages", "sits_sampling_design_available_labels"))
    }
    # use only the available classes
    class_areas <- class_areas[available_classes]
    expected_ua <- expected_ua[available_classes]
    # check that names of class areas are contained in the labels
    .check_that(all(names(class_areas) %in% labels),
                msg = .conf("messages", "sits_sampling_design_labels"))
    # calculate proportion of class areas
    prop <- class_areas / sum(class_areas)
    # standard deviation of the stratum
    std_dev <- signif(sqrt(expected_ua * (1 - expected_ua)), 3)
    # calculate sample size
    sample_size <-  round((sum(prop * std_dev) / std_err) ^ 2)
    # determine "equal" allocation
    n_classes <- length(class_areas)
    equal <- rep(round(sample_size / n_classes), n_classes)
    names(equal) <- names(class_areas)
    # find out the classes which are rare
    rare_classes <- prop[prop <= rare_class_prop]
    #  Given each allocation for rare classes (e.g, 100 samples)
    #  allocate the rest of the sample size proportionally
    #  to the other more frequent classes
    alloc_options_lst <- purrr::map(alloc_options, function(al) {
        # determine the number of samples to be allocated
        # to more frequent classes
        samples_rare_classes <- al * length(rare_classes)
        remaining_samples <- sample_size - samples_rare_classes
        # allocate samples per class
        # rare classes are given a fixed value (e.g., 100, 75, 50)
        # other classes are allocated proportionally to area
        alloc_class_lst <- purrr::map(prop, function(p) {
            if (p <= rare_class_prop) {
                choice <- al
            } else {
                choice_prop <- p / (1.0 - sum(rare_classes))
                choice <- round(choice_prop * remaining_samples)
            }
            return(choice)
        })
        alloc_class <- cbind(alloc_class_lst)
        colnames(alloc_class) <- paste0("alloc_", al)
        return(alloc_class)
    })
    # get the three allocation options
    alloc_options <- do.call(cbind, alloc_options_lst)
    # final option is the proportional allocation
    alloc_prop <- round(prop * sample_size)
    # put it all together
    design <- cbind(prop, expected_ua, std_dev,
                    equal, alloc_options, alloc_prop
    )
    return(design)
}

#' @title Allocation of sample size to strata
#' @name sits_stratified_sampling
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Takes a class cube with different labels and a sampling
#' design with a number of samples per class and allocates a set of
#' locations for each class
#'
#' @param  cube                 Classified cube
#' @param  sampling_design      Result of sits_sampling_design
#' @param  alloc                Allocation method chosen
#' @param  overhead             Additional percentage to account
#'                              for border points
#' @param  multicores           Number of cores that will be used to
#'                              sample the images in parallel.
#' @param  shp_file             Name of shapefile to be saved (optional)
#' @param progress              Show progress bar? Default is TRUE.
#' @return samples              Point sf object with required samples
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
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     # estimated UA for classes
#'     expected_ua <- c(Cerrado = 0.95, Forest = 0.95,
#'                      Pasture = 0.95, Soy_Corn = 0.95)
#'     # design sampling
#'     sampling_design <- sits_sampling_design(label_cube, expected_ua)
#'     # select samples
#'     samples <- sits_stratified_sampling(label_cube,
#'                                         sampling_design, "alloc_prop")
#'
#' }
#' @export
sits_stratified_sampling <- function(cube,
                                     sampling_design,
                                     alloc = "alloc_prop",
                                     overhead = 1.2,
                                     multicores = 2L,
                                     shp_file = NULL,
                                     progress = TRUE) {
    .check_set_caller("sits_stratified_sampling")
    # check the cube is valid
    .check_raster_cube_files(cube)
    # check the cube is valid
    .check_that(inherits(cube, "class_cube") ||
                    inherits(cube, "class_vector_cube"))
    # get the labels
    labels <- .cube_labels(cube)
    n_labels <- length(labels)
    # check number of labels
    .check_that(nrow(sampling_design) <= n_labels)
    # check names of labels
    .check_that(all(rownames(sampling_design) %in% labels))
    # check allocation method
    .check_that(alloc %in% colnames(sampling_design),
                msg = .conf("messages", "sits_stratified_sampling_alloc"))

    # check samples by class
    samples_by_class <- unlist(sampling_design[, alloc])
    .check_int_parameter(samples_by_class, is_named = TRUE,
                         msg = .conf("messages", "sits_stratified_sampling_samples")
    )
    # check multicores
    .check_int_parameter(multicores, min = 1, max = 2048)
    # check progress
    .check_progress(progress)
    # transform labels to tibble
    labels <- tibble::rownames_to_column(
        as.data.frame(labels), var = "label_id"
    ) |>
        dplyr::mutate(label_id = as.numeric(.data[["label_id"]]))
    # transform sampling design data to tibble
    sampling_design <- tibble::rownames_to_column(
        as.data.frame(sampling_design), var = "labels"
    )
    # merge sampling design with samples metadata to ensure reference to the
    # correct class / values from the cube
    samples_class <- dplyr::inner_join(
        x = sampling_design,
        y = labels,
        by = "labels"
    ) |>
        dplyr::select("labels", "label_id", dplyr::all_of(alloc)) |>
        dplyr::rename("label" = "labels")
    # include overhead
    samples_class[alloc] <- ceiling(unlist(samples_class[[alloc]]) * overhead)
    # call function to allocate sample per strata
    samples <- .samples_alloc_strata(
        cube = cube,
        samples_class = samples_class,
        alloc = alloc,
        multicores = multicores,
        progress = progress
    )
    # save results
    if (.has(shp_file)) {
        .check_that(tools::file_ext(shp_file) == "shp",
                    msg = .conf("messages", "sits_stratified_sampling_shp")
        )
        sf::st_write(samples, shp_file, append = FALSE)
        message(.conf("messages", "sits_stratified_sampling_shp_save"), shp_file)
    }
    return(samples)
}
