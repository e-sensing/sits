#' @title Sample a percentage of a time series
#' @name sits_sample
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @description Takes a sits tibble with different labels and
#'              returns a new tibble. For a given field as a group criterion,
#'              this new tibble contains a percentage
#'              of the total number of samples per group.
#'              If frac > 1 , all sampling will be done with replacement.
#'
#' @param  data       Sits time series tibble
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
#' @title Suggest high confidence samples to increase the training set.
#'
#' @name sits_confidence_sampling
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' \enumerate{
#' \item{Select an initial set of samples.}
#' \item{Train a machine learning model.}
#' \item{Build a data cube and classify it using the model.}
#' \item{Run a Bayesian smoothing in the resulting probability cube.}
#' \item{Perform confidence sampling.}
#' }
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
                                     n = 20,
                                     min_margin = 0.90,
                                     sampling_window = 10,
                                     multicores = 1,
                                     memsize = 1) {
    .check_set_caller("sits_confidence_sampling")
    # Pre-conditions
    .check_is_probs_cube(probs_cube)
    .check_int_parameter(n, min = 20)
    .check_num_parameter(min_margin, min = 0.01, max = 1.0)
    .check_int_parameter(sampling_window, min = 10)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_int_parameter(memsize, min = 1, max = 16384)

    # get labels
    cube_labels <- .cube_labels(probs_cube)

    # The following functions define optimal parameters for parallel processing
    #
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(probs_cube)))
    # Overlapping pixels
    overlap <- ceiling(sampling_window / 2) - 1
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = overlap),
        npaths = sampling_window,
        nbytes = 8,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(probs_cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)


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
            purrr::map2_dfr(cube_labels, seq_along(cube_labels),
                            function(lab, i) {
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
                top_values
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
    cube_labels <- .cube_labels(cube)
    n_labels <- length(cube_labels)
    if (length(expected_ua) == 1) {
        expected_ua <- rep(expected_ua, n_labels)
        names(expected_ua) <- cube_labels
    }
    # check names of labels
    .check_that(all(names(expected_ua) %in% cube_labels))
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
    .check_that(all(names(class_areas) %in% cube_labels),
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
    cube_labels <- .cube_labels(cube)
    n_labels <- length(cube_labels)
    # check number of labels
    .check_that(nrow(sampling_design) <= n_labels)
    # check names of labels
    .check_that(all(rownames(sampling_design) %in% cube_labels))
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
    cube_labels <- tibble::rownames_to_column(
        as.data.frame(cube_labels), var = "label_id"
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
        y = cube_labels,
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
        message(.conf("messages",
                      "sits_stratified_sampling_shp_save"), shp_file)
    }
    return(samples)
}
