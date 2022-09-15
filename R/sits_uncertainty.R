#' @title Estimate classification uncertainty based on probs cube
#'
#' @name  sits_uncertainty
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @param  cube         Probability data cube.
#' @param  type         Method to measure uncertainty. See details.
#' @param  window_size  Size of neighborhood to calculate entropy.
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
#' entropy, 'least' is the difference between 100% and most confident
#' prediction, and 'margin' is the difference between the two most confident
#' predictions.
#'
#' @references Monarch, Robert Munro. Human-in-the-Loop Machine Learning:
#' Active learning and annotation for human-centered AI. Simon and Schuster,
#' 2021.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # calculate uncertainty
#'     uncert_cube <- sits_uncertainty(probs_cube)
#'     # plot the resulting uncertainty cube
#'     plot(uncert_cube)
#' }
#' @export
sits_uncertainty <- function(cube, type = "least", window_size = 5,
                             memsize = 4, multicores = 2,
                             output_dir = getwd(), version = "v1") {

    # check if cube has probability data
    .check_is_probs_cube(cube)
    # check type
    .check_chr_within(
        x = type, within = names(.uncertainty_fn_list),
        msg = "invalid 'type' parameter"
    )
    # check multicores
    .check_multicores(multicores)
    # check memsize
    .check_memsize(memsize)
    # check output dir
    .check_output_dir(output_dir)
    # check version
    .check_version(version)
    # check window size
    .check_window_size(window_size)

    UseMethod("sits_uncertainty", cube)
}

#' @rdname sits_uncertainty
#' @export
sits_uncertainty.probs_cube <- function(cube, type = "least", window_size = 5,
                                        memsize = 4, multicores = 2,
                                        output_dir = getwd(), version = "v1") {
    # Get job size
    job_size <- .raster_file_blocksize(
        .raster_open_rast(.fi_path(.fi(.tile(cube))))
    )
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = job_size, npaths = length(.fi_paths(.fi(.tile(cube)))) *
            length(.tile_labels(.tile(cube))), nbytes = 8,
        proc_bloat = .config_processing_bloat(), overlap = 0
    )
    # Check if memsize is above minimum needed to process one block
    .check_that(
        x = job_memsize < memsize,
        local_msg = paste("minimum memsize needed is", job_memsize, "GB"),
        msg = "provided 'memsize' is insufficient for processing"
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Prepare uncertainty function
    uncert_fn <- .uncertainty_fn_list[[type]]

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Uncertainty
    # Process each tile sequentially
    uncert_cube <- .cube_foreach_tile(cube, function(tile) {

        # Classify the data
        class_tile <- .uncertainty_tile(
            tile = tile, band = "uncert", uncert_fn = uncert_fn,
            window_size = window_size, memsize = memsize,
            multicores = multicores, output_dir = output_dir,
            version = version
        )

        return(class_tile)
    })

    return(uncert_cube)
}

#---- internal functions ----

.uncertainty_tile <- function(tile, band, uncert_fn, window_size, memsize,
                              multicores, output_dir, version) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
        message("Recovery: tile '", tile[["tile"]], "' already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)")
        uncert_tile <- .tile_uncertainty_from_file(
            file = out_file, band = band, base_tile = tile
        )
        return(uncert_tile)
    }
    # Create jobs
    # Get job size
    job_size <- .raster_file_blocksize(
        .raster_open_rast(.fi_path(.fi(tile)))
    )
    # Compute how many jobs to process
    jobs <- .jobs_create(
        job_size = job_size, block_overlap = 0,
        ncols = .tile_ncols(tile), nrows = .tile_nrows(tile),
        xmin = .xmin(tile), xmax = .xmax(tile),
        ymin = .ymin(tile), ymax = .ymax(tile),
        crs = .crs(tile)
    )
    # Process jobs in parallel
    block_files <- .jobs_parallel_chr(jobs, function(job) {
        # Get job block
        block <- .block(job)
        # Output overlap file name
        overlap_block_file <- .file_block_name(
            pattern = .file_pattern(file = out_file, suffix = "_overlap"),
            block = block,
            output_dir = output_dir
        )
        # Output file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            unlink(overlap_block_file)
            return(block_file)
        }
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Used in check (below)
        original_nrows <- nrow(values)
        # Apply the labeling function to values
        values <- uncert_fn(
            data = values, ncols = .ncols(block), nrows = .nrows(block),
            window_size = window_size
        )
        # Are the results consistent with the data input?
        .check_that(
            x = nrow(values) == original_nrows,
            msg = paste(
                "number of rows of class matrix is different",
                "from number of input pixels"
            )
        )
        # Prepare uncertainty to be saved
        band_conf <- .conf_derived_band(
            derived_class = "uncertainty_cube", band = band
        )
        offset <- .band_offset(band_conf)
        if (!is.null(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .band_scale(band_conf)
        if (!is.null(scale) && scale != 1) {
            values <- values / scale
        }
        # Prepare and save results as raster
        .raster_write_block(
            file = overlap_block_file, block = block, bbox = .bbox(job),
            values = values, data_type = .band_data_type(band_conf),
            missing_value = .band_miss_value(band_conf)
        )
        # Create extent to crop overlaps
        blk_no_overlap <- .jobs_remove_overlap(job)
        # Save chunk
        # Crop removing overlaps
        .raster_crop(
            r_obj = .raster_open_rast(overlap_block_file), file = block_file,
            data_type = .band_data_type(band_conf), overwrite = TRUE,
            block = blk_no_overlap, missing_value = .band_miss_value(band_conf)
        )
        on.exit(unlink(overlap_block_file), add = TRUE)
        # Return block file
        block_file
    })
    # Merge blocks into a new uncertainty_cube tile
    uncert_tile <- .tile_uncertainty_merge_blocks(
        file = out_file, band = band, labels = .tile_labels(tile),
        base_tile = tile, block_files = block_files, multicores = multicores
    )
    # Return uncertainty tile
    uncert_tile
}

#---- uncertainty functions ----

.uncertainty_fn_least <- function(data, ncols, nrows, window_size) {
    # Pocess least confidence
    data <- C_least_probs(data) # return a matrix[rows(data),1]
    # Process window
    if (window_size > 1) {
        data <- C_kernel_median(
            x = data, ncols = ncols, nrows = nrows, band = 0,
            window_size = window_size
        )
    }
    # Return data
    data
}

.uncertainty_fn_entropy <- function(data, ncols, nrows, window_size) {
    # Pocess least confidence
    data <- C_entropy_probs(data) # return a matrix[rows(data),1]
    # Process window
    if (window_size > 1) {
        data <- C_kernel_median(
            x = data, ncols = ncols, nrows = nrows, band = 0,
            window_size = window_size
        )
    }
    # Return data
    data
}

.uncertainty_fn_margin <- function(data, ncols, nrows, window_size) {
    # Pocess least confidence
    data <- C_margin_probs(data) # return a matrix[rows(data),1]
    # Process window
    if (window_size > 1) {
        data <- C_kernel_median(
            x = data, ncols = ncols, nrows = nrows, band = 0,
            window_size = window_size
        )
    }
    # Return data
    data
}

# List of uncertainty functions
.uncertainty_fn_list <- list(
    least = .uncertainty_fn_least,
    entropy = .uncertainty_fn_entropy,
    margin = .uncertainty_fn_margin
)
