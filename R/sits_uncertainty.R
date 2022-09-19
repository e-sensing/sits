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

    # Check if cube has probability data
    .check_is_probs_cube(cube)
    # Check window size
    .check_window_size(window_size)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # check output dir
    .check_output_dir(output_dir)
    # check version
    .check_version(version)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.fi_path(.fi(cube))))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
        # npaths = input(nlayers) + output(1)
        npaths = length(.tile_labels(cube)) + 1,
        nbytes = 8, proc_bloat = .config_processing_bloat()
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Define the class of the smoothing
    class(type) <- c(type, class(type))
    UseMethod("sits_uncertainty", type)
}

#' @rdname sits_uncertainty
#' @export
sits_uncertainty.least <- function(cube, type = "least", window_size = 5,
                                   memsize = 4, multicores = 2,
                                   output_dir = getwd(), version = "v1") {
    # Uncertainty parameters checked in smooth function creation
    # Create uncertainty function
    uncert_fn <- .uncertainty_fn_least(window_size = window_size)
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Uncertainty
    # Process each tile sequentially
    uncert_cube <- .cube_foreach_tile(cube, function(tile) {
        # Compute uncertainty
        uncert_tile <- .uncertainty_tile(
            tile = tile, band = "least", overlap = overlap,
            uncert_fn = uncert_fn, output_dir = output_dir, version = version
        )
        return(uncert_tile)
    })
    return(uncert_cube)
}

#' @rdname sits_uncertainty
#' @export
sits_uncertainty.entropy <- function(cube, type = "entropy", window_size = 5,
                                     memsize = 4, multicores = 2,
                                     output_dir = getwd(), version = "v1") {
    # Uncertainty parameters checked in smooth function creation
    # Create uncertainty function
    uncert_fn <- .uncertainty_fn_entropy(window_size = window_size)
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Uncertainty
    # Process each tile sequentially
    uncert_cube <- .cube_foreach_tile(cube, function(tile) {
        # Compute uncertainty
        uncert_tile <- .uncertainty_tile(
            tile = tile, band = "entropy", overlap = overlap,
            uncert_fn = uncert_fn, output_dir = output_dir, version = version
        )
        return(uncert_tile)
    })
    return(uncert_cube)
}

#' @rdname sits_uncertainty
#' @export
sits_uncertainty.margin <- function(cube, type = "margin", window_size = 5,
                                    memsize = 4, multicores = 2,
                                    output_dir = getwd(), version = "v1") {
    # Uncertainty parameters checked in smooth function creation
    # Create uncertainty function
    uncert_fn <- .uncertainty_fn_margin(window_size = window_size)
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Uncertainty
    # Process each tile sequentially
    uncert_cube <- .cube_foreach_tile(cube, function(tile) {
        # Compute uncertainty
        uncert_tile <- .uncertainty_tile(
            tile = tile, band = "margin", overlap = overlap,
            uncert_fn = uncert_fn, output_dir = output_dir, version = version
        )
        return(uncert_tile)
    })
    return(uncert_cube)
}

#---- internal functions ----

.uncertainty_tile <- function(tile, band, overlap, uncert_fn, output_dir,
                              version) {
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
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = overlap)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file), block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .tile_read_block(
            tile = tile, band = .tile_bands(tile), block = block,
            replace_by_minmax = TRUE
        )
        # Apply the labeling function to values
        values <- uncert_fn(values = values, block = block)
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
        # Job crop block
        crop_block <- .chunks_block_no_overlap(chunk)
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .band_data_type(band_conf),
            missing_value = .band_miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Return block file
        block_file
    })
    # Merge blocks into a new uncertainty_cube tile
    uncert_tile <- .tile_uncertainty_merge_blocks(
        file = out_file, band = band, labels = .tile_labels(tile),
        base_tile = tile, block_files = block_files,
        multicores = .jobs_multicores()
    )
    # Return uncertainty tile
    uncert_tile
}

#---- uncertainty functions ----

.uncertainty_fn_least <- function(window_size) {
    # Check window size
    .check_window_size(window_size)

    # Define uncertainty function
    uncert_fn <- function(values, block) {
        # Used in check (below)
        original_nrows <- nrow(values)
        # Pocess least confidence
        values <- C_least_probs(values) # return a matrix[rows(values),1]
        # Process window
        if (window_size > 1) {
            values <- C_kernel_median(
                x = values, ncols = .ncols(block), nrows = .nrows(block),
                band = 0, window_size = window_size
            )
        }
        # Are the results consistent with the data input?
        .check_that(
            x = nrow(values) == original_nrows,
            msg = paste(
                "number of rows of class matrix is different",
                "from number of input pixels"
            )
        )
        # Return data
        values
    }
    # Return closure
    uncert_fn
}

.uncertainty_fn_entropy <- function(window_size) {
    # Check window size
    .check_window_size(window_size)

    # Define uncertainty function
    uncert_fn <- function(values, block) {
        # Used in check (below)
        original_nrows <- nrow(values)
        # Pocess least confidence
        values <- C_entropy_probs(values) # return a matrix[rows(values),1]
        # Process window
        if (window_size > 1) {
            values <- C_kernel_median(
                x = values, ncols = .ncols(block), nrows = .nrows(block),
                band = 0, window_size = window_size
            )
        }
        # Are the results consistent with the data input?
        .check_that(
            x = nrow(values) == original_nrows,
            msg = paste(
                "number of rows of class matrix is different",
                "from number of input pixels"
            )
        )
        # Return data
        values
    }
    # Return closure
    uncert_fn
}

.uncertainty_fn_margin <- function(window_size) {
    # Check window size
    .check_window_size(window_size)

    # Define uncertainty function
    uncert_fn <- function(values, block) {
        # Pocess least confidence
        values <- C_margin_probs(values) # return a matrix[rows(data),1]
        # Process window
        if (window_size > 1) {
            values <- C_kernel_median(
                x = values, ncols = .ncols(block), nrows = .nrows(block),
                band = 0, window_size = window_size
            )
        }
        # Return data
        values
    }
    # Return closure
    uncert_fn
}
