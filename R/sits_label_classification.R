#' @title Build a labelled image from a probability cube
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              and label them based on the maximum probability for each pixel.
#'
#' @param  cube        Classified image data cube.
#' @param  multicores  Number of workers to label the classification in
#'                     parallel.
#' @param  memsize     maximum overall memory (in GB) to label the
#'                     classification.
#' @param  output_dir  Output directory for classified files.
#' @param  version     Version of resulting image
#'                     (in the case of multiple runs).
#' @return             A data cube with an image with the classified map.
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
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
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(bayes_cube)
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
sits_label_classification <- function(cube, memsize = 4, multicores = 2,
                                      output_dir = getwd(), version = "v1") {

    # Pre-conditions - Check parameters
    .check_is_probs_cube(cube)
    .check_memsize(memsize)
    .check_multicores(multicores)
    .check_output_dir(output_dir)
    .check_version(version)

    # Check memory and multicores
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = 0),
        # npaths = input(nlayers) + output(1)
        npaths = length(.tile_labels(cube)) + 1,
        nbytes = 8, proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    UseMethod("sits_label_classification", cube)
}

#' @rdname sits_label_classification
#' @export
sits_label_classification.probs_cube <- function(cube, memsize = 4,
                                                 multicores = 2,
                                                 output_dir = getwd(),
                                                 version = "v1") {
    # Labeling parameters checked in label function
    # Create label classification function
    label_fn <- .label_fn_majority()
    # Process each tile sequentially
    class_cube <- .cube_foreach_tile(cube, function(tile) {
        # Label the data
        class_tile <- .label_tile(
            tile = tile, band = "class", label_fn = label_fn,
            output_dir = output_dir, version = version
        )
        return(class_tile)
    })
    return(class_cube)
}

#---- internal functions ----

.label_tile  <- function(tile, band, label_fn, output_dir, version) {
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
        class_tile <- .tile_class_from_file(
            file = out_file, band = band, base_tile = tile
        )
        return(class_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- .block(chunk)
        # Output file name
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
            tile = tile, band = .tile_bands(tile), block = block
        )
        # Apply the labeling function to values
        values <- label_fn(values)
        # Prepare probability to be saved
        band_conf <- .conf_derived_band(
            derived_class = "class_cube", band = band
        )
        offset <- .offset(band_conf)
        if (!is.null(offset) && offset != 0) {
            values <- values - offset
        }
        scale <- .scale(band_conf)
        if (!is.null(scale) && scale != 1) {
            values <- values / scale
        }
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned value
        block_file
    })
    # Merge blocks into a new class_cube tile
    class_tile <- .tile_class_merge_blocks(
        file = out_file, band = band, labels = .tile_labels(tile),
        base_tile = tile, block_files = block_files,
        multicores = .jobs_multicores()
    )
    # Return class tile
    class_tile
}

#---- label functions ----

.label_fn_majority <- function() {

    label_fn <- function(values) {
        # Used to check values (below)
        input_pixels <- nrow(values)
        values <- C_label_max_prob(values)
        # Are the results consistent with the data input?
        .check_processed_values(values, input_pixels)
        # Return values
        values
    }
    # Return closure
    label_fn
}
