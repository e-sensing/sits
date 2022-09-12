#' @title Build a labelled image from a probability cube
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              and label them based on the maximum probability for each pixel.
#'
#' @param  cube              Classified image data cube.
#' @param  multicores        Number of workers to label the classification in
#'                           parallel.
#' @param  memsize           maximum overall memory (in GB) to label the
#'                           classification.
#' @param  output_dir        Output directory for classified files.
#' @param  version           Version of resulting image
#'                           (in the case of multiple runs).
#' @return                   A data cube with an image with the classified map.
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
sits_label_classification <- function(cube,
                                      multicores = 2,
                                      memsize = 4,
                                      output_dir = ".",
                                      version = "v1") {

    # set caller to show in errors
    .check_set_caller("sits_label_classification")

    # precondition - check if cube has probability data
    .check_is_probs_cube(cube)
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - memsize
    .check_memsize(memsize)
    # precondition - output dir
    .check_output_dir(output_dir)
    # precondition - version
    .check_version(version)

    # Get job size
    job_size <- .raster_file_blocksize(
        .raster_open_rast(.fi_path(.fi(.tile(cube))))
    )
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = job_size, npaths = length(.fi_paths(.fi(.tile(cube)))) +
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

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Labeling
    # Process each tile sequentially
    class_cube <- .cube_foreach_tile(cube, function(tile) {

        # Classify the data
        class_tile <- .sits_label_tile(
            tile = tile, label_fn = label_max_prob, memsize = memsize,
            multicores = multicores, output_dir = output_dir,
            version = version
        )

        return(class_tile)
    })

    return(class_cube)

}

.sits_label_tile  <- function(tile,
                              label_fn,
                              memsize,
                              multicores,
                              output_dir,
                              version) {

    # Output file
    out_file <- .file_class_name(
        tile = tile, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        # # Callback final tile classification
        # .callback(process = "tile_classification", event = "recovery",
        #           context = environment())
        message("Recovery: tile '", tile[["tile"]], "' already exists.")
        message("(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)")
        probs_tile <- .tile_class_from_file(
            file = out_file,
            band = "probs",
            base_tile = tile
        )
        return(probs_tile)
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
        # Output file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (file.exists(block_file)) {
            # Try to open the file
            r_obj <- .try(.raster_open_rast(block_file), default = NULL)
            # If file can be opened, check if the result is correct
            # this file will not be processed again
            if (!is.null(r_obj)) {
                # Verify if the raster is corrupted
                valid_block <- .try({
                    .raster_get_values(r_obj)
                    # Return value
                    TRUE
                },
                default = {
                    unlink(block_file)
                    # Return value
                    FALSE
                })

                if (valid_block) {
                    return(block_file)
                }
            }
        }
        # Read and preprocess values
        probs <- .sits_derived_data_read(
            tile = tile, band = "probs", block = block
        )

        # Apply the labeling function to values
        class_values <- label_fn(probs)

        # Are the results consistent with the data input?
        .check_that(
            x = nrow(class_values) == nrow(probs),
            msg = paste(
                "number of rows of class matrix is different",
                "from number of input pixels"
            )
        )

        # Prepare probability to be saved
        conf_band <- .conf_derived_band(
            derived_class = "class_cube", band = "class"
        )
        offset <- .band_offset(conf_band)
        if (!is.null(offset) && offset != 0) {
            class_values <- class_values - offset
        }
        scale <- .band_scale(conf_band)
        if (!is.null(scale) && scale != 1) {
            class_values <- class_values / scale
        }

        # Prepare and save results as raster
        .raster_write_block(
            file = block_file,
            block = block,
            bbox = .bbox(job),
            values = class_values,
            data_type = .band_data_type(conf_band)
        )

        # Returned value
        block_file
    })

    # Merge blocks into a new class_cube tile
    class_tile <- .tile_class_merge_blocks(
        file = out_file, band = "class",
        labels = .tile_labels(tile),
        base_tile = tile, block_files = block_files,
        multicores = multicores
    )

    return(class_tile)
}
