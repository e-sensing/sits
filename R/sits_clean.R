#' @title Cleans a classified map using a local window
#'
#' @name sits_clean
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Applies a modal function to clean up possible noisy pixels keeping
#' the most frequently values within the neighborhood.
#' In a tie, the first value of the vector is considered. Modal functions
#' applied to classified cubes are useful to remove salt-and-pepper noise
#' in the result.
#'
#' @param cube        Classified data cube (tibble of class "class_cube").
#' @param ...         Specific parameters for specialised functions
#' @param window_size An odd integer  representing the size of the
#'                    sliding window of the modal function (min = 1, max = 15).
#' @param memsize     Memory available for classification in GB
#'                    (integer, min = 1, max = 16384).
#' @param multicores  Number of cores to be used for classification
#'                    (integer, min = 1, max = 2048).
#' @param output_dir  Valid directory for output file.
#'                    (character vector of length 1).
#' @param version     Version of the output file
#'                    (character vector of length 1)
#' @param progress    Logical: Show progress bar?
#'
#' @return A tibble with an classified map (class = "class_cube").
#'
#' @note
#' The \code{sits_clean} function is useful to further remove
#' classification noise which has not been detected by
#' \code{\link[sits]{sits_smooth}}. It improves the spatial consistency
#' of the classified maps.
#'
#' @examples
#' if (sits_run_examples()) {
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir()
#'     )
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir()
#'     )
#'     # apply a mode function in the labelled cube
#'     clean_cube <- sits_clean(
#'         cube = label_cube,
#'         window_size = 5,
#'         output_dir = tempdir(),
#'         multicores = 1
#'     )
#' }
#'
#' @export
sits_clean <- function(cube, ...) {
    .check_set_caller("sits_clean")
    # Precondition
    # Check the cube is valid
    .check_na_null_parameter(cube)
    UseMethod("sits_clean", cube)
}
#' @rdname sits_clean
#' @export
sits_clean.class_cube <- function(cube, ...,
                                  window_size = 5L,
                                  memsize = 4L,
                                  multicores = 2L,
                                  output_dir,
                                  version = "v1-clean",
                                  progress = TRUE) {
    # Preconditions
    # Check cube has files
    .check_raster_cube_files(cube)
    # Check window size
    .check_int_parameter(window_size, min = 3L, max = 15L, is_odd = TRUE)
    # Check memsize
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    # Check multicores
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    # Check output_dir
    .check_output_dir(output_dir)
    # Check version and progress
    version <- .message_version(version)
    progress <- .message_progress(progress)

    # Get input band
    band <- .cube_bands(cube)

    # The following functions define optimal parameters for parallel processing
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = overlap),
        npaths = 1L, nbytes = 8L,
        proc_bloat = .conf("processing_bloat")
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
        image_size = .tile_size(.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Process each tile sequentially
    clean_cube <- .cube_foreach_tile(cube, function(tile) {
        # Process the data
        .clean_tile(
            tile = tile,
            block = block,
            band = band,
            window_size = window_size,
            overlap = overlap,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    })
    # Update cube class and return
    .set_class(clean_cube, "class_cube", class(clean_cube))
}

#' @rdname sits_clean
#' @export
sits_clean.raster_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_clean"))
}
#' @rdname sits_clean
#' @export
sits_clean.derived_cube <- function(cube, ...) {
    stop(.conf("messages", "sits_clean"))
}
#' @rdname sits_clean
#' @export
sits_clean.default <- function(cube, ...) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        cube <- .cube_find_class(cube)
    } else {
        stop(.conf("messages", "sits_clean"))
    }
    sits_clean(cube, ...)
}
