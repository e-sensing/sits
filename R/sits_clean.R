#' @title Cleans a classified map using a local window
#'
#' @name sits_clean
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Applies a modal function to clean up possible noisy pixels keeping
#' the most frequently values within the neighborhood.
#' In a tie, the first value of the vector is considered.
#'
#' @param cube        Classified data cube (tibble of class "class_cube").
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
#' @examples
#' if (sits_run_examples()) {
#' rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#' # create a data cube from local files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "BDC",
#'     collection = "MOD13Q1-6",
#'     data_dir = data_dir
#' )
#' # classify a data cube
#' probs_cube <- sits_classify(
#'     data = cube,
#'     ml_model = rf_model,
#'     output_dir = tempdir(),
#'     version = "ex_clean"
#' )
#' # label the probability cube
#' label_cube <- sits_label_classification(
#'     probs_cube,
#'     output_dir = tempdir(),
#'     version = "ex_clean"
#' )
#' # apply a mode function in the labelled cube
#' clean_cube <- sits_clean(
#'     cube = label_cube,
#'     window_size = 5,
#'     memsize = 4,
#'     multicores = 2,
#'     output_dir = tempdir(),
#'     version = "ex_clean",
#'     progress = FALSE
#' )
#' }
#'
#' @export
sits_clean <- function(cube, window_size = 5L, memsize = 4L,
                       multicores = 2L, output_dir, version = "v1",
                       progress = TRUE) {
    # Precondition
    # Check the cube is valid
    .check_valid(cube)
    UseMethod("sits_clean", cube)
}
#' @rdname sits_clean
#' @export
sits_clean.class_cube <- function(cube, window_size = 5L, memsize = 4L,
                                  multicores = 2L, output_dir, version = "v1",
                                  progress = TRUE) {
    # Preconditions
    # Check cube has files
    .check_cube_files(cube)
    # Check window size
    .check_window_size(window_size, min = 1, max = 15)
    # Check memsize
    .check_memsize(memsize, min = 1, max = 16384)
    # Check multicores
    .check_multicores(multicores, min = 1, max = 2048)
    # Check output_dir
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)
    # version is case-insensitive in sits
    version <- tolower(version)
    # Check progress
    .check_progress(progress)

    # Get input band
    band <- .cube_bands(cube)
    # image size
    image_size <- .raster_size(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = image_size, overlap = overlap),
        npaths = 1, nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter
    multicores <- .jobs_max_multicores(
        job_memsize = job_memsize, memsize = memsize, multicores = multicores
    )
    # Prepare parallelization
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)

    # Create assets as jobs
    assets_cube <- .cube_split_assets(cube)

    # Process each feature in parallel
    assets_band <- .jobs_map_sequential_dfr(assets_cube, function(asset) {
        # Process the data
        output_asset <- .clean_asset(
            asset = asset,
            block = image_size,
            band = band,
            window_size = window_size,
            overlap = overlap,
            output_dir = output_dir,
            version = version
        )
        return(output_asset)
    })
    # Join output assets and return it
    clean_cube <- .cube_merge_tiles(assets_band)
    class(clean_cube) <- c("class_cube", class(clean_cube))
    return(clean_cube)
}

#' @rdname sits_clean
#' @export
sits_clean.raster_cube <- function(cube, window_size = 5L, memsize = 4L,
                                   multicores = 2L, output_dir, version = "v1",
                                   progress = TRUE) {
    stop("Input should be a classified cube")
    return(cube)
}
#' @rdname sits_clean
#' @export
sits_clean.derived_cube <- function(cube, window_size = 5L, memsize = 4L,
                                    multicores = 2L, output_dir, version = "v1",
                                    progress = TRUE) {
    stop("Input should be a classified cube")
    return(cube)
}
#' @rdname sits_clean
#' @export
sits_clean.tbl_df <- function(cube, window_size = 5L, memsize = 4L,
                              multicores = 2L, output_dir, version = "v1",
                              progress = TRUE) {
    cube <- tibble::as_tibble(cube)
    if (all(.conf("sits_cube_cols") %in% colnames(cube))) {
        cube <- .cube_find_class(cube)
    } else
        stop("Input should be a classified cube")
    clean_cube <- sits_clean(cube, window_size, memsize, multicores,
                       output_dir, version, progress)
    return(clean_cube)
}
#' @rdname sits_clean
#' @export
sits_clean.default <- function(cube, window_size = 5L, memsize = 4L,
                               multicores = 2L, output_dir, version = "v1",
                               progress = TRUE) {
    stop("Input should be a classified cube")
}
