#' @title Cleans a classified map using a windowed mode
#'
#' @name sits_clean
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description
#' Applies a windowed mode function to clean up possible noisy pixels keeping
#' the most frequently values within the neighborhood.
#' In a tie, the first value of the vector is considered.
#'
#' @param cube        A classified data cube
#' @param window_size An odd number representing the size of the
#'                    sliding window of the modal function.
#' @param memsize     Memory available for classification (in GB).
#' @param multicores  Number of cores to be used for classification.
#' @param output_dir  Directory where files will be saved.
#' @param version     Version of the output file.
#' @param progress    Show progress bar?
#'
#' @return A data cube with an classified map cleaned.
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
#'     data = cube, ml_model = rf_model, output_dir = tempdir()
#' )
#' # label the probability cube
#' label_cube <- sits_label_classification(
#'     probs_cube, output_dir = tempdir()
#' )
#' # apply a mode function in the labelled cube
#' clean_cube <- sits_clean(
#'     label_cube, window_size = 5,  output_dir = tempdir()
#' )
#' }
#'
#' @export
sits_clean <- function(cube,
                       window_size,
                       memsize = 4,
                       multicores = 2,
                       output_dir,
                       version = "v1",
                       progress = TRUE) {
    # Check cube
    .check_cube_is_class_cube(cube)
    # Check window size
    .check_window_size(window_size)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output_dir
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)
    # Check progress
    .check_progress(progress)

    # Get input band
    band <- .cube_bands(cube)

    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2) - 1
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block, overlap = overlap),
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
    assets_band <- .jobs_map_parallel_dfr(assets_cube, function(asset) {
        # Process the data
        output_asset <- .clean_asset(
            asset = asset,
            block = block,
            band = band,
            window_size = window_size,
            overlap = overlap,
            output_dir = output_dir,
            version = version
        )
        return(output_asset)
    }, progress = progress)
    # Join output assets and return it
    .cube_merge_tiles(assets_band)
}
