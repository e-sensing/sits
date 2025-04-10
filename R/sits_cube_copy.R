#' @title Copy the images of a cube to a local directory
#' @name sits_cube_copy
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @description
#'
#' This function downloads the images of a cube in parallel.
#' A region of interest (\code{roi}) can be provided to crop
#' the images and a resolution (\code{res}) to resample the
#' bands. \code{sits_cube_copy} is useful to improve processing time in the
#' regularization operation.
#'
#' @param cube       A data cube (class "raster_cube")
#' @param roi        Region of interest. Either:
#'                  \enumerate{
#'                  \item{A path to a shapefile with polygons;}
#'                  \item{A \code{sf} object from \code{sf} package;}
#'                  \item{A named \code{vector} (\code{"lon_min"},
#'                        \code{"lat_min"}, \code{"lon_max"}, \code{"lat_max"})
#'                        in WGS84;}
#'                  \item{A named \code{vector} (\code{"xmin"}, \code{"xmax"},
#'                        \code{"ymin"}, \code{"ymax"}) with XY coordinates
#'                        in WGS84.}
#'                   }
#' @param res        An integer value corresponds to the output
#'                   spatial resolution of the images. Default is NULL.
#' @param crs        Reference system for output cube (by default,
#'                   the same CRS from the input cube is assumed)
#' @param n_tries    Number of attempts to download the same image.
#'                   Default is 3.
#' @param multicores Number of cores for parallel downloading
#'                   (integer, min = 1, max = 2048).
#' @param output_dir Output directory where images will be saved.
#'                   (character vector of length 1).
#' @param progress   Logical: show progress bar?
#'
#' @return Copy of input data cube (class "raster cube").
#'
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies the ARD image collection
#'          from a cloud provider to a local directory for faster processing.}
#'      \item{\code{\link[sits]{sits_regularize}}: create a regular data cube
#'          from an ARD image collection.}
#'      \item{\code{\link[sits]{sits_apply}}: create new indices by combining
#'          bands of a  regular data cube (optional).}
#'      \item{\code{\link[sits]{sits_get_data}}: extract time series
#'          from a regular data cube based on user-provided labelled samples.}
#'      \item{\code{\link[sits]{sits_train}}: train a machine learning
#'          model based on image time series.}
#'      \item{\code{\link[sits]{sits_classify}}: classify a data cube
#'          using a machine learning model and obtain a probability cube.}
#'      \item{\code{\link[sits]{sits_smooth}}: post-process a probability cube
#'          using a spatial smoother to remove outliers and
#'          increase spatial consistency.}
#'      \item{\code{\link[sits]{sits_label_classification}}: produce a
#'          classified map by selecting the label with the highest probability
#'          from a smoothed cube.}
#' }
#' @examples
#' if (sits_run_examples()) {
#'     # Creating a sits cube from BDC
#'     bdc_cube <- sits_cube(
#'         source = "BDC",
#'         collection = "CBERS-WFI-16D",
#'         tiles = c("007004", "007005"),
#'         bands = c("B15", "CLOUD"),
#'         start_date = "2018-01-01",
#'         end_date = "2018-01-12"
#'     )
#'     # Downloading images to a temporary directory
#'     cube_local <- sits_cube_copy(
#'         cube = bdc_cube,
#'         output_dir = tempdir(),
#'         roi = c(
#'             lon_min = -46.5,
#'             lat_min = -45.5,
#'             lon_max = -15.5,
#'             lat_max = -14.6
#'         ),
#'         multicores = 2L,
#'         res = 250,
#'     )
#' }
#'
#' @export
sits_cube_copy <- function(cube,
                           roi = NULL,
                           res = NULL,
                           crs = NULL,
                           n_tries = 3,
                           multicores = 2L,
                           output_dir,
                           progress = TRUE) {
    # Set caller for error msgs
    .check_set_caller("sits_cube_copy")
    # Pre-conditions
    .check_is_raster_cube(cube)
    # Check n_tries parameter
    .check_num_min_max(x = n_tries, min = 1, max = 50)
    # Check files
    .check_raster_cube_files(cube)
    # Spatial filter
    if (.has(roi)) {
        # if crs is not NULL, use user parameter as default
        # else use input cube crs
        roi <- .roi_as_sf(
            roi,
            default_crs = ifelse(.has(crs), crs, .cube_crs(cube))
        )
        cube <- .cube_filter_spatial(cube = cube, roi = roi)

        if (!.cube_has_unique_resolution(cube)) {
            .check_that(
                .has(res),
                msg = .conf("messages", "sits_cube_copy_different_resolutions")
            )
        }
    }
    .check_int_parameter(multicores, min = 1, max = 2048)
    # Check Output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check progress
    progress <- .message_progress(progress)
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Adjust tile system name
    cube <- .cube_convert_tile_name(cube)
    # Update token (for big tiffs and slow networks)
    cube <- .cube_token_generator(cube)
    # Create assets as jobs
    cube_assets <- .cube_split_assets(cube)
    # Process each tile sequentially
    cube_assets <- .jobs_map_parallel_dfr(cube_assets, function(asset) {
        .download_asset(
            asset = asset,
            roi = roi,
            res = res,
            n_tries = n_tries,
            output_dir = output_dir
        )
    }, progress = progress)
    # Check and return
    .check_empty_data_frame(cube_assets)
    cube_assets <- .cube_merge_tiles(cube_assets)
    # Revert tile system name
    .cube_revert_tile_name(cube_assets)
}
