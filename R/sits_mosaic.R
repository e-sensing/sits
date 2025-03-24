#' @title Mosaic classified cubes
#' @name sits_mosaic
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#'
#' @description Creates a mosaic of all tiles of a sits cube.
#' Mosaics can be created from both regularized ARD images or from classified
#' maps. In the case of ARD images, a mosaic will be produce for each band/date
#' combination. It is better to first regularize the data cubes and then
#' use \code{sits_mosaic}.
#'
#' @param cube       A sits data cube.
#' @param crs        A target coordinate reference system of raster mosaic.
#'                   The provided crs could be a string
#'                   (e.g, "EPSG:4326" or a proj4string), or
#'                   an EPSG code number (e.g. 4326).
#'                   Default is "EPSG:3857" - WGS 84 / Pseudo-Mercator.
#' @param roi        Region of interest (see below).
#' @param multicores Number of cores that will be used to
#'                   crop the images in parallel.
#' @param output_dir Directory for output images.
#' @param version    Version of resulting image
#'                   (in the case of multiple tests)
#' @param progress   Show progress bar? Default is TRUE.
#'
#' @return a sits cube with only one tile.
#'
#' @note
#'  The "roi" parameter defines a region of interest. It can be
#'  an sf_object, a shapefile, or a bounding box vector with
#'  named XY values (\code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}) or
#'  named lat/long values (\code{lon_min}, \code{lon_max},
#'    \code{lat_min}, \code{lat_max}).
#'
#'  When the data cube has tiles that cover different UTM grid zones,
#'  the user should specify the CRS of the mosaic. We use
#'  "EPSG:3857" (Pseudo-Mercator) as the default.
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
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube, output_dir = tempdir())
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         bayes_cube,
#'         output_dir = tempdir()
#'     )
#'     # create roi
#'     roi <- sf::st_sfc(
#'         sf::st_polygon(
#'             list(rbind(
#'                 c(-55.64768, -11.68649),
#'                 c(-55.69654, -11.66455),
#'                 c(-55.62973, -11.61519),
#'                 c(-55.64768, -11.68649)
#'             ))
#'         ),
#'         crs = "EPSG:4326"
#'     )
#'     # crop and mosaic classified image
#'     mosaic_cube <- sits_mosaic(
#'         cube = label_cube,
#'         roi = roi,
#'         crs = "EPSG:4326",
#'         output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_mosaic <- function(cube,
                        crs = "EPSG:3857",
                        roi = NULL,
                        multicores = 2,
                        output_dir,
                        version = "v1",
                        progress = TRUE) {
    .check_set_caller("sits_mosaic")
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_crs(crs)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_output_dir(output_dir)
    version <- .check_version(version)
    .check_lgl_parameter(progress)
    # version is case-insensitive in sits
    version <- tolower(version)
    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    # Prepare parallel processing
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Create assets as jobs
    cube_assets <- .cube_split_assets(cube)
    # Process each asset in parallel
    cube_assets <- .jobs_map_parallel_dfr(cube_assets, function(asset) {
        asset_cropped <- .mosaic_crop_asset(
            asset = asset,
            crs = crs,
            roi = roi,
            output_dir = output_dir,
            version = version
        )
        # Return a cropped asset
        asset_cropped
    }, progress = progress)
    # Join output assets as a cube
    cube <- .cube_merge_tiles(cube_assets)
    # Mosaic tiles
    .mosaic_merge_tiles(
        cube = cube,
        crs = crs,
        output_dir = output_dir,
        multicores = multicores,
        version = version,
        progress = progress
    )
}
