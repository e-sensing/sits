#' @title Mosaic classified cubes
#'
#' @name sits_mosaic
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Rolf Simoes,     \email{rolf.simoes@@inpe.br}
#'
#' @description Creates a mosaic of all tiles of a classified cube.
#' It is possible to provide a \code{roi} to cropping the mosaic.
#' If the \code{roi} parameter is supplied, each tile will be
#' cropped in parallel and the multicores parameter can be
#' used to define how many tiles will be cropped simultaneously.
#'
#' @param cube       A sits data cube.
#' @param crs        A target coordinate reference system of raster mosaic.
#'                   The provided crs could be a character
#'                   (e.g, "EPSG:4326" or a proj4string), or a
#'                   a numeric with the EPSG code (e.g. 4326).
#' @param roi        Region of interest (see below).
#' @param multicores Number of cores that will be used to
#'                   crop the images in parallel.
#' @param output_dir Directory for output images.
#' @param  version   Version of resulting image
#'                   (in the case of multiple tests)
#' @param progress   Show progress bar? Default is TRUE.
#'
#' @return a sits cube with only one tile.
#'
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lon_min", "lat_min", "lon_max", "lat_max")
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
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(bayes_cube)
#'     # create roi
#'     roi <- sf::st_sfc(
#'         sf::st_polygon(
#'           list(rbind(
#'             c(-55.64768, -11.68649),
#'             c(-55.69654, -11.66455),
#'             c(-55.62973, -11.61519),
#'             c(-55.64768, -11.68649)))), crs = 4326
#'     )
#'     # crop and mosaic classified image
#'     mosaic_cube <- sits_mosaic(
#'              cube = label_cube,
#'              roi = roi,
#'              crs = 4326
#'     )
#' }
#'
#' @export
sits_mosaic <- function(cube,
                        crs,
                        roi = NULL,
                        multicores = 2,
                        output_dir = getwd(),
                        version = "v1",
                        progress = TRUE) {
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_that(
        x = inherits(cube, c("class_cube", "uncertainty_cube")),
        msg = "cube not supported in mosaic function"
    )
    .check_crs(crs)
    .check_multicores(multicores)
    .check_output_dir(output_dir)
    .check_version(version)
    .check_progress(progress)

    # Spatial filter
    if (!is.null(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Process each tile in parallel
    cube <- .jobs_map_parallel_dfr(cube, function(tile) {
        tile_cropped <- .mosaic_crop_tile(
            tile = tile, crs = crs, roi = roi,
            output_dir = output_dir, version = version
        )
        # Return a cropped tile
        tile_cropped
    }, progress = progress)
    .mosaic_merge_tiles(
        cube = cube, crs = crs, multicores = multicores,
        output_dir = output_dir, version = version
    )
}

.mosaic_merge_tiles <- function(cube, crs, multicores, output_dir, version) {
    # Generate a vrt file
    vrt_file <- tempfile(fileext = ".vrt")
    cube_files <- unlist(.cube_paths(cube))
    .gdal_buildvrt(
        file = vrt_file, base_files = cube_files, quiet = TRUE
    )
    # Get a template tile
    base_tile <- .tile(cube)
    # Update tile name
    .tile_name(base_tile) <- "MOSAIC"
    out_file <- .file_mosaic_name(
        tile = base_tile, band = .band_derived(.tile_bands(base_tile)),
        version = version,
        output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        message("Recovery: file '", out_file, "' already exists.")
        message("(If you want to produce a new cropped image, please ",
                "change 'version' or 'output_dir' parameter)")
        base_tile <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(base_tile)),
            bands = .fi_bands(.fi(base_tile)),
            date = .tile_start_date(base_tile),
            base_tile = base_tile, update_bbox = TRUE
        )
        return(base_tile)
    }
    # Get band class configurations
    band_conf <- .conf_derived_band(
        derived_class = .cube_derived_class(cube), band = .cube_bands(cube)
    )
    # Generate raster mosaic
    .gdal_warp(
        file = out_file,
        base_files = vrt_file,
        params = list(
            "-ot" = .gdal_data_type[[.data_type(band_conf)]],
            "-of" = .conf("gdal_presets", "image", "of"),
            "-co" = .conf("gdal_presets", "image", "co"),
            "-t_srs" = .as_crs(crs),
            "-wo" = paste0("NUM_THREADS=", multicores),
            "-multi" = TRUE,
            "-srcnodata" = .miss_value(band_conf)
        ),
        quiet = TRUE
    )
    # Create COG overviews
    .gdal_addo(base_file = out_file)
    # Create tile based on template
    base_tile <- .tile_derived_from_file(
        file = out_file, band = .tile_bands(base_tile),
        base_tile = base_tile, derived_class = .tile_derived_class(base_tile),
        labels = .tile_labels(base_tile),
        update_bbox = TRUE
    )
    # Delete cube files
    unlink(c(cube_files, vrt_file))
    # Return cube
    return(base_tile)
}

.mosaic_crop_tile <- function(tile, crs, roi, output_dir, version) {
    # Get tile path
    file <- .tile_path(tile)
    # Create output file name
    out_file <- .file_crop_name(
        tile = tile, band = .tile_bands(tile),
        version = version, output_dir = output_dir
    )
    # Get band configs from tile
    band_conf <- .conf_derived_band(
        derived_class = .tile_derived_class(tile), band = .tile_bands(tile)
    )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        message("Recovery: file '", out_file, "' already exists.")
        message("(If you want to produce a new cropped image, please ",
                "change 'version' or 'output_dir' parameter)")
        tile <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(tile)),
            bands = .fi_bands(.fi(tile)), date = .tile_start_date(tile),
            base_tile = tile, update_bbox = TRUE
        )
        return(tile)
    }
    # If the tile is fully contained in roi it's not necessary to crop it
    if (!is.null(roi)) {
        # Is tile contained in roi?
        is_within <- .tile_within(tile, roi)
        if (is_within) {
            # Reproject tile for its crs
            .gdal_reproject_image(
                file = file, out_file = out_file,
                crs = .as_crs(.tile_crs(tile)),
                as_crs = .mosaic_crs(tile = tile, as_crs = crs),
                miss_value = .miss_value(band_conf), multicores = 1
            )
            tile <- .tile_class_from_file(
                file = out_file, band = .tile_bands(tile), base_tile = tile
            )
            return(tile)
        }
        # TODO: include this operation inside gdal_crop_image()
        # Write roi in a temporary file
        roi <- .roi_write(
            roi = roi,
            output_file = tempfile(fileext = ".shp"),
            quiet = TRUE
        )
    }
    # Crop tile image
    out_file <- .gdal_crop_image(
        file = file, out_file = out_file,
        roi = roi, crs = .as_crs(.tile_crs(tile)),
        as_crs = .mosaic_crs(tile = tile, as_crs = crs),
        miss_value = .miss_value(band_conf),
        multicores = 1
    )
    # Delete temporary roi file
    .mosaic_del_roi(roi)
    # Update asset metadata
    update_bbox <- if (is.null(roi)) FALSE else TRUE
    tile <- .tile_eo_from_files(
        files = out_file, fid = .fi_fid(.fi(tile)),
        bands = .fi_bands(.fi(tile)), date = .tile_start_date(tile),
        base_tile = tile, update_bbox = update_bbox
    )
    tile
}

.mosaic_del_roi <- function(roi) {
    if (is.null(roi)) {
        return(roi)
    }
    dir_name <- dirname(roi)
    file_name <- .file_sans_ext(roi)
    shp_exts <- c(".shp", ".shx", ".dbf", ".prj")
    unlink(paste0(file.path(dir_name, file_name), shp_exts))
}

.mosaic_type <- function(tile) {
    if (.cube_source(tile) %in% "BDC") {
        return("BDC")
    }
    return("RASTER")
}

.mosaic_switch <- function(tile, ...) {
    switch(.mosaic_type(tile), ...)
}

.mosaic_crs <- function(tile, as_crs) {
    .mosaic_switch(
        tile,
        "BDC" = .as_crs("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs "),
        "RASTER" = .as_crs(as_crs)
    )
}

.roi_write <- function(roi, output_file, quiet, ...) {
    sf::st_write(obj = roi, dsn = output_file, quiet = quiet, ...)
    output_file
}
