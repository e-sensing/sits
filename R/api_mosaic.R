#' @title Split data cube by band and date
#' @keywords internal
#' @noRd
#' @param  cube      Data cube
#' @return           Data.frame with unnested cube by band and date
.mosaic_split_band_date <- function(cube) {
    UseMethod(".mosaic_split_band_date", cube)
}
#' @title Split raster data cube by band and date
#' @keywords internal
#' @noRd
#' @param  cube      Data cube
#' @return           Data.frame with unnested cube
#' @export
.mosaic_split_band_date.raster_cube <- function(cube) {
    data <- tidyr::unnest(
        cube,
        cols = "file_info",
        names_sep = "."
    )
    data <- dplyr::mutate(
        data,
        job_date = .data[["file_info.date"]],
        job_band = .data[["file_info.band"]]
    )
    data <- dplyr::group_by(
        data,
        .data[["job_date"]],
        .data[["job_band"]]
    )
    data <- tidyr::nest(
        data,
        file_info = dplyr::starts_with("file_info"),
        .names_sep = "."
    )
    data <- .set_class(data, class(cube))
    data <- tidyr::nest(
        data,
        cube = -c("job_date", "job_band")
    )
    data
}
#' @title Split derived data cube by band and date
#' @keywords internal
#' @noRd
#' @param  cube      Data cube
#' @return           Data.frame with unnested cube
#' @export
.mosaic_split_band_date.derived_cube <- function(cube) {
    data <- tidyr::unnest(
        cube,
        cols = "file_info",
        names_sep = "."
    )
    data <- dplyr::mutate(
        data,
        job_start_date = .data[["file_info.start_date"]],
        job_end_date = .data[["file_info.end_date"]],
        job_band = .data[["file_info.band"]]
    )
    data <- dplyr::group_by(
        data,
        .data[["job_start_date"]],
        .data[["job_end_date"]],
        .data[["job_band"]]
    )
    data <- tidyr::nest(
        data,
        file_info = dplyr::starts_with("file_info"),
        .names_sep = "."
    )
    data <- .set_class(data, class(cube))
    data <- tidyr::nest(
        data,
        cube = -c("job_start_date", "job_end_date", "job_band")
    )
    data
}
#' @title Merge tiles to get mosaic
#' @keywords internal
#' @noRd
#' @param  cube         Data cube
#' @param  crs          CRS of the mosaic
#' @param  output_dir   Directory where file will be written
#' @param  multicores   Number of cores used for regularization.
#' @param  version      Version of result.
#' @param  progress     Show progress bar?
#' @return              Merged data cube
.mosaic_merge_tiles <- function(cube,
                                crs,
                                output_dir,
                                multicores,
                                version,
                                progress) {
    # Create band date as jobs
    band_date_cube <- .mosaic_split_band_date(cube)
    # Get band configs from tile
    band_conf <- .tile_band_conf(.tile(cube), band = .cube_bands(cube))
    # Get cube file paths
    cube_files <- unlist(.cube_paths(cube))
    on.exit(unlink(cube_files))
    # Process jobs in parallel
    mosaic_cube <- .jobs_map_parallel_dfr(band_date_cube, function(job) {
        # Get cube as a job
        cube <- job[["cube"]][[1]]
        # Get cube file paths
        cube_files <- unlist(.cube_paths(cube))
        # Get a template tile
        base_tile <- .tile(cube)
        # Update tile name
        .tile_name(base_tile) <- "MOSAIC"
        out_file <- .file_mosaic_name(
            tile = base_tile,
            band = .tile_bands(base_tile),
            version = version,
            output_dir = output_dir
        )
        # Resume feature
        if (.raster_is_valid(out_file, output_dir = output_dir)) {
            if (.check_messages()) {
                .check_recovery(out_file)
            }
            base_tile <- .tile_from_file(
                file = out_file, base_tile = base_tile,
                band = .tile_bands(base_tile), update_bbox = TRUE,
                labels = .tile_labels(base_tile)
            )
            return(base_tile)
        }

        # Generate raster mosaic
        .gdal_warp(
            file = out_file,
            base_files = cube_files,
            params = list(
                "-ot" = .gdal_data_type[[.data_type(band_conf)]],
                "-of" = .conf("gdal_presets", "image", "of"),
                "-co" = .conf("gdal_presets", "image", "co"),
                "-t_srs" = .as_crs(crs),
                "-wo" = paste0("NUM_THREADS=", multicores),
                "-multi" = FALSE,
                "-srcnodata" = .miss_value(band_conf)
            ),
            quiet = TRUE
        )
        # Create COG overviews
        .gdal_addo(base_file = out_file)
        # Create tile based on template
        base_tile <- .tile_from_file(
            file = out_file, base_tile = base_tile,
            band = .tile_bands(base_tile), update_bbox = TRUE,
            labels = .tile_labels(base_tile)
        )
        # Return cube
        return(base_tile)
    }, progress = progress)
    # Join output assets as a cube and return it
    .cube_merge_tiles(mosaic_cube)
}
#' @title Crop asset as a part of mosaicking
#' @keywords internal
#' @noRd
#' @param  asset        Data cube
#' @param  crs          CRS of the mosaic
#' @param  output_dir   Directory where file will be written
#' @param  version      Version of result.
#' @return              Cropped data cube
.mosaic_crop_asset <- function(asset, crs, roi, output_dir, version) {
    # Get asset file path
    file <- .tile_path(asset)
    # Create output file name
    out_file <- .file_crop_name(
        tile = asset, band = .tile_bands(asset),
        version = version, output_dir = output_dir
    )
    # Create a temporary output file name
    # (this is required as in Windows machines, GDAL can't read and write
    # using the same file)
    out_file_base <- .file_crop_name(
        tile = asset, band = .tile_bands(asset),
        version = paste0(version, "mosaic"), output_dir = output_dir
    )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        .check_recovery(out_file)
        asset <- .tile_from_file(
            file = out_file, base_tile = asset,
            band = .tile_bands(asset), update_bbox = TRUE,
            labels = .tile_labels(asset)
        )
        return(asset)
    }
    # Get band configs from tile
    band_conf <- .tile_band_conf(asset, band = .tile_bands(asset))
    # Scaling image to byte
    .gdal_scale(
        file = file,
        out_file = out_file,
        src_min = .min_value(band_conf),
        src_max = .max_value(band_conf),
        dst_min = .min_value(band_conf),
        dst_max = .max_value(band_conf),
        miss_value = .miss_value(band_conf),
        data_type = .data_type(band_conf)
    )
    # If the asset is fully contained in roi it's not necessary to crop it
    if (.has(roi)) {
        # Is asset within in roi?
        is_within <- .tile_within(asset, roi)
        if (is_within) {
            # Reproject tile for its crs
            .gdal_reproject_image(
                file = out_file, out_file = out_file,
                crs = .as_crs(.tile_crs(asset)),
                as_crs = .mosaic_crs(tile = asset, as_crs = crs),
                miss_value = .miss_value(band_conf),
                data_type = .data_type(band_conf),
                multicores = 1,
                overwrite = TRUE
            )
            asset <- .tile_from_file(
                file = out_file, base_tile = asset,
                band = .tile_bands(asset), update_bbox = FALSE,
                labels = .tile_labels(asset)
            )
            return(asset)
        }
        # Write roi in a temporary file
        roi <- .roi_write(
            roi = roi,
            output_file = tempfile(fileext = ".shp"),
            quiet = TRUE
        )
        # Delete temporary roi file
        on.exit(.roi_delete(roi))
    }
    # Crop and reproject tile image
    out_file <- .gdal_crop_image(
        file = out_file,
        out_file = out_file,
        roi_file = roi,
        as_crs = .mosaic_crs(tile = asset, as_crs = crs),
        miss_value = .miss_value(band_conf),
        data_type = .data_type(band_conf),
        multicores = 1,
        overwrite = TRUE
    )
    # Move the generated file to use the correct name
    file.rename(out_file_base, out_file)
    # Update asset metadata
    update_bbox <- if (.has(roi)) TRUE else FALSE
    asset <- .tile_from_file(
        file = out_file, base_tile = asset,
        band = .tile_bands(asset), update_bbox = update_bbox,
        labels = .tile_labels(asset)
    )
    return(asset)
}
#' @title Delete ROI
#' @keywords internal
#' @noRd
#' @param  roi          Region of interest
#' @return              Called for side effects
.roi_delete <- function(roi) {
    if (is.null(roi)) {
        return(roi)
    }
    dir_name <- dirname(roi)
    file_name <- .file_sans_ext(roi)
    shp_exts <- c(".shp", ".shx", ".dbf", ".prj")
    unlink(paste0(file.path(dir_name, file_name), shp_exts))
    return(invisible(roi))
}
#' @title Get type of mosaic
#' @keywords internal
#' @noRd
#' @param  tile         Tile of data cube
#' @return              BDC or RASTER
.mosaic_type <- function(tile) {
    if (.cube_source(tile) == "BDC") {
        return("BDC")
    }
    return("RASTER")
}
#' @title Switch based on mosaic type
#' @keywords internal
#' @noRd
#' @param  tile         Tile of data cube
#' @return              Result dependent on the type
.mosaic_switch <- function(tile, ...) {
    switch(.mosaic_type(tile), ...)
}
#' @title Get mosaic CRS
#' @keywords internal
#' @noRd
#' @param  tile         Tile of data cube
#' @return              Either BDC Albers projection or CRS for other rasters
.mosaic_crs <- function(tile, as_crs) {
    .mosaic_switch(
        tile,
        BDC = .as_crs("+proj=aea
                        +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22
                        +x_0=5000000 +y_0=10000000
                        +ellps=GRS80 +units=m +no_defs "),
        RASTER = .as_crs(as_crs)
    )
}
