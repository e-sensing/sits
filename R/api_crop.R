#' @title Crop cube
#' @keywords internal
#' @noRd
#' @param  cube         Data cube
#' @param  roi          ROI to crop
#' @param  output_dir   Directory where file will be written
#' @return              Cropped data cube
.crop <- function(cube,
                  roi = NULL,
                  multicores = 2,
                  output_dir,
                  progress = TRUE) {
    .check_set_caller("sits_crop")
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_int_parameter(multicores, min = 1, max = 2048)
    .check_output_dir(output_dir)
    .check_lgl_parameter(progress)
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
        # Get asset file path
        file <- .tile_path(asset)
        output_dir <- .file_path_expand(output_dir)
        .check_that(
            output_dir != .file_dir(file),
            local_msg = "Source and destination directories must be different",
            msg = "Invalid `output_dir` parameter"
        )
        # Create output file name
        out_file <- .file_path(.file_base(file), output_dir = output_dir)
        # Resume feature
        if (.raster_is_valid(out_file, output_dir = output_dir)) {
            .check_recovery(out_file)
            asset_cropped <- .tile_from_file(
                file = out_file, base_tile = asset,
                band = .tile_bands(asset), update_bbox = TRUE,
                labels = .tile_labels(asset)
            )
            return(asset_cropped)
        }

        asset_cropped <- .crop_asset(
            asset = asset,
            roi = roi,
            output_file = out_file
        )
        # Return a cropped asset
        asset_cropped
    }, progress = progress)
    # Join output assets as a cube
    cube <- .cube_merge_tiles(cube_assets)
    # Return cropped cube
    cube
}
#' @title Crop asset
#' @keywords internal
#' @noRd
#' @param  asset        Data cube
#' @param  roi          ROI to crop
#' @param  output_file  Output file where image will be written
#' @param  gdal_params  Additional parameters to crop using gdal warp
#' @return              Cropped data cube
.crop_asset <- function(asset, roi, output_file, gdal_params = NULL) {
    # Get band configs from tile
    band_conf <- .tile_band_conf(asset, band = .tile_bands(asset))
    # If the asset is fully contained in roi it's not necessary to crop it
    if (!.has(roi) || .tile_within(asset, roi)) {
        # Copy image to output_dir
        .gdal_warp(
            base_files = file,
            file = output_file,
            params = list("-overwrite" = TRUE),
            quiet = FALSE
        )
        # Update asset metadata
        asset <- .tile_from_file(
            file = output_file, base_tile = asset,
            band = .tile_bands(asset), update_bbox = FALSE,
            labels = .tile_labels(asset)
        )
        return(asset)
    } else if (.has(roi)) {
        # Write roi in a temporary file
        roi_file <- .roi_write(
            roi = roi,
            output_file = tempfile(fileext = ".gpkg"),
            quiet = TRUE
        )
        # Delete temporary roi_file
        on.exit(.roi_delete(roi_file), add = TRUE)
        # Crop and reproject tile image
        output_file <- .gdal_crop_image(
            file = file,
            out_file = output_file,
            roi_file = roi_file,
            as_crs = NULL,
            miss_value = .miss_value(band_conf),
            data_type = .data_type(band_conf),
            multicores = 1,
            overwrite = TRUE,
            gdal_params
        )
        # Update asset metadata
        asset <- .tile_from_file(
            file = output_file, base_tile = asset,
            band = .tile_bands(asset), update_bbox = TRUE,
            labels = .tile_labels(asset)
        )
        return(asset)
    }
}
