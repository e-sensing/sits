#' @title Crop cube
#' @name .crop
#' @description cuts a data cube according to a ROI
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  cube         Data cube
#' @param  output_dir   Directory where file will be written
#' @param  roi          ROI to crop
#' @param  overwrite    Overwrite existing output file (Default is FALSE)
#' @param  progress     Show progress bar??
#' @return              Cropped data cube
.crop <- function(cube,
                  output_dir,
                  roi = NULL,
                  multicores = 2L,
                  overwrite = FALSE,
                  progress = progress) {
    .check_set_caller("sits_crop")
    # Pre-conditions
    .check_is_raster_cube(cube)
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    .check_output_dir(output_dir)
    .check_lgl_parameter(progress)
    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        cube <- .cube_filter_spatial(cube = cube, roi = roi)
    }
    # Prepare parallel processing
    is_child_process <- .parallel_is_open()
    .parallel_start(workers = multicores)
    # If a child process calls this function
    # on.exit was already set in the main process
    if (!is_child_process) {
        on.exit(.parallel_stop(), add = TRUE)
    }
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
        if (!overwrite && .raster_is_valid(out_file, output_dir = output_dir)) {
            .check_recovery()
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
#' @name .crop_asset
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @param  asset        Data cube
#' @param  roi          ROI to crop
#' @param  output_file  Output file where image will be written
#' @param  gdal_params  Additional parameters to crop using gdal warp
#' @return              Cropped data cube
.crop_asset <- function(asset, roi, output_file, gdal_params = list()) {
    # Get asset path and expand it
    file <- .file_path_expand(.tile_path(asset))
    # Get band configs from tile
    band_conf <- .tile_band_conf(asset, band = .tile_bands(asset))
    # If the asset is fully contained in roi it's not necessary to crop it
    if (!.has(roi) || .tile_within(asset, roi)) {
        # Define gdal params
        gdal_params <- utils::modifyList(gdal_params, list("-overwrite" = TRUE))
        # Copy image to output_dir
        .gdal_warp(
            base_files = file,
            file = output_file,
            params = gdal_params,
            quiet = TRUE,
            conf_opts = unlist(.conf("gdal_read_options"))
        )
        # Update asset metadata
        asset <- .tile_from_file(
            file = output_file, base_tile = asset,
            band = .tile_bands(asset), update_bbox = FALSE,
            labels = .tile_labels(asset)
        )
        return(asset)
    } else if (.has(roi)) {
        # Compute the intersection between roi and tile bbox
        roi <- .intersection(.bbox_as_sf(.tile_bbox(asset)), roi)
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
            multicores = 1L,
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
