#' @title Downloads an asset
#' @noRd
#' @param asset   File to be downloaded (with path)
#' @param res     Spatial resolution
#' @param sf_roi  Region of interest (sf object)
#' @param n_tries Number of tries to download the same image.
#' @param output_dir Directory where file will be saved
#' @param progress Show progress bar?
#' @returns  Updated asset
.download_asset <- function(asset, res, roi, n_tries, output_dir, progress, ...) {
    # Get asset path and expand it
    file <- .file_path_expand(.tile_path(asset))

    # Create a list of user parameters as gdal format
    gdal_params <- .gdal_format_params(asset = asset, roi = roi, res = res)

    # Update cube bbox
    update_bbox <- FALSE
    if (.has(res) || .has(roi)) {
        update_bbox <- TRUE
    }

    # Get asset path and expand it
    file <- .file_path_expand(.tile_path(asset))
    # Create output file
    asset[["sensor"]] <- .download_remove_slash(.tile_sensor(asset))
    out_file <- .file_eo_name(
        tile = asset,
        band = .tile_bands(asset),
        date = .tile_start_date(asset),
        output_dir = output_dir
    )

    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        if (.check_messages()) {
            .check_recovery(out_file)
        }
        asset <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(asset)),
            bands = .tile_bands(asset), date = .tile_start_date(asset),
            base_tile = asset, update_bbox = update_bbox
        )
        return(asset)
    }

    # gdal_open_params <- .conf("gdal_read_options")
    # # Download file
    # while (n_tries > 0) {
    #     out <- .try(
    #
    #         .gdal_crop_image(
    #             file = out_file, base_file = file,
    #
    #         )
    #
    #
    #         .gdal_translate(
    #             file = out_file, base_file = file,
    #             params = params,
    #             conf_opts = gdal_open_params,
    #             quiet = TRUE
    #         ), default = NULL
    #     )
    #     if (.raster_is_valid(out)) {
    #         return(out_file)
    #     }
    #     n_tries <- n_tries - 1
    #
    #     secs_to_retry <- sample(x = seq.int(10, 30), size = 1)
    #     Sys.sleep(secs_to_retry)
    #     message(paste("Trying to download image in X seconds", file))
    # }
    # if (!.has(out)) {
    #     warning(paste("Error in downloading file", file))
    # }
    # # Return file name
    # out_file

    # Download file
    download_fn <- .download_gdal(
        file = file,
        out_file = out_file,
        params = gdal_params,
        n_tries = n_tries
    )

    # Update asset metadata
    asset <- .tile_eo_from_files(
        files = out_file, fid = .fi_fid(.fi(asset)),
        bands = .tile_bands(asset), date = .tile_start_date(asset),
        base_tile = asset, update_bbox = update_bbox
    )
    # Return updated asset
    return(asset)
}

#' @title Download function when using GDAL
#' @noRd
#' @param out_file    Path where file will be saved
#' @param gdal_params GDAL parameters
#' @returns  Appropriate GDAL download function
.download_gdal <- function(file, out_file, params, n_tries, ...) {
    gdal_open_params <- .conf("gdal_read_options")
    # Download file
    while (n_tries > 0) {
        out <- .try(
            .gdal_translate(
                file = out_file, base_file = file,
                params = params,
                conf_opts = gdal_open_params,
                quiet = TRUE
            ), default = NULL
        )
        if (.raster_is_valid(out)) {
            return(out_file)
        }
        n_tries <- n_tries - 1

        secs_to_retry <- sample(x = seq.int(10, 30), size = 1)
        Sys.sleep(secs_to_retry)
        message(paste("Trying to download image in X seconds", file))
    }
    if (!.has(out)) {
        warning(paste("Error in downloading file", file))
    }
    # Return file name
    out_file
}

#' @title Remove slash from sensor name
#' @noRd
#' @param x    Sensor name (e.g. "TM/OLI")
#' @returns    Sensor name without slashes
.download_remove_slash <- function(x) {
    gsub(pattern = "/", replacement = "", x = x, fixed = TRUE)
}
