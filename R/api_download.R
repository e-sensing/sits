#' @title Downloads an asset
#' @noRd
#' @param asset   File to be downloaded (with path)
#' @param res     Spatial resolution
#' @param sf_roi  Region of interest (sf object)
#' @param n_tries Number of tries to download the same image.
#' @param output_dir Directory where file will be saved
#' @param progress Show progress bar?
#' @returns  Updated asset
.download_asset <- function(asset, res, sf_roi, n_tries, output_dir,
                            progress, ...) {
    # Get all paths and expand
    file <- .file_path_expand(.tile_path(asset))
    # Create a list of user parameters as gdal format
    gdal_params <- .gdal_format_params(
        asset = asset,
        sf_roi = sf_roi,
        res = res
    )
    # Create output file
    out_file <- .file_path(
        .tile_satellite(asset),
        .download_remove_slash(.tile_sensor(asset)),
        .tile_name(asset),
        .tile_bands(asset),
        .tile_start_date(asset),
        output_dir = output_dir,
        ext = "tif"
    )
    if (inherits(asset, "derived_cube")) {
        out_file <- paste0(output_dir, "/", basename(file))
    }
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        if (.check_messages()) {
            .check_recovery(out_file)
        }
        asset <- .download_update_asset(
            asset = asset, roi = sf_roi, res = res, out_file = out_file
        )
        return(asset)
    }
    # Get a gdal or default download
    download_fn <- .download_controller(
        out_file = out_file, gdal_params = gdal_params
    )
    # Download file
    suppressWarnings(download_fn(file, n_tries, ...))
    # Update asset metadata
    asset <- .download_update_asset(
        asset = asset, roi = sf_roi, res = res, out_file = out_file
    )
    # Return updated asset
    asset
}
#' @title Updates an asset for download
#' @noRd
#' @param asset  File to be downloaded (with path)
#' @param roi Region of interest (sf object)
#' @param res    Spatial resolution
#' @param out_file Path where file will be saved
#' @returns  Updated asset
.download_update_asset <- function(asset, roi, res, out_file) {
    if (!is.null(roi) || !is.null(res)) {
        # Open raster
        r_obj <- .raster_open_rast(out_file)
        # Update spatial bbox
        .xmin(asset) <- .raster_xmin(r_obj)
        .xmax(asset) <- .raster_xmax(r_obj)
        .ymin(asset) <- .raster_ymin(r_obj)
        .ymax(asset) <- .raster_ymax(r_obj)
        .crs(asset) <- .raster_crs(r_obj)
        asset[["file_info"]][[1]][["ncols"]] <- .raster_ncols(r_obj)
        asset[["file_info"]][[1]][["nrows"]] <- .raster_nrows(r_obj)
        asset[["file_info"]][[1]][["xres"]] <- .raster_xres(r_obj)
        asset[["file_info"]][[1]][["yres"]] <- .raster_yres(r_obj)
        asset[["file_info"]][[1]][["xmin"]] <- .raster_xmin(r_obj)
        asset[["file_info"]][[1]][["xmax"]] <- .raster_xmax(r_obj)
        asset[["file_info"]][[1]][["ymin"]] <- .raster_ymin(r_obj)
        asset[["file_info"]][[1]][["ymax"]] <- .raster_ymax(r_obj)
    }
    asset[["file_info"]][[1]][["path"]] <- out_file
    return(asset)
}
#' @title Choice of appropriate download function
#' @noRd
#' @param out_file    Path where file will be saved
#' @param gdal_params GDAL parameters
#' @returns  Appropriate download function
.download_controller <- function(out_file, gdal_params) {
    # gdal is used if the image needs to be cropped or resampled
    if (any(c("-srcwin", "-tr") %in% names(gdal_params))) {
        download_fn <- .download_gdal(out_file, gdal_params)
    } else {
        download_fn <- .download_base(out_file)
    }
    return(download_fn)
}
#' @title Download function when using GDAL
#' @noRd
#' @param out_file    Path where file will be saved
#' @param gdal_params GDAL parameters
#' @returns  Appropriate GDAL download function
.download_gdal <- function(out_file, gdal_params) {
    # Ellipse is not used in gdal_translate. Defined to keep consistency.
    download_fn <- function(file, n_tries, ...) {
        # Download file
        while (n_tries > 0) {
            out <- .try(
                .gdal_translate(
                    file = out_file, base_file = file,
                    params = gdal_params, quiet = TRUE
                ), default = NULL
            )
            if (.has(out)) {
                return(out_file)
            }
            n_tries <- n_tries - 1
        }
        if (!.has(out)) {
            warning(paste("Error in downloading file", file))
        }
        # Return file name
        out_file
    }
    download_fn
}
#' @title Download function when not using GDAL
#' @noRd
#' @param out_file    Path where file will be saved
#' @returns  Appropriate non-GDAL download function
.download_base <- function(out_file) {
    donwload_fn <- function(file, n_tries, ...) {
        # Remove vsi driver path
        file <- .file_remove_vsi(file)
        # Add file scheme in local paths
        if (.file_is_local(file)) {
            file <- .file_path("file://", file, sep = "")
        }
        # Perform request
        out <- .retry_request(url = file, path = out_file, n_tries = n_tries)
        # Verify error
        if (.response_is_error(out)) {
            warning(paste("Error in downloading file", file))
        }
        # Return file name
        out_file
    }
    donwload_fn
}
#' @title Remove slash from sensor name
#' @noRd
#' @param x    Sensor name (e.g. "TM/OLI")
#' @returns    Sensor name without slashes
.download_remove_slash <- function(x) {
    gsub(pattern = "/", replacement = "", x = x, fixed = TRUE)
}
