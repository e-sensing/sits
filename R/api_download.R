.download_asset <- function(asset, res, sf_roi, output_dir, progress) {
    # Get all paths and expand
    file <- .file_normalize(.tile_path(asset))
    # Create a list of user parameters as gdal format
    gdal_params <- .gdal_format_params(asset = asset,
                                       sf_roi = sf_roi,
                                       res = res)
    # Create output file
    derived_cube <- inherits(asset, "derived_cube")
    if (derived_cube)
        out_file <- paste0(output_dir, "/", basename(file))
    else
        out_file <- .file_path(
            .tile_satellite(asset),
            .download_remove_slash(.tile_sensor(asset)),
            .tile_name(asset),
            .tile_bands(asset),
            .tile_start_date(asset),
            output_dir = output_dir,
            ext = "tif"
        )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        if (.check_messages()) {
            message("Recovery: file '", out_file, "' already exists.")
            message("(If you want to get a new version, please ",
                    "change 'output_dir' parameter
                    or delete the existing file)"
            )
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
    suppressWarnings(download_fn(file))
    # Update asset metadata
    asset <- .download_update_asset(
        asset = asset, roi = sf_roi, res = res, out_file = out_file
    )
    # Return updated asset
    asset
}

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

.download_controller <- function(out_file, gdal_params) {
    # gdal is used if the image needs to be cropped or resampled
    if (any(c("-srcwin", "-tr") %in% names(gdal_params))) {
        download_fn <- .download_gdal(out_file, gdal_params)
    } else {
        download_fn <- .download_base(out_file)
    }
    return(download_fn)
}

.download_gdal <- function(out_file, gdal_params) {
    download_fn <- function(file) {
        .gdal_translate(
            file = out_file, base_file = file,
            params = gdal_params, quiet = TRUE
        )
        out_file
    }
    download_fn
}

.download_base <- function(out_file) {
    donwload_fn <- function(file) {
        # Remove vsi driver path
        file <- .file_remove_vsi(file)
        # Add file scheme in local paths
        if (.file_is_local(file)) {
            file <- .file_path("file://", file, sep = "")
        }
        httr::GET(
            url = file, httr::write_disk(path = out_file, overwrite = TRUE)
        )
        # Return file name
        out_file
    }
    donwload_fn
}

.download_remove_slash <- function(x) {
    gsub(pattern = "/", replacement = "", x = x)
}
