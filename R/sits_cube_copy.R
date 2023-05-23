#' Copy the images of a cube to a local directory
#'
#' This function downloads the images of a cube in parallel.
#' A region of interest (\code{roi}) can be provided to crop
#' the images and a resolution (\code{res}) to resample the
#' bands.
#'
#' @param cube       A sits cube
#' @param roi        A Region of interest. See details below.
#' @param res        An integer value corresponds to the output
#'                   spatial resolution of the images. Default is NULL.
#' @param multicores Number of workers for parallel downloading.
#' @param output_dir Output directory where images will be saved.
#' @param progress   Show progress bar?
#'
#' @return a sits cube with updated metadata.
#'
#' @note
#'    The \code{roi} parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lon_min", "lat_min", "lon_max", "lat_max")
#'
#' @examples
#' if (sits_run_examples()) {
#'   # Creating a sits cube from BDC
#'   bdc_cube <- sits_cube(
#'       source = "BDC",
#'       collection = "CB4_64_16D_STK-1",
#'       tiles = c("022024", "022025"),
#'       bands = c("B15", "CLOUD"),
#'       start_date = "2018-01-01",
#'       end_date = "2018-01-12"
#'   )
#'
#'   # Downloading images to a temporary directory
#'   cube_local <- sits_cube_copy(
#'       cube = bdc_cube,
#'       output_dir = tempdir(),
#'       roi = c(lon_min = -42.28469009,
#'               lat_min = -14.95411527,
#'               lon_max = -41.74745556,
#'               lat_max = -14.65950650),
#'       multicores = 2
#'   )
#' }
#'
#' @export
sits_cube_copy <- function(cube,
                           roi = NULL,
                           res = NULL,
                           multicores = 2,
                           output_dir,
                           progress = TRUE) {

    # Pre-conditions
    .check_is_raster_cube(cube)
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
    }
    .check_res(res)
    if (inherits(output_dir, "character")) {
        output_dir <- path.expand(output_dir)
    }
    .check_output_dir(output_dir)
    .check_multicores(multicores)
    .check_progress(progress)

    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # Create assets as jobs
    cube_assets <- .cube_split_assets(cube)
    # Process each tile sequentially
    cube_assets <- .jobs_map_parallel_dfr(cube_assets, function(asset) {
        local_asset <- .download_asset(
            asset = asset,
            res = res,
            roi = roi,
            output_dir = output_dir,
            progress = progress
        )
        # Return local tile
        local_asset
    }, progress = progress)
    # Join output assets as a cube and return it
    .cube_merge_tiles(cube_assets)
}

.download_asset <- function(asset, res, roi, output_dir, progress) {
    # Get all paths and expand
    file <- .file_normalize(.tile_path(asset))
    # Create a list of user parameters as gdal format
    gdal_params <- .gdal_format_params(asset = asset, roi = roi, res = res)
    # Create output file
    derived_cube <- inherits(asset, "derived_cube")
    if (derived_cube)
        out_file <- paste0(output_dir, "/", basename(file))
    else
        out_file <- .file_path(
            .tile_satellite(asset), .remove_slash(.tile_sensor(asset)),
            .tile_name(asset), .tile_bands(asset),
            .tile_start_date(asset), output_dir = output_dir, ext = "tif"
        )
    # Resume feature
    if (.raster_is_valid(out_file, output_dir = output_dir)) {
        if (.check_messages()) {
            message("Recovery: file '", out_file, "' already exists.")
            message("(If you want to get a new version, please ",
                    "change 'output_dir' parameter or delete the existing file)")
        }
        asset <- .download_update_asset(
            asset = asset, roi = roi, res = res, out_file = out_file
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
        asset = asset, roi = roi, res = res, out_file = out_file
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

.gdal_format_params <- function(asset, roi, res) {
    gdal_params <- list()
    if (.has(res)) {
        gdal_params[["-tr"]] <- list(xres = res, yres = res)
    }
    if (.has(roi)) {
        gdal_params[["-srcwin"]] <- .gdal_as_srcwin(asset = asset, roi = roi)
    }
    gdal_params[c("-of", "-co")] <- list(
        "GTiff", .conf("gdal_presets", "image", "co")
    )
    band_conf <- .tile_band_conf(asset, .tile_bands(asset))
    gdal_params[["-a_nodata"]] <- .miss_value(band_conf)
    return(gdal_params)
}

.gdal_as_srcwin <- function(asset, roi) {
    block <- .raster_sub_image(tile = asset, roi = roi)
    list(xoff = block[["col"]] - 1,
         yoff = block[["row"]] - 1,
         xsize = block[["ncols"]],
         ysize = block[["nrows"]]
    )
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

.remove_slash <- function(x) {
    gsub(pattern = "/", replacement = "", x = x)
}
