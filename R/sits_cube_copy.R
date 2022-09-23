#' Copy the images of a cube to a local directory
#'
#' This function downloads the images of a cube in parallel.
#' A region of interest (\code{roi}) can be provided to crop
#' the images and a resolution (\code{res}) to resample the
#' bands.
#'
#' @param cube       A sits cube
#' @param roi        A Region of interest. See details bellow.
#' @param res        An integer value corresponds to the output
#'                   spatial resolution of the images.
#'                   Default is NULL.
#' @param output_dir Output directory where images will be saved.
#' @param multicores Number of workers for parallel downloading.
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
                           output_dir = getwd(),
                           multicores = 2,
                           progress = TRUE) {
    # check documentation mode
    progress <- .check_documentation(progress)

    # precondition - cube
    .check_is_sits_cube(cube)
    # precondition - res
    .check_res(res)
    # precondition - output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - progress
    .check_lgl_type(progress)
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)
    # Create assets as jobs
    assets <- .cube_create_assets(cube)
    # Process each tile sequentially
    assets <- .jobs_map_parallel_dfr(assets, function(asset) {
        local_tile <- .download_asset(
            asset = asset, res = res,
            roi = roi, output_dir = output_dir,
            progress = progress
        )
    }, progress = progress)
    # Join output assets as a cube and return it
    .cube_merge_assets(assets)
}

.download_asset <- function(asset, res, roi, output_dir, progress) {
    # Get all paths and expand
    file <- path.expand(.fi_paths(.fi(asset)))
    # Create a list of user parameters as gdal format
    gdal_params <- .gdal_format_params(asset = asset, roi = roi, res = res)
    # Create output file
    out_file <- file.path(output_dir, .file_base(file))
    # Resume feature
    if (.raster_is_valid(out_file)) {
        return(out_file)
    }
    # Get a gdal or default download
    download_fn <- .download_controller(out_file, gdal_params)
    # Download file
    out_file <- download_fn(file)
    # Update asset metadata
    update_bbox <- if (is.null(roi) && is.null(res)) FALSE else TRUE
    asset <- .tile_eo_from_files(
        files = out_file, fid = .fi_fid(.fi(asset)),
        bands = .fi_bands(.fi(asset)), date = .tile_start_date(asset),
        base_tile = asset, update_bbox = update_bbox
    )
    asset
}

.gdal_format_params <- function(asset, roi, res) {
    gdal_params <- list()
    if (!is.null(res)) {
        gdal_params[["tr"]] <- c(res, res)
    }
    if (!is.null(roi)) {
        gdal_params[["srcwin"]] <- .gdal_as_srcwin(asset = asset, roi = roi)
    }
    gdal_params[c("of", "co")] <- list("GTiff", .config_gtiff_default_options())

    gdal_params
}

.gdal_as_srcwin <- function(asset, roi) {
    block <- .sits_raster_sub_image(tile = asset, roi = roi)
    c(xoff = block[["col"]] - 1,
      yoff = block[["row"]] - 1,
      xsize = block[["ncols"]],
      ysize = block[["nrows"]]
    )
}

.download_controller <- function(out_file, gdal_params) {
    # gdal is used if the image needs to be cropped or resampled
    if (any(c("srcwin", "tr") %in% names(gdal_params))) {
        download_fn <- .download_gdal(out_file, gdal_params)
    } else {
        download_fn <- .download_base(out_file)
    }
    return(download_fn)
}

.download_gdal <- function(out_file, gdal_params) {
    download_fn <- function(file) {
        gdal_params[c("src_dataset", "dst_dataset")] <- list(file, out_file)
        do.call(
            what = gdalUtilities::gdal_translate,
            args = gdal_params
        )
        out_file
    }
    download_fn
}

.download_base <- function(out_file) {
    donwload_fn <- function(file) {
        # Add file scheme in path
        if (.file_is_local(file)) {
            file <- .file_path("file://", file, sep = "")
        }
        download.file(
            url = .file_remove_vsi(file),
            destfile = out_file, quiet = TRUE
        )
        out_file
    }
    donwload_fn
}
