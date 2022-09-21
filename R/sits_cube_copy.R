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
    # Create features as jobs
    features <- .cube_feature_create(cube)
    # Process each tile sequentially
    features <- .jobs_map_sequential_dfr(features, function(feature) {
        local_tile <- .download_tile(
            feature = feature, res = res,
            roi = roi, output_dir = output_dir
        )
    })
    # Join output features as a cube and return it
    .cube_merge_features(features)
}

.download_tile <- function(feature, res, roi, output_dir, progress) {
    # Get all paths and expand
    paths <- path.expand(.fi_paths(.fi(feature)))
    # Create a list of user parameters
    gdal_params <- .get_gdal_params(feature = feature, roi = roi, res = res)
    # Start parallel downloading
    destfiles <- .jobs_map_parallel_chr(paths, function(path) {
        # Create dest file
        destfile <- file.path(output_dir, .file_base(path))
        # Resume feature
        if (.raster_is_valid(destfile)) {
            return(destfile)
        }
        # Download file
        destfile <- download_fn(
            path = path, destfile = destfile, gdal_params = gdal_params,
            output_dir = output_dir
        )
        # Return the destination file path
        destfile
    })

    update_bbox <- if (is.null(roi) && is.null(res)) FALSE else TRUE
    feature <- .tile_eo_from_files(
        files = destfiles, fid = .fi_fid(.fi(feature)),
        bands = .fi_bands(.fi(feature)), date = .tile_start_date(feature),
        base_tile = feature, update_bbox = update_bbox
    )
    return(feature)
}

.get_gdal_params <- function(feature, roi, res) {
    gdal_params <- list()
    if (!is.null(res)) {
        gdal_params[["tr"]] <- .get_gdal_res(res)
    }
    if (!is.null(roi)) {
        gdal_params[["srcwin"]] <- .get_gdal_roi(feature = feature, roi = roi)
    }
    gdal_params[c("of", "co")] <- list("GTiff", .config_gtiff_default_options())

    gdal_params
}

.get_gdal_roi <- function(feature, roi) {
    block <- .sits_raster_sub_image(tile = feature, roi = roi)
    c(xoff = block[["first_col"]] - 1,
      yoff = block[["first_row"]] - 1,
      xsize = block[["ncols"]],
      ysize = block[["nrows"]]
    )
}

.get_gdal_res <- function(res) {
    c(res, res)
}

.file_is_local_path <- function(path) {
    !grepl(pattern = "^[^:]+:", x = path)
}

.file_remove_vsi <- function(path) {
    gsub(pattern = "^(/vsicurl/|/vsis3/|/vsigs/)", replacement = "", x = path)
}

.gdal_download <- function(destfile, gdal_params) {
    download_fn <- function(path) {
        gdal_params[c("src_dataset", "dst_dataset")] <- list(path, destfile)
        do.call(
            what = gdalUtilities::gdal_translate,
            args = gdal_params
        )
    }
    download_fn
}

.base_download <- function(destfile) {
    donwload_fn <- function(path) {
        download.file(
            url = .file_remove_vsi(path),
            destfile = destfile,
            method = "wget",
            quiet = TRUE
        )
    }
    donwload_fn
}

.is_spatial_download <- function(gdal_params) {
    "srcwin" %in% names(gdal_params) || "res" %in% names(gdal_params)
}

download_controler <- function(destile, gdal_params) {
    if (.is_spatial_download(gdal_params)) {
        download_fn <- .gdal_download(destile, gdal_params)
    } else {
        download_fn <- .base_download(destile)
    }
    return(download_fn)
}

download_fn <- function(path, destfile, gdal_params, output_dir) {
    # Add file scheme in path
    if (.file_is_local_path(path)) {
        path <- .file_path("file://", path)
    }
    # Get a spatial or default download
    download_fun <- download_controler(destfile, gdal_params)
    # Download file
    .try(download_fun(path),  default = function(e) stop(e$message))
    # Return destination file
    destfile
}
