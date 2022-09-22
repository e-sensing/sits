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
        local_tile <- .download_feature(
            feature = feature, res = res,
            roi = roi, output_dir = output_dir,
            progress = progress
        )
    })
    # Join output features as a cube and return it
    .cube_merge_features(features)
}

.download_feature <- function(feature, res, roi, output_dir, progress) {
    # Get all paths and expand
    files <- path.expand(.fi_paths(.fi(feature)))
    # Create a list of user parameters as gdal format
    gdal_params <- .gdal_format_params(feature = feature, roi = roi, res = res)
    # Start parallel downloading
    destfiles <- .jobs_map_parallel_chr(files, function(file) {
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
        # Return the destination file path
        out_file
    })

    update_bbox <- if (is.null(roi) && is.null(res)) FALSE else TRUE
    feature <- .tile_eo_from_files(
        files = destfiles, fid = .fi_fid(.fi(feature)),
        bands = .fi_bands(.fi(feature)), date = .tile_start_date(feature),
        base_tile = feature, update_bbox = update_bbox
    )
    return(feature)
}

.gdal_format_params <- function(feature, roi, res) {
    gdal_params <- list()
    if (!is.null(res)) {
        gdal_params[["tr"]] <- c(res, res)
    }
    if (!is.null(roi)) {
        gdal_params[["srcwin"]] <- .gdal_as_srcwin(feature = feature, roi = roi)
    }
    gdal_params[c("of", "co")] <- list("GTiff", .config_gtiff_default_options())

    gdal_params
}

.gdal_as_srcwin <- function(feature, roi) {
    block <- .sits_raster_sub_image(tile = feature, roi = roi)
    c(xoff = block[["col"]] - 1,
      yoff = block[["row"]] - 1,
      xsize = block[["ncols"]],
      ysize = block[["nrows"]]
    )
}

.download_controller <- function(destfile, gdal_params) {
    # gdal is used if the image needs to be cropped or resampled
    if (any(c("srcwin", "tr") %in% names(gdal_params))) {
        download_fn <- .download_gdal(destfile, gdal_params)
    } else {
        download_fn <- .download_base(destfile)
    }
    return(download_fn)
}

.download_gdal <- function(destfile, gdal_params) {
    download_fn <- function(file) {
        gdal_params[c("src_dataset", "dst_dataset")] <- list(file, destfile)
        do.call(
            what = gdalUtilities::gdal_translate,
            args = gdal_params
        )
        destfile
    }
    download_fn
}

.download_base <- function(destfile) {
    donwload_fn <- function(file) {
        # Add file scheme in path
        if (.file_is_local(file)) {
            file <- .file_path("file://", file, sep = "")
        }
        download.file(
            url = .file_remove_vsi(file),
            destfile = destfile, quiet = TRUE
        )
        destfile
    }
    donwload_fn
}
