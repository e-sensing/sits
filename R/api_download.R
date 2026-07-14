#' @title Download an asset of a data cube
#' @keywords internal
#' @noRd
#' @name .download_asset
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param asset A data cube asset (one tile)
#' @param roi Region of interest. Default is NULL.
#' @param res Spatial resolution. Default is NULL.
#' @param n_tries Number of attempts to download the same image. Default is 3.
#' @param output_dir Output directory where images will be saved.
#' @return data cube with downloaded tile
.download_asset <- function(asset, roi, res, n_tries, output_dir) {
    UseMethod(".download_asset", asset)
}
#' @export
.download_asset.raster_cube <- function(asset, roi, res, n_tries,
                                        output_dir) {
    # Define filename
    output_file <- .file_eo_name(
        tile = asset,
        band = .tile_bands(asset),
        date = .tile_start_date(asset),
        output_dir = output_dir
    )
    # Download
    .download_asset_operation(
        asset = asset,
        roi = roi,
        res = res,
        n_tries = n_tries,
        output_file = output_file,
        output_dir = output_dir,
        resampling = .conf("download_resampling", "raster_cube")
    )
}
#' @export
.download_asset.derived_cube <- function(asset, roi, res, n_tries,
                                         output_dir) {
    # Extract version
    fields <- .file_sans_ext(.file_base(.tile_path(asset)))
    fields <- strsplit(fields, split = "_", fixed = TRUE)
    fields <- fields[[1L]]
    version <- fields[[length(fields)]]
    # Define filename
    output_file <- .file_derived_name(
        tile = asset,
        band = .tile_bands(asset),
        version = version,
        output_dir = output_dir
    )
    # Download
    .download_asset_operation(
        asset = asset,
        roi = roi,
        res = res,
        n_tries = n_tries,
        output_file = output_file,
        output_dir = output_dir,
        resampling = .conf("download_resampling", "raster_cube")
    )
}
#' @export
.download_asset.class_cube <- function(asset, roi, res, n_tries,
                                       output_dir) {
    # Extract version
    fields <- .file_sans_ext(.file_base(.tile_path(asset)))
    fields <- strsplit(fields, split = "_", fixed = TRUE)
    fields <- fields[[1L]]
    version <- fields[[length(fields)]]
    # Define filename
    output_file <- .file_derived_name(
        tile = asset,
        band = .tile_bands(asset),
        version = version,
        output_dir = output_dir
    )
    # Download
    .download_asset_operation(
        asset = asset,
        roi = roi,
        res = res,
        n_tries = n_tries,
        output_file = output_file,
        output_dir = output_dir,
        resampling = .conf("download_resampling", "class_cube")
    )
}
#' @title Download an asset applying naming and resampling rules
#' @keywords internal
#' @noRd
#' @param asset A data cube asset (one tile)
#' @param roi Region of interest
#' @param res Output spatial resolution (or NULL)
#' @param n_tries Number of download attempts
#' @param output_file Output file path
#' @param output_dir Output directory
#' @param resampling GDAL resampling method used when `res` is set
#' @return Data cube with downloaded tile
.download_asset_operation <- function(asset, roi, res, n_tries,
                                      output_file, output_dir, resampling) {
    # Create GDAL params. Resampling only applies when the resolution changes.
    # Otherwise the asset is copied/cropped without altering pixel values.
    gdal_params <- list()
    if (.has(res)) {
        gdal_params[["-tr"]] <- list(res, res)
        gdal_params[["-r"]] <- resampling
    }
    # Try to download
    while (n_tries > 0L) {
        # Check if the output file already exists
        if (all(.raster_is_valid(output_file, output_dir = output_dir))) {
            local_asset <- .tile_from_file(
                file = output_file, base_tile = asset,
                band = .tile_bands(asset), update_bbox = TRUE,
                labels = .tile_labels(asset)
            )

            return(local_asset)
        }
        # Update token (for big tiffs and slow networks)
        asset <- .cube_token_generator(asset)
        # Check if cube requires gdal auth
        use_gdal_auth <- .cube_uses_gdal_auth(asset)
        # Crop and download
        local_asset <- .try(
            expr = .crop_asset(
                asset       = asset,
                roi         = roi,
                output_file = output_file,
                gdal_params = gdal_params,
                gdal_auth   = use_gdal_auth
            ),
            .default = NULL
        )
        # Check if the downloaded file is valid
        if (.has(local_asset) &&
            all(.raster_is_valid(output_file, output_dir = output_dir))) {
            return(local_asset)
        }
        # If file is not valid, try to download it again.
        n_tries <- n_tries - 1L
        # Generate random seconds to wait before try again. This approach
        # is used to avoid flood the server.
        secs_to_retry <- .conf("download_sleep_time")
        secs_to_retry <- sample.int(secs_to_retry, size = 1L)
        Sys.sleep(secs_to_retry)
        # Flush token
        asset <- .cube_token_flush(asset)
    }
    # Return local asset
    local_asset
}
