#' @title Adjust cube tile name
#' @keywords internal
#' @noRd
#' @name .download_asset
#' @param asset      A data cube
#' @param roi        Region of interest.
#'                   Either an sf_object, a shapefile,
#'                   or a bounding box vector with
#'                   named XY values ("xmin", "xmax", "ymin", "ymax") or
#'                   named lat/long values
#'                   ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param res        An integer value corresponds to the output
#'                   spatial resolution of the images. Default is NULL.
#' @param n_tries    Number of attempts to download the same image.
#'                   Default is 3.
#' @param multicores Number of cores for parallel downloading
#'                   (integer, min = 1, max = 2048).
#' @param output_dir Output directory where images will be saved.
#'                   (character vector of length 1).
#' @return data cube with downloaded tile
.download_asset <- function(asset, roi, res, n_tries, output_dir) {
    # Create GDAL Params
    gdal_params <- list()
    if (.has(res)) {
        gdal_params[["-tr"]] <- list(res, res)
    }
    # Fix sensor name
    asset[["sensor"]] <- gsub(
        pattern = "/",
        replacement = "",
        x = .tile_sensor(asset),
        fixed = TRUE
    )
    # Define output file name
    output_file <- .file_eo_name(
        tile = asset,
        band = .tile_bands(asset),
        date = .tile_start_date(asset),
        output_dir = output_dir
    )
    # Try to download
    while (n_tries > 0) {
        # Check if the output file already exists
        if (.raster_is_valid(output_file)) {
            local_asset <- .tile_from_file(
                file = output_file, base_tile = asset,
                band = .tile_bands(asset), update_bbox = TRUE,
                labels = .tile_labels(asset)
            )

            return(local_asset)
        }
        # Update token (for big tiffs and slow networks)
        asset <- .cube_token_generator(asset)
        # Crop and download
        local_asset <- .try(
            expr = .crop_asset(
                asset       = asset,
                roi         = roi,
                output_file = output_file,
                gdal_params = gdal_params
            ),
            .default = NULL
        )
        # Check if the downloaded file is valid
        if (.has(local_asset) && .raster_is_valid(output_file)) {
            return(local_asset)
        }
        # If file is not valid, try to download it again.
        n_tries <- n_tries - 1
        # Generate random seconds to wait before try again. This approach
        # is used to avoid flood the server.
        secs_to_retry <- .conf("download_sleep_time")
        secs_to_retry <- sample(x = seq_len(secs_to_retry), size = 1)
        Sys.sleep(secs_to_retry)
    }
    # Return local asset
    local_asset
}
