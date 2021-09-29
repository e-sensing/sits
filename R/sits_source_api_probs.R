#' @keywords internal
#' @export
.source_cube.probs_cube <- function(source, ...,
                                    name,
                                    satellite,
                                    sensor,
                                    start_date,
                                    end_date,
                                    probs_labels,
                                    probs_files) {

    # set caller to show in errors
    .check_set_caller(".source_cube.probs_cube")

    # iterate through the input files
    tiles <- purrr::map(seq_along(probs_files), function(i) {

        # precondition - check if labels match files
        # read the information from the files using GDAL
        rg_obj <- terra::rast(probs_files[[i]])
        n_layers <- terra::nlyr(rg_obj)
        .check_num(
            x = n_layers,
            min = length(probs_labels),
            max = length(probs_labels),
            msg = paste("mismatch between labels and bands in file",
                        probs_files[[i]]))

        # get the file params
        params <- .raster_params_file(probs_files[[i]])

        # build the file information
        file_info <- tibble::tibble(
            band = "probs",
            start_date = as.Date(start_date),
            end_date = as.Date(end_date),
            res = params$xres,
            path = probs_files[[i]]
        )

        # go tile by tile
        tile <- .sits_cube_create(
            name        = name,
            source      = "PROBS",
            collections = "PROBS",
            satellite   = satellite,
            sensor      = sensor,
            tile        = NA,
            bands       = list("PROBS"),
            labels      = list(probs_labels),
            nrows       = params$nrows,
            ncols       = params$ncols,
            xmin        = params$xmin,
            xmax        = params$xmax,
            ymin        = params$ymin,
            ymax        = params$ymax,
            xres        = params$xres,
            yres        = params$yres,
            crs         = params$crs,
            file_info   = list(file_info)
        )
        return(tile)
    })
    probs_cube <- dplyr::bind_rows(tiles)

    class(probs_cube) <- c("probs_cube", "raster_cube", class(probs_cube))
    return(probs_cube)
}
