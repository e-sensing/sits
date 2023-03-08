
# ---- gdal API ----

.gdal_data_type <- c(
    "INT1U" = "Byte", "INT2U" = "UInt16", "INT2S" = "Int16",
    "INT4U" = "UInt32", "INT4S" = "Int32", "FLT4S" = "Float32",
    "FLT8S" = "Float64"
)

.gdal_params <- function(params) {
    # Check if parameters are named
    if (!all(.has_name(params))) {
        stop("parameters should be named")
    }
    unlist(mapply(function(par, val) {
        if (is.null(val)) {
            NULL
        } else if (is.logical(val)) {
            if (val) par else NULL
        } else if (is.list(val)) {
            c(par, unlist(val))
        } else {
            .as_chr(rbind(par, val))
        }
    }, names(params), unname(params), USE.NAMES = FALSE))
}

.gdal_translate <- function(file, base_file, params, quiet) {
    sf::gdal_utils(
        util = "translate", source = base_file[[1]], destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}

.gdal_warp <- function(file, base_files, params, quiet) {
    sf::gdal_utils(
        util = "warp", source = base_files, destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
}

.gdal_buildvrt <- function(file, base_files, quiet) {
    sf::gdal_utils(
        util = "buildvrt", source = base_files,
        destination = file, quiet = quiet
    )
}

.gdal_addo <- function(base_file) {
    conf_cog <- .conf("gdal_presets", "cog")
    suppressMessages(
        sf::gdal_addo(
            file = base_file,
            method = conf_cog[["method"]],
            overviews = conf_cog[["overviews"]]
        )
    )
}

.gdal_template_from_file <- function(base_file, file, nlayers, miss_value,
                                     data_type) {
    # Convert to gdal data type
    data_type <- .gdal_data_type[[data_type]]
    # Output file
    file <- .try({
        .gdal_translate(
            file = file,
            base_file = base_file,
            params = list(
                "-ot" = data_type,
                "-of" = .conf("gdal_presets", "image", "of"),
                "-b" = rep(1, nlayers),
                "-scale" = list(0, 1, miss_value, miss_value),
                "-a_nodata" = miss_value,
                "-co" = .conf("gdal_presets", "image", "co")
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}

.gdal_template_block <- function(block, bbox, file, nlayers, miss_value,
                                 data_type) {
    # Get first file
    file <- file[[1]]
    # Convert to gdal data type
    data_type <- .gdal_data_type[[data_type]]
    # Output file
    file <- .try({
        .gdal_translate(
            file = file,
            # GDAL does not allow raster creation, to bypass this limitation
            # Let's base our raster creation by using a tiny template
            # (647 Bytes)
            base_file = system.file(
                "extdata/raster/gdal/template.tif", package = "sits"
            ),
            params = list(
                "-ot" = data_type,
                "-of" = .conf("gdal_presets", "block", "of"),
                "-b" = rep(1, nlayers),
                "-outsize" = list(.ncols(block), .nrows(block)),
                "-scale" = list(0, 1, miss_value, miss_value),
                "-a_srs" = .crs(bbox),
                "-a_ullr" = list(
                    .xmin(bbox), .ymax(bbox), .xmax(bbox), .ymin(bbox)
                ),
                "-a_nodata" = miss_value,
                "-co" = .conf("gdal_presets", "block", "co")
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}

.gdal_merge_into <- function(file, base_files, multicores) {
    # Merge src_files
    file <- .try({
        .gdal_warp(
            file = file,
            base_files = base_files,
            params = list(
                "-wo" = paste0("NUM_THREADS=", multicores),
                "-multi" = TRUE,
                "-q" = TRUE,
                "-overwrite" = FALSE
            ),
            quiet = TRUE
        )
    },
    .rollback = {
        unlink(file)
    },
    .finally = {
        # Delete auxiliary files
        unlink(paste0(file, ".aux.xml"))
    })
    # Return file
    file
}

.gdal_crop_image <- function(file,
                             out_file,
                             roi_file,
                             as_crs = NULL,
                             miss_value = NULL,
                             multicores = 1) {
    gdal_params <- list(
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-multi" = TRUE,
        "-t_srs" = as_crs,
        "-cutline" = roi_file,
        "-dstnodata" = miss_value
    )
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, quiet = TRUE
    )
    out_file
}

.gdal_scale <- function(file,
                        out_file,
                        src_min,
                        src_max,
                        dst_min,
                        dst_max,
                        miss_value,
                        data_type) {
    .gdal_translate(
        file = out_file,
        base_file = file,
        params = list(
            "-ot" = .gdal_data_type[[data_type]],
            "-of" = .conf("gdal_presets", "image", "of"),
            "-scale" = list(src_min, src_max, dst_min, dst_max),
            "-a_nodata" = miss_value,
            "-co" = .conf("gdal_presets", "image", "co")
        ),
        quiet = TRUE
    )
}

.gdal_reproject_image <- function(file, out_file, crs, as_crs, miss_value,
                                  multicores) {
    gdal_params <- list(
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-multi" = TRUE,
        "-s_srs" = crs,
        "-t_srs" = as_crs,
        "-srcnodata" = miss_value
    )
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, quiet = TRUE
    )
    out_file
}
