# ---- gdal API ----

.gdal_data_type <- c(
    INT1U = "Byte", INT2U = "UInt16", INT2S = "Int16",
    INT4U = "UInt32", INT4S = "Int32", FLT4S = "Float32",
    FLT8S = "Float64"
)
#' @title Get GDAL parameters
#' @noRd
#' @param params   Params used to describe GDAL file
#' @returns        Cleaned GDAL parameters
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
#' @title Format GDAL parameters
#' @noRd
#' @param asset  File to be accessed (with path)
#' @param sf_roi Region of interest (sf object)
#' @param res    Spatial resolution
#' @returns      Formatted GDAL parameters
.gdal_format_params <- function(asset, sf_roi, res) {
    gdal_params <- list()
    if (.has(res)) {
        gdal_params[["-tr"]] <- list(xres = res, yres = res)
    }
    if (.has(sf_roi)) {
        gdal_params[["-srcwin"]] <- .gdal_as_srcwin(
            asset = asset,
            sf_roi = sf_roi
        )
    }
    gdal_params[c("-of", "-co")] <- list(
        "GTiff", .conf("gdal_presets", "image", "co")
    )
    band_conf <- .tile_band_conf(asset, .tile_bands(asset))
    gdal_params[["-a_nodata"]] <- .miss_value(band_conf)
    return(gdal_params)
}
#' @title Format GDAL block parameters for data access
#' @noRd
#' @param asset  File to be accessed (with path)
#' @param sf_roi Region of interest (sf object)
#' @returns      Formatted GDAL block parameters for data access
.gdal_as_srcwin <- function(asset, sf_roi) {
    block <- .raster_sub_image(tile = asset, sf_roi = sf_roi)
    list(
        xoff = block[["col"]] - 1,
        yoff = block[["row"]] - 1,
        xsize = block[["ncols"]],
        ysize = block[["nrows"]]
    )
}
#' @title Run gdal_translate
#' @noRd
#' @param file        File to be created (with path)
#' @param base_file   File to be copied from (with path)
#' @param params       GDAL parameters
#' @param quiet       TRUE/FALSE
#' @returns           Called for side effects
.gdal_translate <- function(file, base_file, params, quiet) {
    sf::gdal_utils(
        util = "translate", source = base_file[[1]], destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
    return(invisible(file))
}
#' @title Run gdal_warp
#' @noRd
#' @param file        File to be created (with path)
#' @param base_files  Files to be copied from (with path)
#' @param param       GDAL parameters
#' @param quiet       TRUE/FALSE
#' @returns           Called for side effects
.gdal_warp <- function(file, base_files, params, quiet) {
    sf::gdal_utils(
        util = "warp", source = base_files, destination = file[[1]],
        options = .gdal_params(params), quiet = quiet
    )
    return(invisible(file))
}
#' @title Run gdal_warp for SAR GRD files
#' @noRd
#' @param raster_file  File to be copied from (with path)
#' @param size         Size of output file
#' @returns            Name of output file
.gdal_warp_grd <- function(raster_file, size) {
    temp_file <- tempfile(fileext = ".tif")
    .gdal_warp(
        file = temp_file,
        base_files = raster_file,
        params = list(
            "-ts" = list(size[["xsize"]], size[["ysize"]]),
            "-multi" = FALSE,
            "-q" = TRUE,
            "-overwrite" = FALSE
        ),
        quiet = TRUE)
    return(temp_file)
}
#' @title Run gdal_addo
#' @noRd
#' @param base_file   Base file to be processed
#' @returns           Called for side effects
.gdal_addo <- function(base_file) {
    conf_cog <- .conf("gdal_presets", "cog")
    suppressMessages(
        sf::gdal_addo(
            file = base_file,
            method = conf_cog[["method"]],
            overviews = conf_cog[["overviews"]],
            options = c(GDAL_NUM_THREADS = "2")
        )
    )
    return(invisible(file))
}
#' @title Run gdal_translate from a block to a file
#' @noRd
#' @param block        Block with
#' @param bbox         Bounding box for file
#' @param file         Files to be written to (with path)
#' @param nlayers      Number of layers in GDAL file
#' @param miss_value   Missing value
#' @param data_type.   GDAL data type
#' @returns            Name of file that was written to
.gdal_template_block <- function(block, bbox, file, nlayers, miss_value,
                                 data_type) {
    # Get first file
    file <- file[[1]]
    # Convert to gdal data type
    data_type <- .gdal_data_type[[data_type]]
    # Output file
    file <- .try(
        {
            .gdal_translate(
                file = file,
                # GDAL does not allow raster creation, to bypass this limitation
                # We base our raster creation by using a tiny template
                # (647 Bytes)
                base_file = system.file(
                    "extdata/raster/gdal/template.tif",
                    package = "sits"
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
        }
    )
    # Return file
    return(file)
}
#' @title Merge files into a single file
#' @noRd
#' @param file         Files to be written to (with path)
#' @param base_files   Files to be copied from (with path)
#' @param multicores   Number of cores to be used in parallel
#' @param roi          ROI to crop base_files
#' @returns            Name of file that was written to
.gdal_merge_into <- function(file, base_files, multicores, roi = NULL) {
    r_obj <- .raster_open_rast(file)
    # Merge src_files
    file <- .try(
        {
            if (.has(roi)) {
                # reproject ROI
                roi <- .roi_as_sf(roi, as_crs = .raster_crs(r_obj))
                # Write roi in a temporary file
                roi_file <- .roi_write(
                    roi = roi,
                    output_file = tempfile(fileext = ".shp"),
                    quiet = TRUE
                )
                .gdal_warp(
                    file = file,
                    base_files = base_files,
                    params = list(
                        "-wo" = paste0("NUM_THREADS=", multicores),
                        "-multi" = FALSE,
                        "-cutline" = roi_file,
                        "-q" = TRUE,
                        "-overwrite" = FALSE
                    ),
                    quiet = TRUE
                )
            } else {
                .gdal_warp(
                    file = file,
                    base_files = base_files,
                    params = list(
                        "-wo" = paste0("NUM_THREADS=", multicores),
                        "-multi" = FALSE,
                        "-q" = TRUE,
                        "-overwrite" = FALSE
                    ),
                    quiet = TRUE
                )
            }
        },
        .rollback = {
            unlink(file)
        },
        .finally = {
            # Delete auxiliary files
            unlink(paste0(file, ".aux.xml"))
            if (.has(roi)) unlink(roi_file)
        }
    )
    # Return file
    return(file)
}
#' @title Crop an image and save to file
#' @noRd
#' @param file         Input file (with path)
#' @param out_file     Output files (with path)
#' @param as_crs       Output CRS (if different from input)
#' @param miss_value   Missing value
#' @param data_type    GDAL data type
#' @param multicores   Number of cores to be used in parallel
#' @param overwrite    TRUE/FALSE
#' @returns            Called for side effects
.gdal_crop_image <- function(file,
                             out_file,
                             roi_file,
                             as_crs,
                             miss_value,
                             data_type,
                             multicores = 1,
                             overwrite = TRUE) {
    gdal_params <- list(
        "-ot" = .gdal_data_type[[data_type]],
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-multi" = FALSE,
        "-t_srs" = as_crs,
        "-cutline" = roi_file,
        "-dstnodata" = miss_value,
        "-overwrite" = overwrite
    )
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, quiet = TRUE
    )
    return(invisible(out_file))
}
#' @title Rescale image values and save to file
#' @noRd
#' @param file         Input file (with path)
#' @param out_file     Output files (with path)
#' @param src_min      Minimum value in source image
#' @param src_max      Maximum value in source image
#' @param dst_min      Minimum value in destination image
#' @param dst_max      Maximum value in destination image
#' @param miss_value   Missing value
#' @param data_type    GDAL data type
#' @returns            Called for side effects
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
    return(invisible(file))
}
#' @title Change the projection of an image and save to file
#' @noRd
#' @param file         Input file (with path)
#' @param out_file     Output files (with path)
#' @param crs          Input CRS
#' @param as_crs       Output CRS
#' @param miss_value   Missing value
#' @param data_type    GDAL data type
#' @param multicores   Number of cores to be used in parallel
#' @param overwrite    TRUE/FALSE
#' @returns            Output file
.gdal_reproject_image <- function(file, out_file, crs, as_crs, miss_value,
                                  data_type, multicores, overwrite = TRUE) {
    gdal_params <- list(
        "-ot" = .gdal_data_type[[data_type]],
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-multi" = FALSE,
        "-s_srs" = crs,
        "-t_srs" = as_crs,
        "-srcnodata" = miss_value,
        "-overwrite" = overwrite
    )
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, quiet = TRUE
    )
    return(invisible(out_file))
}
