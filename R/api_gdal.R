# ---- gdal API ----

.gdal_data_type <- c(
    INT1U = "Byte", INT2U = "UInt16", INT2S = "Int16",
    INT4U = "UInt32", INT4S = "Int32", FLT4S = "Float32",
    FLT8S = "Float64"
)
#' @title Get GDAL parameters
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param params   Params used to describe GDAL file
#' @returns        Cleaned GDAL parameters
.gdal_params <- function(params) {
    .check_set_caller(".gdal_params")
    # Check if parameters are named
    .check_that(all(.has_name(params)))
    # gdalutils default value, sf gives error otherwise
    if (!.has(params)) {
        return(character(0))
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
#' @title Run gdal_translate
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param file           File to be created (with path)
#' @param base_file      File to be copied from (with path)
#' @param params         GDAL parameters
#' @param conf_opts      GDAL global configuration options
#' @param quiet          TRUE/FALSE
#' @returns              Called for side effects
.gdal_translate <- function(file, base_file, params,
                            conf_opts = character(0L), quiet) {
    sf::gdal_utils(
        util = "translate", source = base_file[[1L]], destination = file[[1L]],
        options = .gdal_params(params), config_options = conf_opts,
        quiet = quiet
    )
    invisible(file)
}
#'
#' @title Run gdal_warp
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param file        File to be created (with path)
#' @param base_files  Files to be copied from (with path)
#' @param param       GDAL parameters
#' @param conf_opts   GDAL global configuration options
#' @param quiet       TRUE/FALSE
#' @returns           Called for side effects
.gdal_warp <- function(file, base_files, params,
                       quiet, conf_opts = character(0L)) {
    sf::gdal_utils(
        util = "warp", source = base_files, destination = file[[1L]],
        options = .gdal_params(params), config_options = conf_opts,
        quiet = quiet
    )
    return(invisible(file))
}
#' @title Run gdal_warp_file
#' @noRd
#' @param raster_file  File to be copied from (with path)
#' @param sizes        Sizes of output file
#' @param t_srs        Target spatial reference system
#' @returns            Name of output file
.gdal_warp_file <- function(raster_file, sizes, t_srs = NULL) {
    # create a temporary file
    temp_file <- tempfile(fileext = ".tif")
    # basic parameters
    params <- list(
        "-ts" = list(sizes[["xsize"]], sizes[["ysize"]]),
        "-multi" = FALSE,
        "-q" = TRUE,
        "-overwrite" = FALSE
    )
    # additional param for target SRS
    if (.has(t_srs)) {
        params <- append(params, c("t_srs" = t_srs))
    }
    # warp the data
    .gdal_warp(
        file = temp_file,
        base_files = raster_file,
        params = params,
        quiet = TRUE
    )
    return(temp_file)
}
#' @title Run gdal_addo
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param base_file   Base file to be processed
#' @returns           Called for side effects
.gdal_addo <- function(base_file) {
    conf_cog <- .conf("gdal_presets", "cog")
    sf::gdal_addo(
        file = base_file,
        method = conf_cog[["method"]],
        overviews = conf_cog[["overviews"]],
        options = c(GDAL_NUM_THREADS = "2")
    )
    invisible(file)
}
#' @title Run gdal_translate from a block to a file
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
    file <- file[[1L]]
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
                    "-of" = .conf("gdal_presets", "image", "of"),
                    "-b" = rep(1L, nlayers),
                    "-outsize" = list(.ncols(block), .nrows(block)),
                    "-scale" = list(0.0, 1.0, miss_value, miss_value),
                    "-a_srs" = .crs(bbox),
                    "-a_ullr" = list(
                        .xmin(bbox), .ymax(bbox), .xmax(bbox), .ymin(bbox)
                    ),
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
        }
    )
    # Return file
    file
}
#' @title Merge files into a single file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param file         Files to be written to (with path)
#' @param base_files   Files to be copied from (with path)
#' @param multicores   Number of cores to be used in parallel
#' @param roi          ROI to crop base_files
#' @returns            Name of file that was written to
.gdal_merge_into <- function(file, base_files, multicores, roi = NULL) {
    rast <- .raster_open_rast(file)
    # Merge src_files
    file <- .try(
        {
            if (.has(roi)) {
                # reproject ROI
                roi <- .roi_as_sf(roi, as_crs = .raster_crs(rast))
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
            if (.has(roi)) .roi_delete(roi_file)
        }
    )
    # Return file
    return(file)
}

#' @title Crop an image and save to file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @param file         Input file (with path)
#' @param out_file     Output files (with path)
#' @param roi_file     File containing ROI in a GDAL readable format
#' @param as_crs       Output CRS (if different from input)
#' @param miss_value   Missing value
#' @param data_type    GDAL data type
#' @param multicores   Number of cores to be used in parallel
#' @param overwrite    TRUE/FALSE
#' @param ...          Additional parameters
#' @returns            Called for side effects
.gdal_crop_image <- function(file,
                             out_file,
                             roi_file,
                             as_crs,
                             miss_value,
                             data_type,
                             multicores = 1L,
                             overwrite = TRUE, ...) {
    gdal_params <- list(
        "-ot" = .gdal_data_type[[data_type]],
        "-of" = .conf("gdal_presets", "image", "of"),
        "-co" = .conf("gdal_presets", "image", "co"),
        "-wo" = paste0("NUM_THREADS=", multicores),
        "-multi" = FALSE,
        "-t_srs" = as_crs,
        "-cutline" = roi_file,
        "-crop_to_cutline" = TRUE,
        "-dstnodata" = miss_value,
        "-overwrite" = overwrite
    )
    if (.has(list(...))) {
        gdal_params <- utils::modifyList(gdal_params, as.list(...))
    }
    .gdal_warp(
        file = out_file, base_files = file,
        params = gdal_params, conf_opts = unlist(.conf("gdal_read_options")),
        quiet = TRUE
    )
    return(invisible(out_file))
}
#' @title Rescale image values and save to file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
    invisible(file)
}
#' @title Change the projection of an image and save to file
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
#' @title Get GDAL Version
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @noRd
#' @returns  GDAL Version
.gdal_version <- function() {
    sf_versions <- sf::sf_extSoftVersion()
    sf_versions[["GDAL"]]
}
