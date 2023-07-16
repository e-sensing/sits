#' @title Supported vector packages
#' @keywords internal
#' @noRd
#' @return   Names of vector packages supported by sits
.vector_supported_packages <- function() {
    return("sf")
}

#' @title Check for vector package availability
#' @name .vector_check_package
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @return name of the package.
.vector_check_package <- function() {
    pkg_class <- .conf_vector_pkg()
    class(pkg_class) <- pkg_class

    UseMethod(".vector_check_package", pkg_class)
}

.vector_open_vec <- function(file, layer = NULL) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_open_vec", pkg_class)
}

#' @title Vector package internal get values function
#' @name .vector_get_values
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param v_obj   vector package object
#' @param ...     additional parameters to be passed to vector package
#'
#' @return Numeric matrix associated to vector object
.vector_get_values <- function(v_obj, ...) {
    # check package
    pkg_class <- .vector_check_package()

    # call function
    UseMethod(".vector_get_values", pkg_class)
}

#' @title vector package internal set values function
#' @name .vector_set_values
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param v_obj   vector package object
#' @param values  Numeric matrix to copy to vector object
#' @param ...     additional parameters to be passed to vector package
#'
#' @return        vector object
.vector_set_values <- function(v_obj, values, ...) {
    # check package
    pkg_class <- .vector_check_package()

    UseMethod(".vector_set_values", pkg_class)
}

#' @title Raster package internal write raster file function
#' @name .raster_write_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj         raster package object to be written
#' @param file          file path to save raster file
#' @param format        GDAL file format string (e.g. SHP)
#' @param layer         ...
#' @param overwrite     logical indicating if file can be overwritten
#' @param ...           additional parameters to be passed to raster package
#'
#' @return              No value, called for side effects.
.vector_write_vec <- function(r_obj,
                              file,
                              format,
                              layer,
                              overwrite, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_write_rast", pkg_class)
}

#' @title Raster package internal create raster object function
#' @name .vector_new_vec
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param ...           additional parameters to be passed to raster package
#'
#' @return               A vector object.
.vector_new_vec <- function( ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_new_rast", pkg_class)
}

#' @title Raster package internal object properties
#' @name .vector_nrows
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object
#' @param ...      additional parameters to be passed to raster package
#'
#' @return Raster object spatial properties
.vector_nrows <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_nrows", pkg_class)
}

#' @name .raster_ncols
#' @keywords internal
#' @noRd
.raster_ncols <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_ncols", pkg_class)
}

#' @name .raster_nlayers
#' @keywords internal
#' @noRd
.raster_nlayers <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_nlayers", pkg_class)
}

#' @name .raster_xmax
#' @keywords internal
#' @noRd
.raster_xmax <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_xmax", pkg_class)
}

#' @name .raster_xmin
#' @keywords internal
#' @noRd
.raster_xmin <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_xmin", pkg_class)
}

#' @name .raster_ymax
#' @keywords internal
#' @noRd
.raster_ymax <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_ymax", pkg_class)
}

#' @name .raster_ymin
#' @keywords internal
#' @noRd
.raster_ymin <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_ymin", pkg_class)
}

#' @name .raster_xres
#' @keywords internal
#' @noRd
.raster_xres <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_xres", pkg_class)
}

#' @name .raster_yres
#' @keywords internal
#' @noRd
.raster_yres <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_yres", pkg_class)
}

#' @name .raster_crs
#' @keywords internal
#' @noRd
.raster_crs <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_crs", pkg_class)
}

#' @name .raster_bbox
#' @keywords internal
#' @noRd
.raster_bbox <- function(r_obj, ...,
                         block = NULL) {
    if (is.null(block)) {
        # return a named bbox
        bbox <- c(
            xmin = .raster_xmin(r_obj),
            ymin = .raster_ymin(r_obj),
            xmax = .raster_xmax(r_obj),
            ymax = .raster_ymax(r_obj)
        )
    } else {
        r_crop <- .raster_crop_metadata(
            .raster_rast(r_obj = r_obj),
            block = block
        )
        bbox <- .raster_bbox(r_crop)
    }

    return(bbox)
}

#' @name .raster_res
#' @keywords internal
#' @noRd
.raster_res <- function(r_obj, ...) {
    # return a named resolution
    res <- list(
        xres = .raster_xres(r_obj),
        yres = .raster_yres(r_obj)
    )

    return(res)
}

#' @name .raster_size
#' @keywords internal
#' @noRd
.raster_size <- function(r_obj, ...) {
    # return a named size
    size <- list(
        nrows = .raster_nrows(r_obj),
        ncols = .raster_ncols(r_obj)
    )

    return(size)
}

#' @title Raster package internal frequency values function
#' @name .raster_freq
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object to count values
#' @param ...      additional parameters to be passed to raster package
#'
#' @return matrix with layer, value, and count columns
.raster_freq <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_freq", pkg_class)
}

#' @title Raster package internal frequency values function
#' @name .raster_colrow
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj  raster package object
#' @param x,y    coordinates (either x or y) in raster projection
#'
#' @return integer with column or row
.raster_col <- function(r_obj, x) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_col", pkg_class)
}


#' @name .raster_row
#' @keywords internal
#' @noRd
.raster_row <- function(r_obj, y) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_row", pkg_class)
}

#' @title Determine the file params to write in the metadata
#' @name .raster_params_file
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its spatial parameters
#' @param file     A path to a raster file
#'
#' @return A tibble with the raster spatial parameters
.raster_params_file <- function(file) {
    # set caller to show in errors
    .check_set_caller(".raster_params_file")

    # preconditions
    .check_length(
        x = file,
        min = 1,
        msg = "no file was informed"
    )

    # use first file
    file <- file[[1]]

    # open file
    r_obj <- .raster_open_rast(file = file)

    params <- tibble::tibble(
        nrows = .raster_nrows(r_obj = r_obj),
        ncols = .raster_ncols(r_obj = r_obj),
        xmin  = .raster_xmin(r_obj = r_obj),
        xmax  = .raster_xmax(r_obj = r_obj),
        ymin  = .raster_ymin(r_obj = r_obj),
        ymax  = .raster_ymax(r_obj = r_obj),
        xres  = .raster_xres(r_obj = r_obj),
        yres  = .raster_yres(r_obj = r_obj),
        crs   = .raster_crs(r_obj = r_obj)
    )

    return(params)
}

.raster_template <- function(base_file, out_file, nlayers, data_type,
                             missing_value) {
    # Create an empty image template
    gdalUtilities::gdal_translate(
        src_dataset = .file_normalize(base_file),
        dst_dataset = .file_normalize(out_file),
        ot = .raster_gdal_datatype(data_type),
        of = "GTiff",
        b = rep(1, nlayers),
        scale = c(0, 1, missing_value, missing_value),
        a_nodata = missing_value,
        co = .conf("gdal_creation_options"),
        q = TRUE
    )
    # Delete auxiliary files
    on.exit(unlink(paste0(out_file, ".aux.xml")), add = TRUE)
    return(out_file)
}

#' @title Merge all input files into one raster file
#' @name .raster_merge
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param out_files     Output raster files path.
#' @param base_file     Raster file path to be used as template. If \code{NULL},
#'   all block_files will be merged without a template.
#' @param block_files   Input block files paths.
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param missing_value Missing value of out_files.
#' @param multicores    Number of cores to process merging
#'
#' @return No return value, called for side effects.
#'
.raster_merge_blocks <- function(out_files,
                                 base_file,
                                 block_files,
                                 data_type,
                                 missing_value,
                                 multicores = 2) {
    # Check consistency between block_files and out_files
    if (is.list(block_files)) {
        if (any(lengths(block_files) != length(out_files))) {
            stop("number of 'block_files' does not match 'out_file' length")
        }
    } else {
        if (length(out_files) != 1) {
            stop("invalid 'out_files' length")
        }
        block_files <- as.list(block_files)
    }

    # for each file merge blocks
    for (i in seq_along(out_files)) {
        # Expand paths for out_file
        out_file <- .file_normalize(out_files[[i]])
        # Check if out_file not exists
        .check_that(
            x = !file.exists(out_file),
            local_msg = paste0("file '", out_file, "' already exists"),
            msg = "invalid 'out_file' parameter"
        )
        # Get file paths
        merge_files <- purrr::map_chr(block_files, `[[`, i)
        # Expand paths for block_files
        merge_files <- .file_normalize(merge_files)
        # check if block_files length is at least one
        .check_file(
            x = merge_files,
            extensions = "tif",
            msg = "invalid input block files to merge"
        )
        # Get number of layers
        nlayers <- .raster_nlayers(.raster_open_rast(merge_files[[1]]))
        if (.has(base_file)) {
            # Create raster template
            .raster_template(
                base_file = base_file, out_file = out_file, nlayers = nlayers,
                data_type = data_type, missing_value = missing_value
            )
            # Merge into template
            .try(
                {
                    # merge using gdal warp
                    suppressWarnings(
                        gdalUtilities::gdalwarp(
                            srcfile = merge_files,
                            dstfile = out_file,
                            wo = paste0("NUM_THREADS=", multicores),
                            multi = TRUE,
                            q = TRUE,
                            overwrite = FALSE
                        )
                    )
                },
                .rollback = {
                    unlink(out_file)
                }
            )
        } else {
            # Merge into template
            .try(
                {
                    # merge using gdal warp
                    suppressWarnings(
                        gdalUtilities::gdalwarp(
                            srcfile = merge_files,
                            dstfile = out_file,
                            wo = paste0("NUM_THREADS=", multicores),
                            ot = .raster_gdal_datatype(data_type),
                            multi = TRUE,
                            of = "GTiff",
                            q = TRUE,
                            co = .conf("gdal_creation_options"),
                            overwrite = FALSE
                        )
                    )
                },
                .rollback = {
                    unlink(out_file)
                }
            )
        }
    }
    return(out_files)
}

.raster_clone <- function(file, nlayers = NULL) {
    r_obj <- .raster_open_rast(file = file)

    if (is.null(nlayers)) {
        nlayers <- .raster_nlayers(r_obj = r_obj)
    }
    r_obj <- .raster_rast(r_obj = r_obj, nlayers = nlayers, vals = NA)

    return(r_obj)
}

.raster_is_valid <- function(files, output_dir = NULL) {
    # resume processing in case of failure
    if (!all(file.exists(files))) {
        return(FALSE)
    }
    # check if files were already checked before
    checked_files <- NULL
    checked <- logical(0)
    if (!is.null(output_dir)) {
        checked_files <- .file_path(
            ".check", .file_sans_ext(files),
            ext = ".txt",
            output_dir = file.path(output_dir, ".sits"),
            create_dir = TRUE
        )
        checked <- file.exists(checked_files)
    }
    files <- files[!files %in% checked]
    if (length(files) == 0) {
        return(TRUE)
    }
    # try to open the file
    r_obj <- .try(
        {
            .raster_open_rast(files)
        },
        .default = {
            unlink(files)
            NULL
        }
    )
    # File is not valid
    if (is.null(r_obj)) {
        return(FALSE)
    }
    # if file can be opened, check if the result is correct
    # this file will not be processed again
    # Verify if the raster is corrupted
    check <- .try(
        {
            r_obj[.raster_ncols(r_obj) * .raster_nrows(r_obj)]
            TRUE
        },
        .default = {
            unlink(files)
            FALSE
        }
    )
    # Update checked files
    checked_files <- checked_files[!checked]
    if (.has(checked_files) && check) {
        for (file in checked_files) cat(file = file)
    }
    # Return check
    check
}

.raster_write_block <- function(files, block, bbox, values, data_type,
                                missing_value, crop_block = NULL) {
    # to support old models convert values to matrix
    values <- as.matrix(values)
    nlayers <- ncol(values)
    if (length(files) > 1) {
        if (length(files) != ncol(values)) {
            stop("number of output files does not match number of layers")
        }
        # Write each layer in a separate file
        nlayers <- 1
    }
    for (i in seq_along(files)) {
        # Get i-th file
        file <- files[[i]]
        # Get layers to be saved
        cols <- if (length(files) > 1) i else seq_len(nlayers)
        # Create a new raster
        r_obj <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = nlayers, crs = bbox[["crs"]]
        )
        # Copy values
        r_obj <- .raster_set_values(
            r_obj = r_obj,
            values = values[, cols]
        )
        # If no crop_block provided write the probabilities to a raster file
        if (is.null(crop_block)) {
            .raster_write_rast(
                r_obj = r_obj, file = file, data_type = data_type,
                overwrite = TRUE, missing_value = missing_value
            )
        } else {
            # Crop removing overlaps
            .raster_crop(
                r_obj = r_obj, file = file, data_type = data_type,
                overwrite = TRUE, block = crop_block,
                missing_value = missing_value
            )
        }
    }
    # Return file path
    files
}
