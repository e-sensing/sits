#' @title Supported raster packages
#' @keywords internal
#' @noRd
#' @return   Names of raster packages supported by sits
.raster_supported_packages <- function() {
    return("terra")
}

#' @title Check for raster package availability
#' @name .raster_check_package
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return name of the package.
.raster_check_package <- function() {
    pkg_class <- .conf_raster_pkg()
    class(pkg_class) <- pkg_class

    UseMethod(".raster_check_package", pkg_class)
}

#' @title Check for block object consistency
#' @name .raster_check_block
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @return  No value, called for side effects.
.raster_check_block <- function(block) {
    # set caller to show in errors
    .check_set_caller(".raster_check_block")
    # precondition 1
    .check_chr_contains(
        x = names(block),
        contains = c("row", "nrows", "col", "ncols")
    )
    # precondition 2
    .check_that(block[["row"]] > 0 && block[["col"]] > 0)
    # precondition 3
    .check_that(block[["nrows"]] > 0 && block[["ncols"]] > 0)
    return(invisible(block))
}

#' @title Check for bbox object consistency
#' @name .raster_check_bbox
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @return  No value, called for side effects.
.raster_check_bbox <- function(bbox) {
    # set caller to show in errors
    .check_set_caller(".raster_check_bbox")
    # precondition 1
    .check_chr_contains(
        x = names(bbox),
        contains = c("xmin", "xmax", "ymin", "ymax")
    )
    # precondition 2
    .check_that(bbox[["ymin"]] < bbox[["ymax"]])
    # precondition 3
    .check_that(bbox[["xmin"]] < bbox[["xmax"]])
    return(invisible(bbox))
}
#' @title Convert internal data type to gdal data type
#' @name .raster_gdal_datatype
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return GDAL datatype associated to internal data type used by sits
.raster_gdal_datatype <- function(data_type) {
    # set caller to show in errors
    .check_set_caller(".raster_gdal_datatype")
    # SITS data types
    sits_data_types <- .raster_gdal_datatypes(sits_names = TRUE)
    # GDAL data types
    gdal_data_types <- .raster_gdal_datatypes(sits_names = FALSE)
    names(gdal_data_types) <- sits_data_types
    # check data_type type
    .check_that(length(data_type) == 1)
    .check_that(data_type %in% sits_data_types)
    # convert
    return(gdal_data_types[[data_type]])
}
#' @title Match sits data types to GDAL data types
#' @name .raster_gdal_datatypes
#' @keywords internal
#' @noRd
#' @param sits_names a \code{logical} indicating whether the types are supported
#'  by sits.
#'
#' @return a \code{character} with datatypes.
.raster_gdal_datatypes <- function(sits_names = TRUE) {
    if (sits_names) {
        return(c(
            "INT1U", "INT2U", "INT2S", "INT4U", "INT4S",
            "FLT4S", "FLT8S"
        ))
    }

    return(c(
        "Byte", "UInt16", "Int16", "UInt32", "Int32",
        "Float32", "Float64"
    ))
}

#' @title Raster package internal get values function
#' @name .raster_get_values
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric matrix associated to raster object
.raster_get_values <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    # call function
    UseMethod(".raster_get_values", pkg_class)
}

#' @title Raster package internal set values function
#' @name .raster_set_values
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object
#' @param values  Numeric matrix to copy to raster object
#' @param ...     additional parameters to be passed to raster package
#'
#' @return        Raster object
.raster_set_values <- function(r_obj, values, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_set_values", pkg_class)
}

#' @title Raster package internal set values function
#' @name .raster_set_na
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj     raster package object
#' @param na_value  Numeric matrix to copy to raster object
#' @param ...       additional parameters to be passed to raster package
#'
#' @return Raster object
.raster_set_na <- function(r_obj, na_value, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_set_na", pkg_class)
}

#' @title Get top values of a raster.
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @keywords internal
#' @noRd
#' @description
#' Get the top values of a raster as a point `sf` object. The values
#' locations are guaranteed to be separated by a certain number of pixels.
#'
#' @param r_obj           A raster object.
#' @param block           Individual block that will be processed.
#' @param band            A numeric band index used to read bricks.
#' @param n               Number of values to extract.
#' @param sampling_window Window size to collect a point (in pixels).
#'
#' @return                A point `tibble` object.
#'
.raster_get_top_values <- function(r_obj,
                                   block,
                                   band,
                                   n,
                                   sampling_window) {
    # Pre-conditions have been checked in calling functions
    # Get top values
    # filter by median to avoid borders
    # Process window
    values <- .raster_get_values(
        r_obj,
        row = block[["row"]],
        col = block[["col"]],
        nrows = block[["nrows"]],
        ncols = block[["ncols"]]
    )
    values <- C_kernel_median(
        x = values,
        nrows = block[["nrows"]],
        ncols = block[["ncols"]],
        band = 0,
        window_size = sampling_window
    )
    samples_tb <- C_max_sampling(
        x = values,
        nrows = block[["nrows"]],
        ncols = block[["ncols"]],
        window_size = sampling_window
    )
    samples_tb <- dplyr::slice_max(
        samples_tb,
        .data[["value"]],
        n = n,
        with_ties = FALSE
    )

    tb <- r_obj |>
        terra::xyFromCell(
            cell = samples_tb[["cell"]]
        ) |>
        tibble::as_tibble()
    # find NA
    na_rows <- which(is.na(tb))
    # remove NA
    if (length(na_rows) > 0) {
        tb <- tb[-na_rows, ]
        samples_tb <- samples_tb[-na_rows, ]
    }
    # Get the values' positions.
    result_tb <- tb |>
        sf::st_as_sf(
            coords = c("x", "y"),
            crs = .raster_crs(r_obj),
            dim = "XY",
            remove = TRUE
        ) |>
        sf::st_transform(crs = "EPSG:4326") |>
        sf::st_coordinates()

    colnames(result_tb) <- c("longitude", "latitude")
    result_tb <- result_tb |>
        dplyr::bind_cols(samples_tb)

    return(result_tb)
}

#' @title Raster package internal extract values function
#' @name .raster_extract
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object
#' @param xy      numeric matrix with coordinates
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric matrix with raster values for each coordinate
.raster_extract <- function(r_obj, xy, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_extract", pkg_class)
}

#' @name .raster_file_blocksize
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj  raster package object
#'
#' @return An vector with the file block size.
.raster_file_blocksize <- function(r_obj) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_file_blocksize", pkg_class)
}

#' @title Raster package internal object creation
#' @name .raster_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object to be cloned
#' @param nlayers  number of raster layers
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Raster package object
.raster_rast <- function(r_obj, nlayers = 1, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_rast", pkg_class)
}

#' @title Raster package internal open raster function
#' @name .raster_open_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param file    raster file to be opened
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Raster package object
.raster_open_rast <- function(file, ...) {
    # set caller to show in errors
    .check_set_caller(".raster_open_rast")

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_open_rast", pkg_class)
}

#' @title Raster package internal write raster file function
#' @name .raster_write_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj         raster package object to be written
#' @param file          file path to save raster file
#' @param format        GDAL file format string (e.g. GTiff)
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param overwrite     logical indicating if file can be overwritten
#' @param ...           additional parameters to be passed to raster package
#' @param missing_value A \code{integer} with image's missing value
#'
#' @return              No value, called for side effects.
.raster_write_rast <- function(r_obj,
                               file,
                               data_type,
                               overwrite, ...,
                               missing_value = NA) {
    # check package
    pkg_class <- .raster_check_package()
    UseMethod(".raster_write_rast", pkg_class)
}

#' @title Raster package internal create raster object function
#' @name .raster_new_rast
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param nrows         Number of rows in the raster
#' @param ncols         Number of columns in the raster
#' @param xmin          X minimum of raster origin
#' @param xmax          X maximum of raster origin
#' @param ymin          Y minimum of raster origin
#' @param ymax          Y maximum of raster origin
#' @param nlayers       Number of layers of the raster
#' @param crs           Coordinate Reference System of the raster
#' @param ...           additional parameters to be passed to raster package
#'
#' @return               A raster object.
.raster_new_rast <- function(nrows,
                             ncols,
                             xmin,
                             xmax,
                             ymin,
                             ymax,
                             nlayers,
                             crs, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_new_rast", pkg_class)
}

#' @title Raster package internal read raster file function
#' @name .raster_read_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param file    path to raster file(s) to be read
#' @param block   a valid block with (\code{col}, \code{row},
#'                \code{ncols}, \code{nrows}).
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric matrix read from file based on parameter block
.raster_read_rast <- function(files, ..., block = NULL) {
    # check block
    if (.has(block)) {
        .raster_check_block(block = block)
    }

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_read_rast", pkg_class)
}

#' @title Raster package internal crop raster function
#' @name .raster_crop
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj         Raster package object to be written
#' @param file          File name to save cropped raster.
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param overwrite     logical indicating if file can be overwritten
#' @param block         a valid block with (\code{col}, \code{row},
#'                      \code{ncols}, \code{nrows}).
#' @param missing_value A \code{integer} with image's missing value
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
.raster_crop <- function(r_obj,
                         file,
                         data_type,
                         overwrite,
                         block,
                         missing_value = NA) {
    # pre-condition
    .check_null_parameter(block)
    # check block
    .raster_check_block(block = block)

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_crop", pkg_class)
}

#' @title Raster package internal crop raster function
#' @name .raster_crop_metadata
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object to be written
#' @param block   a valid block with (\code{col}, \code{row},
#'                \code{ncols}, \code{nrows}).
#' @param bbox    numeric vector with (xmin, xmax, ymin, ymax).
#' @param ...     additional parameters to be passed to raster package
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
.raster_crop_metadata <- function(r_obj, ..., block = NULL, bbox = NULL) {
    # set caller to show in errors
    .check_set_caller(".raster_crop_metadata")
    # pre-condition
    .check_that(.has_not(block) || .has_not(bbox))
    # check block
    if (.has(block))
        .raster_check_block(block = block)
    # check bbox
    if (.has(bbox))
        .raster_check_bbox(bbox = bbox)
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_crop_metadata", pkg_class)
}

#' @title Raster package internal object properties
#' @name .raster_properties
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object
#' @param ...      additional parameters to be passed to raster package
#'
#' @return Raster object spatial properties
.raster_nrows <- function(r_obj, ...) {
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
#' @name .raster_scale
#' @keywords internal
#' @noRd
.raster_scale <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_scale", pkg_class)
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

#' @title Raster package internal raster data type
#' @name .raster_datatype
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param r_obj    raster package object
#' @param by_layer  A logical value indicating the type of return
#' @param ...      additional parameters to be passed to raster package
#'
#' @return A character value with data type
.raster_datatype <- function(r_obj, ..., by_layer = TRUE) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_datatype", pkg_class)
}

#' @title Raster package internal summary values function
#' @name .raster_summary
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param r_obj    raster package object to count values
#' @param ...      additional parameters to be passed to raster package
#'
#' @return matrix with layer, value, and count columns
.raster_summary <- function(r_obj, ...) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_summary", pkg_class)
}

#' @title Return col value given an X coordinate
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj  raster package object
#' @param x      X coordinate in raster projection
#'
#' @return integer with column
.raster_col <- function(r_obj, x) {
    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_col", pkg_class)
}
#' @title Return row value given an Y coordinate
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj  raster object
#' @param y      Y coordinate in raster projection
#'
#' @return integer with row number
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
    .check_that(all(file.exists(file)))
    # use first file
    file <- file[[1]]
    # open file
    r_obj <- .raster_open_rast(file = file)
    # build params file
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

#' @title Raster-to-vector
#' @name .raster_extract_polygons
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return name of the package.
.raster_extract_polygons <- function(r_obj, dissolve = TRUE, ...) {
    # check package
    pkg_class <- .raster_check_package()
    UseMethod(".raster_extract_polygons", pkg_class)
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
    # set caller to show in errors
    .check_set_caller(".raster_merge_blocks")
    # Check consistency between block_files and out_files
    if (is.list(block_files)) {
        .check_that(all(lengths(block_files) == length(out_files)))
    } else {
        .check_that(length(out_files) == 1)
        block_files <- as.list(block_files)
    }
    # for each file merge blocks
    for (i in seq_along(out_files)) {
        # Expand paths for out_file
        out_file <- .file_normalize(out_files[[i]])
        # Check if out_file does not exist
        .check_that(!file.exists(out_file))
        # Get file paths
        merge_files <- purrr::map_chr(block_files, `[[`, i)
        # Expand paths for block_files
        merge_files <- .file_normalize(merge_files)
        # check if block_files length is at least one
        .check_file(
            x = merge_files,
            extensions = "tif"
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
    return(invisible(out_files))
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
    .check_set_caller(".raster_write_block")
    # to support old models convert values to matrix
    values <- as.matrix(values)
    nlayers <- ncol(values)
    if (length(files) > 1) {
        .check_that(
            length(files) == ncol(values),
            msg = .conf("messages", ".raster_write_block_mismatch")
        )
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
