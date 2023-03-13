#' @title Supported raster packages
#' @keywords internal
#' @noRd
#' @return   Names of raster packages supported by sits
.raster_supported_packages <- function() {
    return(c("terra"))
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
        contains = c("row", "nrows", "col", "ncols"),
        msg = "invalid 'block' parameter"
    )

    # precondition 2
    .check_that(
        x = block[["row"]] > 0 && block[["col"]] > 0,
        msg = "invalid block"
    )

    # precondition 3
    .check_that(
        x = block[["nrows"]] > 0 && block[["ncols"]] > 0,
        msg = "invalid block"
    )
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
        contains = c("xmin", "xmax", "ymin", "ymax"),
        msg = "invalid 'bbox' parameter"
    )

    # precondition 2
    .check_that(
        x = bbox[["ymin"]] < bbox[["ymax"]],
        msg = "invalid 'bbox' parameter"
    )
}

#' @title Convert internal data type to gdal data type
#' @name .raster_gdal_datatype
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return GDAL datatype associated to internal data type used by sits
.raster_gdal_datatype <- function(data_type) {

    # GDAL data types
    gdal_data_types <- .raster_gdal_datatypes(sits_names = FALSE)
    names(gdal_data_types) <- .raster_gdal_datatypes(sits_names = TRUE)

    # check data_type type
    .check_chr(data_type,
               len_min = 1, len_max = 1,
               msg = "invalid 'data_type' parameter"
    )

    .check_chr_within(data_type,
                      within = .raster_gdal_datatypes(sits_names = TRUE),
                      discriminator = "one_of",
                      msg = "invalid 'data_type' parameter"
    )

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


#' @title Raster package internal data type representation
#' @name .raster_data_type
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data_type   sits internal raster data type.
#'
#' @return  internal data type used by raster package
.raster_data_type <- function(data_type) {

    # check data type
    .check_chr_within(
        x = data_type,
        within = .conf("valid_raster_data_types"),
        msg = "invalid 'data_type' parameter"
    )

    # check package
    pkg_class <- .raster_check_package()

    # call function
    UseMethod(".raster_data_type", pkg_class)
}

#' @title Raster package internal resampling method
#' @name .raster_resampling
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param method   sits internal raster resampling method.
#'
#' @return resampling method (if valid)
.raster_resampling <- function(method) {

    # check data type
    .check_chr_within(
        x = method,
        within = .conf("valid_raster_resampling"),
        msg = "invalid resampling 'method' parameter"
    )

    # check package
    pkg_class <- .raster_check_package()

    # call function
    UseMethod(".raster_resampling", pkg_class)
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
#' @param band            A numeric band index used to read bricks.
#' @param n               Number of values to extract.
#' @param sampling_window Window size to collect a point (in pixels).
#'
#' @return                A point `tibble` object.
#'
.raster_get_top_values <- function(r_obj,
                                   band,
                                   n,
                                   sampling_window) {

    # Pre-conditions have been checked in calling functions
    # Get top values
    # filter by median to avoid borders
    # Process window
    values <- .raster_get_values(r_obj)
    values <- C_kernel_median(
        x = values,
        ncols = .ncols(block),
        nrows = .nrows(block),
        band = 0,
        window_size = sampling_window
    )
    samples_tb <- max_sampling(
        data = values,
        band = band - 1,
        img_nrow = .raster_nrows(r_obj),
        img_ncol = .raster_ncols(r_obj),
        window_size = sampling_window
    )
    samples_tb <- dplyr::slice_max(
        samples_tb,
        .data[["value"]],
        n = n,
        with_ties = FALSE
    )

    # Get the values' positions.
    result_tb <- r_obj %>%
        terra::xyFromCell(
            cell = samples_tb[["cell"]]
        ) %>%
        tibble::as_tibble() %>%
        sf::st_as_sf(
            coords = c("x", "y"),
            crs = .raster_crs(r_obj),
            dim = "XY",
            remove = TRUE
        ) %>%
        sf::st_transform(crs = 4326) %>%
        sf::st_coordinates() %>%
        magrittr::set_colnames(
            value = c("longitude", "latitude")
        ) %>%
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

#' @title Raster package internal extract values function
#' @name .raster_ext_as_sf
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object
#'
#' @return An object with raster extent.
.raster_ext_as_sf <- function(r_obj) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_ext_as_sf", pkg_class)
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
    if (!purrr::is_null(block)) {
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
    .check_null(
        x = block,
        msg = "invalid 'block' parameter"
    )

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
#' @param bbox    numeric vector with (\code{xmin}, \code{xmax},
#'                \code{ymin}, \code{ymax}).
#' @param ...     additional parameters to be passed to raster package
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
.raster_crop_metadata <- function(r_obj, ..., block = NULL, bbox = NULL) {

    # pre-condition
    .check_that(
        is.null(block) || is.null(bbox),
        local_msg = "only either 'block' or 'bbox' should be informed",
        msg = "invalid crop parameter"
    )

    # check block
    if (!is.null(block)) {
        .raster_check_block(block = block)
    }

    # check bbox
    if (!is.null(bbox)) {
        .raster_check_bbox(bbox = bbox)
    }

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

#' @name .raster_crs
#' @keywords internal
#' @noRd
.raster_crs <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_crs", pkg_class)
}
#' @name .raster_sources
#' @keywords internal
#' @noRd
.raster_sources <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_sources", pkg_class)
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
        src_dataset = path.expand(base_file),
        dst_dataset = path.expand(out_file),
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
        out_file <- path.expand(out_files[[i]])
        # Check if out_file not exists
        .check_that(
            x = !file.exists(out_file),
            local_msg = paste0("file '", out_file, "' already exists"),
            msg = "invalid 'out_file' parameter"
        )
        # Get file paths
        merge_files <- purrr::map_chr(block_files, `[[`, i)
        # Expand paths for block_files
        merge_files <- path.expand(merge_files)
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
            .try({
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
            })
        } else {
            # Merge into template
            .try({
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
            })
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

#' @title Raster package internal open raster function
#' @name .raster_missing_value
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param file    raster file to be opened
#'
#' @return a numeric with no data value
.raster_missing_value <- function(file) {
    # set caller to show in errors
    .check_set_caller(".raster_missing_value")

    # check for file length == 1
    .check_that(
        length(file) == 1,
        msg = "more than one file were informed"
    )

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_missing_value", pkg_class)
}

.raster_is_valid <- function(files, output_dir = NULL) {
    # resume processing in case of failure
    if (!all(file.exists(files))) {
        return(FALSE)
    }
    # check if files were already checked before
    checked_file <- NULL
    checked <- character(0)
    if (!is.null(output_dir)) {
        checked_file <- .file_path(
            ".checked",
            ext = ".rds",
            output_dir = file.path(output_dir, ".sits"),
            create_dir = TRUE
        )
        if (file.exists(checked_file)) {
            checked <- readRDS(checked_file)
        }
    }
    files <- files[!files %in% checked]
    if (length(files) == 0) {
        return(TRUE)
    }
    # try to open the file
    r_obj <- .try({
        .raster_open_rast(files)
    },
    .default = {
        unlink(files)
        NULL
    })
    # File is not valid
    if (is.null(r_obj)) {
        return(FALSE)
    }
    # if file can be opened, check if the result is correct
    # this file will not be processed again
    # Verify if the raster is corrupted
    check <- .try({
        r_obj[.raster_ncols(r_obj) * .raster_nrows(r_obj)]
        TRUE
    },
    .default = {
        unlink(files)
        FALSE
    })
    # Update checked files
    if (!is.null(checked_file) && check) {
        checked <- c(checked, files)
        saveRDS(checked, checked_file)
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
