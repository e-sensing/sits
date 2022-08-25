#' @title Supported raster packages
#' @keywords internal
#' @return   Names of raster packages supported by sits
.raster_supported_packages <- function() {
    return(c("terra"))
}

#' @title Check for raster package availability
#' @name .raster_check_package
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return name of the package.
.raster_check_package <- function() {
    pkg_class <- .config_raster_pkg()
    class(pkg_class) <- pkg_class

    UseMethod(".raster_check_package", pkg_class)
}

#' @title Check for block object consistency
#' @name .raster_check_block
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @return  No value, called for side effects.
.raster_check_block <- function(block) {

    # set caller to show in errors
    .check_set_caller(".raster_check_block")

    # precondition 1
    .check_chr_contains(
        x = names(block),
        contains = c("first_row", "nrows", "first_col", "ncols"),
        msg = "invalid 'block' parameter"
    )

    # precondition 2
    .check_that(
        x = block[["first_row"]] > 0 && block[["first_col"]] > 0,
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
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data_type   sits internal raster data type.
#'
#' @return  internal data type used by raster package
.raster_data_type <- function(data_type) {

    # check data type
    .check_chr_within(
        x = data_type,
        within = .config_get("valid_raster_data_types"),
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
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param method   sits internal raster resampling method.
#'
#' @return resampling method (if valid)
.raster_resampling <- function(method) {

    # check data type
    .check_chr_within(
        x = method,
        within = .config_get("valid_raster_resampling"),
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

#' @title Raster package internal extract values function
#' @name .raster_extract
#' @keywords internal
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

#' @title Raster package internal extract values function
#' @name .raster_ext_as_sf
#' @keywords internal
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
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param file    raster file to be opened
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Raster package object
.raster_open_rast <- function(file, ...) {

    # set caller to show in errors
    .check_set_caller(".raster_open_rast")

    # check for file length == 1
    .check_that(
        length(file) == 1,
        msg = "more than one file were informed"
    )

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_open_rast", pkg_class)
}

#' @title Raster package internal write raster file function
#' @name .raster_write_rast
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj         raster package object to be written
#' @param file          file path to save raster file
#' @param format        GDAL file format string (e.g. GTiff)
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param gdal_options  GDAL creation option string (e.g. COMPRESS=LZW)
#' @param overwrite     logical indicating if file can be overwritten
#' @param ...           additional parameters to be passed to raster package
#' @param missing_value A \code{integer} with image's missing value
#'
#' @return              No value, called for side effects.
.raster_write_rast <- function(r_obj,
                               file,
                               format,
                               data_type,
                               gdal_options,
                               overwrite, ...,
                               missing_value = NA) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_write_rast", pkg_class)
}

#' @title Raster package internal create raster object function
#' @name .raster_new_rast
#' @keywords internal
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

#' @title Raster package internal open raster stack function
#' @name .raster_open_stack
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param files   raster files to be opened
#' @param ...     additional parameters to be passed to raster package
#'
#' @return raster package object
.raster_open_stack <- function(files, ...) {

    # set caller to show in errors
    .check_set_caller(".raster_open_stack")

    # check for files length > 0
    .check_length(
        x = files,
        min = 1,
        msg = "no file informed"
    )

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_open_stack", pkg_class)
}

#' @title Raster package internal read raster stack file function
#' @name .raster_read_stack
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param files   raster files to be read
#' @param block   numeric vector with names "first_col", "ncols",
#'   "first_row", "nrows".
#' @param out_size a numeric vector with names "nrows" and "ncols"
#' @param method  method used to resample pixels (used only in case of
#' out_size parameter is informed)
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric matrix read from file based on parameter block
.raster_read_stack <- function(files, ...,
                               block = NULL,
                               out_size = NULL,
                               method = "bilinear") {

    # check out_size
    if (!purrr::is_null(out_size)) {
        .check_chr_contains(
            names(out_size),
            contains = c("nrows", "ncols"),
            msg = "invalid 'out_size' parameter"
        )
    }

    # check method
    .check_chr_within(
        method,
        within = .config_get("valid_raster_resampling"),
        msg = "invalid 'method' parameter"
    )

    # check block
    if (!purrr::is_null(block)) {
        .raster_check_block(block = block)
    }

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_read_stack", pkg_class)
}

#' @title Raster package internal crop raster function
#' @name .raster_crop
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj         Raster package object to be written
#' @param file          File name to save cropped raster.
#' @param format        GDAL file format string (e.g. GTiff)
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param gdal_options  GDAL creation option string (e.g. COMPRESS=LZW)
#' @param overwrite     logical indicating if file can be overwritten
#' @param block         numeric vector with names "first_col", "ncols", "first_row",
#'                      "nrows".
#' @param missing_value A \code{integer} with image's missing value
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
.raster_crop <- function(r_obj,
                         file,
                         format,
                         data_type,
                         gdal_options,
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
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object to be written
#' @param block   numeric vector with names "first_col", "ncols", "first_row",
#'                "nrows".
#' @param bbox    numeric vector with names "xmin", "xmax", "ymin", "ymax".
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

#' @name .raster_properties
#' @keywords internal
.raster_ncols <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_ncols", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_nlayers <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_nlayers", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_xmax <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_xmax", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_xmin <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_xmin", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_ymax <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_ymax", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_ymin <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_ymin", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_xres <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_xres", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_yres <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_yres", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
.raster_crs <- function(r_obj, ...) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_crs", pkg_class)
}

#' @name .raster_properties
#' @keywords internal
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

#' @name .raster_properties
#' @keywords internal
.raster_res <- function(r_obj, ...) {

    # return a named resolution
    res <- list(
        xres = .raster_xres(r_obj),
        yres = .raster_yres(r_obj)
    )

    return(res)
}

#' @name .raster_properties
#' @keywords internal
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


#' @name .raster_colrow
#' @keywords internal
.raster_row <- function(r_obj, y) {

    # check package
    pkg_class <- .raster_check_package()

    UseMethod(".raster_row", pkg_class)
}

#' @title Determine the file params to write in the metadata
#' @name .raster_params_file
#' @keywords internal
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
#' @title Merge all input files into one raster file
#' @name .raster_merge
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param in_files       Input file paths
#' @param out_file       Output raster file path
#' @param format         Format to write the file
#' @param gdal_datatype  Data type in gdal format
#' @param gdal_options   Compression method to be used
#' @param overwrite      Overwrite the file?
#' @param progress       Show progress bar?
#'
#' @return No return value, called for side effects.
#'
.raster_merge <- function(in_files,
                          out_file,
                          format,
                          gdal_datatype,
                          gdal_options,
                          overwrite,
                          progress = FALSE) {

    # set caller to show in errors
    .check_set_caller(".raster_merge")

    # check documentation mode
    progress <- .check_documentation(progress)

    # check if in_file length is at least one
    .check_length(
        x = in_files,
        min = 1,
        msg = "no file to merge"
    )

    # check if all informed files exist
    .check_that(
        x = all(file.exists(in_files)),
        msg = "file does not exist"
    )

    # check overwrite parameter
    .check_that(
        x = (file.exists(out_file) && overwrite) ||
            !file.exists(out_file),
        msg = "cannot overwrite existing file"
    )

    # delete result file
    if (file.exists(out_file)) {
        unlink(out_file)
    }

    # maximum files to merge at a time
    # this value was obtained empirically
    group_len <- 32

    # keep in_files
    delete_files <- FALSE

    # prepare to loop
    loop_files <- in_files

    # loop until one file is obtained
    while (length(loop_files) > 1) {

        # group input files to be merged
        group_files <- tapply(
            loop_files,
            rep(seq_len(ceiling(length(loop_files) / group_len)),
                each = group_len,
                length.out = length(loop_files)
            ), c
        )

        # merge groups
        loop_files <- .sits_parallel_map(group_files, function(group) {
            srcfile <- unlist(group)

            # temp output file
            dstfile <- tempfile(
                tmpdir = dirname(out_file[[1]]),
                fileext = ".tif"
            )

            # merge using gdal warp
            gdalUtilities::gdalwarp(
                srcfile = path.expand(srcfile),
                dstfile = dstfile,
                ot = gdal_datatype,
                of = format,
                co = gdal_options,
                overwrite = overwrite
            )

            # delete temp files
            if (delete_files) unlink(srcfile)

            return(dstfile)
        }, progress = progress)

        loop_files <- unlist(loop_files)

        # delete temp files
        delete_files <- TRUE
    }

    # in_files is the output of above loop
    file.rename(loop_files, out_file)

    # delete
    unlink(in_files)

    return(invisible(NULL))
}
