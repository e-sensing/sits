#' @title Check for raster package availability
#' @name .sits_raster_api_check_package
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return package representation class
.sits_raster_api_check_package <- function() {

    pkg_class <- .sits_config_raster_package()
    class(pkg_class) <- pkg_class

    UseMethod(".sits_raster_api_check_package", pkg_class)
}

#' @keywords internal
#' @export
.sits_raster_api_check_package.default <- function() {

    stop("No API defined for this raster package.")
}

#' @title Check for block object consistency
#' @name .sits_raster_api_check_block
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
.sits_raster_api_check_block <- function(block) {

    # precondition 1
    assertthat::assert_that(
        all(c("row", "nrows", "col", "ncols") %in% names(block)),
        msg = paste(".sits_raster_api_check_block: block object must contains",
                    "'row', 'nrows', 'col', 'ncols' entries")
    )

    # precondition 2
    assertthat::assert_that(
        block[["row"]] > 0 && block[["col"]] > 0,
        msg = ".sits_raster_api_check_block: invalid block"
    )

    # precondition 3
    assertthat::assert_that(
        block[["nrows"]] > 0 && block[["ncols"]] > 0,
        msg = ".sits_raster_api_check_block: invalid block"
    )

}

#' @title Convert internal data type to gdal data type
#' @name .sits_raster_api_gdal_datatype
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @return character string
.sits_raster_api_gdal_datatype <- function(data_type) {

    # allowed data types
    valid_data_types <- c("INT1U", "INT2U", "INT2S", "INT4U",
                          "INT4S", "FLT4S", "FLT8S")

    # GDAL data types
    gdal_data_types <- c("Byte", "UInt16", "Int16", "UInt32",
                         "Int32", "Float32", "Float64")

    # check data_type type
    assertthat::assert_that(
        is.character(data_type),
        msg = paste(".sits_raster_api_data_type: data_type must be a ",
                    "character string")
    )

    # check data type
    assertthat::assert_that(
        all(data_type %in% valid_data_types),
        msg = paste(".sits_raster_api_data_type: valid data types are",
                    paste0("'", valid_data_types, "'", collapse = ", "))
    )

    # convert
    return(gdal_data_types[match(data_type, valid_data_types)])
}

#' @title Raster package internal data type representation
#' @name .sits_raster_api_data_type
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data_type   sits internal raster data type. One of "INT1U", "INT2U",
#'                    "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#'
#' @return character string
.sits_raster_api_data_type <- function(data_type) {

    # allowed data types
    valid_data_types <- c("INT1U", "INT2U", "INT2S", "INT4U",
                          "INT4S", "FLT4S", "FLT8S")
    # check data type
    assertthat::assert_that(
        all(data_type %in% valid_data_types),
        msg = paste(".sits_raster_api_data_type: valid data types are",
                    paste0("'", valid_data_types, "'", collapse = ", "))
    )

    # check package
    pkg_class <- .sits_raster_api_check_package()

    # call function
    UseMethod(".sits_raster_api_data_type", pkg_class)
}

#' @title Raster package internal get values function
#' @name .sits_raster_api_get_values
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric matrix
.sits_raster_api_get_values <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    # call function
    UseMethod(".sits_raster_api_get_values", pkg_class)
}

#' @title Raster package internal set values function
#' @name .sits_raster_api_set_values
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object
#' @param values  Numeric matrix to copy to raster object
#' @param ...     additional parameters to be passed to raster package
#'
#' @return raster package object
.sits_raster_api_set_values <- function(r_obj, values, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_set_values", pkg_class)
}

#' @title Raster package internal extract values function
#' @name .sits_raster_api_extract
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object
#' @param xy      numeric matrix with coordinates
#' @param ...     additional parameters to be passed to raster package
#'
#' @return numeric matrix with raster values for each coordinate
.sits_raster_api_extract <- function(r_obj, xy, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_extract", pkg_class)
}

#' @title Raster package internal object creation
#' @name .sits_raster_api_rast
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object to be cloned
#' @param nlayers  number of raster layers
#' @param ...     additional parameters to be passed to raster package
#'
#' @return raster package object
.sits_raster_api_rast <- function(r_obj, nlayers = 1, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_rast", pkg_class)
}

#' @title Raster package internal open raster function
#' @name .sits_raster_api_open_rast
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param file    raster file to be opened
#' @param ...     additional parameters to be passed to raster package
#'
#' @return raster package object
.sits_raster_api_open_rast <- function(file, ...) {

    # check for file length == 1
    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_open_rast: more than one file were informed"
    )

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_open_rast", pkg_class)
}

#' @title Raster package internal read raster file function
#' @name .sits_raster_api_read_rast
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param file    raster file to be read
#' @param block   numeric vector with names "col", "ncols", "row", "nrows".
#' @param ...     additional parameters to be passed to raster package
#'
#' @return numeric matrix
.sits_raster_api_read_rast <- function(file,
                                       block = NULL, ...) {

    # check for files length == 1
    assertthat::assert_that(
        length(file) == 1,
        msg = ".sits_raster_api_read_rast: more than one file were informed"
    )

    # check block
    if (!purrr::is_null(block)) {

        .sits_raster_api_check_block(block = block)
    }

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_read_rast", pkg_class)
}

#' @title Raster package internal write raster file function
#' @name .sits_raster_api_read_rast
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
#'
#' @return numeric matrix
.sits_raster_api_write_rast <- function(r_obj,
                                        file,
                                        format,
                                        data_type,
                                        gdal_options,
                                        overwrite, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_write_rast", pkg_class)
}

#' @title Raster package internal create raster object function
#' @name .sits_raster_api_new_rast
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
.sits_raster_api_new_rast <- function(nrows,
                                      ncols,
                                      xmin,
                                      xmax,
                                      ymin,
                                      ymax,
                                      nlayers,
                                      crs, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_new_rast", pkg_class)
}

#' @title Raster package internal open raster stack function
#' @name .sits_raster_api_open_stack
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param files   raster files to be opened
#' @param ...     additional parameters to be passed to raster package
#'
#' @return raster package object
.sits_raster_api_open_stack <- function(files, ...) {

    # check for files length > 0
    assertthat::assert_that(
        length(files) > 0,
        msg = ".sits_raster_api_open_stack: no file informed"
    )

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_open_stack", pkg_class)
}

#' @title Raster package internal read raster stack file function
#' @name .sits_raster_api_read_stack
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param files   raster files to be read
#' @param block   numeric vector with names "col", "ncols", "row", "nrows".
#' @param ...     additional parameters to be passed to raster package
#'
#' @return numeric matrix
.sits_raster_api_read_stack <- function(files,
                                        block = NULL, ...) {

    # check block
    if (!purrr::is_null(block)) {

        .sits_raster_api_check_block(block = block)
    }

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_read_stack", pkg_class)
}

#' @title Raster package internal crop raster function
#' @name .sits_raster_api_crop
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj   raster package object to be written
#' @param block   numeric vector with names "col", "ncols", "row", "nrows".
#' @param ...     additional parameters to be passed to raster package
#'
#' @return numeric matrix
.sits_raster_api_crop <- function(r_obj, block, ...) {

    # check block
    .sits_raster_api_check_block(block = block)

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_crop", pkg_class)
}

#' @title Raster package internal object properties
#' @name .sits_raster_api_properties
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object
#' @param ...      additional parameters to be passed to raster package
#'
#' @return raster object spatial properties
.sits_raster_api_nrows <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_nrows", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_ncols <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_ncols", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_nlayers <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_nlayers", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_xmax <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_xmax", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_xmin <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_xmin", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_ymax <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_ymax", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_ymin <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_ymin", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_xres <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_xres", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_yres <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_yres", pkg_class)
}

#' @name .sits_raster_api_properties
.sits_raster_api_crs <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_crs", pkg_class)
}

#' @title Raster package internal frequency values function
#' @name .sits_raster_api_freq
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj    raster package object to count values
#' @param ...      additional parameters to be passed to raster package
#'
#' @return matrix with layer, value, and count columns
.sits_raster_api_freq <- function(r_obj, ...) {

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_freq", pkg_class)
}

#' @title Raster package internal moving window function
#' @name .sits_raster_api_focal
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param r_obj        raster package object to pass a window function
#' @param window_size  number indicating the length of a squared window's side.
#' @param fn           a function to be convoluted. Can be either a string or
#'                     a R function. Character strings options are: "sum",
#'                     "mean", and "modal".
#' @param ...          additional parameters to be passed to raster package
#'
#' @return raster package object
.sits_raster_api_focal <- function(r_obj,
                                   window_size,
                                   fn, ...) {

    # check window_size
    assertthat::assert_that(
        window_size %% 2 == 1,
        msg = ".sits_raster_api_focal: window_size must be an odd number"
    )
    # check fun parameter
    if (is.character(fn)) {

        assertthat::assert_that(
            length(fn) == 1,
            msg = ".sits_raster_api_focal: length of fn parameter must be one"
        )

        assertthat::assert_that(
            fn %in% c("modal", "sum", "mean"),
            msg = ".sits_raster_api_focal: invalid function"
        )
    }

    # check package
    pkg_class <- .sits_raster_api_check_package()

    UseMethod(".sits_raster_api_focal", pkg_class)
}

#' @title Determine the file params to write in the metadata
#' @name .sits_raster_api_params_file
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its spatial parameters
#' @param file     A path to a raster file
#'
#' @return A tibble with the raster spatial parameters
.sits_raster_api_params_file <- function(file) {

    # preconditions
    assertthat::assert_that(
        length(file) > 0,
        msg = ".sits_raster_api_params_file: no file was informed"
    )

    # use first file
    file <- file[[1]]

    # open file
    r_obj <- .sits_raster_api_open_rast(file = file)

    params <- tibble::tibble(
        nrows = .sits_raster_api_nrows(r_obj = r_obj),
        ncols = .sits_raster_api_ncols(r_obj = r_obj),
        xmin  = .sits_raster_api_xmin(r_obj = r_obj),
        xmax  = .sits_raster_api_xmax(r_obj = r_obj),
        ymin  = .sits_raster_api_ymin(r_obj = r_obj),
        ymax  = .sits_raster_api_ymax(r_obj = r_obj),
        xres  = .sits_raster_api_xres(r_obj = r_obj),
        yres  = .sits_raster_api_yres(r_obj = r_obj),
        crs   = .sits_raster_api_crs(r_obj = r_obj)
    )

    return(params)
}
#' @title Given a band, return a set of values for chosen location
#' @name .sits_cube_extract
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description          Given a data cube, retrieve the time series
#'                       of XY locations
#'
#' @param cube           Metadata about a data cube
#' @param band_cube      Name of the band to the retrieved
#' @param xy             Matrix with XY location
.sits_cube_extract <- function(cube, band_cube, xy) {


    # precondition 2
    assertthat::assert_that(
        band_cube %in% sits_bands(cube),
        msg = paste(".sits_raster_api_extract: band", band_cube,
                    "is not available in the cube ", cube$name)
    )

    # filter the files that contain the band
    band <- dplyr::filter(cube$file_info[[1]], band == band_cube)

    # create a stack object
    r_obj <- .sits_raster_api_open_stack(band$path)

    # extract the values
    values <- .sits_raster_api_extract(r_obj, xy)

    # is the data valid?
    assertthat::assert_that(
        nrow(values) == nrow(xy),
        msg = ".sits_raster_api_extract: error in retrieving data"
    )
    return(values)
}

#' @title Given a labelled cube, return the band information
#' @name .sits_cube_area_freq
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param cube           Metadata about a data cube
#' @return               Frequency of each label in the data cube
#'
.sits_cube_area_freq <- function(cube) {

    # precondition
    assertthat::assert_that(
        inherits(cube, "classified_image"),
        msg = ".sits_cube_area_freq: requires a labelled cube"
    )

    # retrieve the r object associated to the labelled cube
    file_info <- cube$file_info[[1]]

    # open first raster
    r_obj <- .sits_raster_api_open_rast(file_info$path[[1]])

    # retrieve the frequency
    freq <- tibble::as_tibble(.sits_raster_api_freq(r_obj))

    return(freq)
}

#' @title Merge all input files into one raster file
#' @name .sits_raster_api_merge
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param in_files       Input file paths
#' @param out_file       Output raster file path
#' @param format         Format to write the file
#' @param gdal_datatype  Data type in gdal format
#' @param gdal_options   Compression method to be used
#' @param overwrite      Overwrite the file?
.sits_raster_api_merge <- function(in_files,
                                   out_file,
                                   format,
                                   gdal_datatype,
                                   gdal_options,
                                   overwrite) {

    # check if in_file length is at least one
    assertthat::assert_that(
        length(in_files) > 0,
        msg = ".sits_raster_api_merge: no file to merge"
    )

    # check if all informed files exist
    assertthat::assert_that(
        all(file.exists(in_files)),
        msg = ".sits_raster_api_merge: file does not exist"
    )

    # check overwrite parameter
    assertthat::assert_that(
        (file.exists(out_file) && overwrite) ||
            !file.exists(out_file),
        msg = ".sits_raster_api_merge: cannot overwrite existing file"
    )

    # delete result file
    if (file.exists(out_file))
        unlink(out_file)

    # maximum files to merge at a time
    # this value was obtained empirically
    group_len <- min(ceiling(length(in_files) / future::nbrOfWorkers()), 256)

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
                length.out = length(loop_files)),
            c
        )

        # merge groups
        loop_files <- furrr::future_map_chr(group_files, function(group) {

            srcfile <- unlist(group)

            # temp output file
            dstfile <- tempfile(tmpdir = dirname(out_file[[1]]),
                                fileext = ".tif")

            # merge using gdal warp
            gdalUtilities::gdalwarp(
                srcfile = srcfile,
                dstfile = dstfile,
                ot = gdal_datatype,
                of = format,
                co = gdal_options,
                overwrite = overwrite
            )

            # delete temp files
            if (delete_files) unlink(srcfile)

            # call garbage collector
            gc()

            return(dstfile)
        })

        # delete temp files
        delete_files <- TRUE

    }

    # in_files is the output of above loop
    file.rename(loop_files, out_file, copy.date = TRUE)

    # delete
    unlink(in_files)

    return(invisible(NULL))
}

#' @title Determine the block spatial parameters of a given cube
#' @name .sits_cube_params_block
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its parameters
#' @param cube     A valid cube
#' @param block    A block insider the cube
#' @return A tibble with the cube parameters
.sits_cube_params_block <- function(cube, block) {

    # compute new Y extent
    ymax  <-  cube$ymax - (block[["row"]] - 1) * cube$yres
    ymin  <-  ymax - block[["nrows"]] * cube$yres

    # prepare result
    params <- tibble::tibble(
        nrows = block[["nrows"]],
        ncols = block[["ncols"]],
        xmin  = cube$xmin,
        xmax  = cube$xmax,
        ymin  = ymin,
        ymax  = ymax,
        xres  = cube$xres,
        yres  = cube$yres,
        crs   = cube$crs
    )

    return(params)
}
