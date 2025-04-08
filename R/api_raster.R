#' @title Supported raster packages
#' @keywords internal
#' @noRd
#' @return   Names of raster packages supported by sits
.raster_supported_packages <- function() {
    return("terra")
}
#' @title Check for block object consistency
#' @name .raster_check_block
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast   raster package object
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric matrix associated to raster object
.raster_get_values <- function(rast, ...) {
    # read values and close connection
    terra::readStart(x = rast)
    res <- terra::readValues(x = rast, mat = TRUE, ...)
    terra::readStop(x = rast)
    return(res)
}

#' @title Raster package internal set values function
#' @name .raster_set_values
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast   raster package object
#' @param values  Numeric matrix to copy to raster object
#' @param ...     additional parameters to be passed to raster package
#'
#' @return        Raster object
.raster_set_values <- function(rast, values, ...) {
    terra::values(x = rast) <- as.matrix(values)
    return(rast)
}
#' @title Raster package internal get values for rasters in memory
#' @name .raster_values_mem
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast   raster package object
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric vector with values
.raster_values_mem <- function(rast, ...) {
    # read values and close connection
    res <- terra::values(x = rast, ...)
    return(res)
}
#' @title Raster package internal set min max
#' @name .raster_set_minmax
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast   raster package object
#'
#' @return        Raster object with additional minmax information
.raster_set_minmax <- function(rast) {
    terra::setMinMax(rast)
    return(invisible(rast))
}
#' @title Raster package internal stretch function
#' @name .raster_stretch
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast   raster package object
#' @param minv   minimum value
#' @param maxv   maximum value
#' @param minq   first quartile
#' @param minq   last quartile
#'
#' @return        Raster object with additional minmax information
.raster_stretch <- function(rast, minv, maxv, minq, maxq) {
    # # stretch the raster
    rast <- terra::stretch(rast, minv, maxv, minq, maxq)
}
#' @title Raster package internal set values function
#' @name .raster_set_na
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast     raster package object
#' @param na_value  Numeric matrix to copy to raster object
#' @param ...       additional parameters to be passed to raster package
#'
#' @return Raster object
.raster_set_na <- function(rast, na_value, ...) {
    terra::NAflag(x = rast) <- na_value
    return(rast)
}

#' @title Get top values of a raster.
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@gmail.com}
#' @keywords internal
#' @noRd
#' @description
#' Get the top values of a raster as a point `sf` object. The values
#' locations are guaranteed to be separated by a certain number of pixels.
#'
#' @param rast           A raster object.
#' @param block           Individual block that will be processed.
#' @param band            A numeric band index used to read bricks.
#' @param n               Number of values to extract.
#' @param sampling_window Window size to collect a point (in pixels).
#'
#' @return                A point `tibble` object.
#'
.raster_get_top_values <- function(rast,
                                   block,
                                   band,
                                   n,
                                   sampling_window) {
    # Pre-conditions have been checked in calling functions
    # Get top values
    # filter by median to avoid borders
    # Process window
    values <- .raster_get_values(
        rast,
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

    tb <- rast |>
        .raster_xy_from_cell(
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
            crs = .raster_crs(rast),
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast   raster package object
#' @param xy      numeric matrix with coordinates
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Numeric matrix with raster values for each coordinate
.raster_extract <- function(rast, xy, ...) {
    terra::extract(x = rast, y = xy, ...)
}
#'
#' @title Return sample of values from terra object
#' @keywords internal
#' @name .raster_sample
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast  raster object
#' @param size   size of sample
#' @param ...     additional parameters to be passed to raster package
#' @return numeric matrix
.raster_sample <- function(rast, size, ...) {
    terra::spatSample(rast, size, ...)
}
#' @title Return block size of a raster
#' @name .raster_file_blocksize
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast  raster package object
#'
#' @return An vector with the file block size.
.raster_file_blocksize <- function(rast) {
    block_size <- c(terra::fileBlocksize(rast[[1]]))
    names(block_size) <- c("nrows", "ncols")
    return(block_size)
}

#' @title Raster package internal object creation
#' @name .raster_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast    raster package object to be cloned
#' @param nlayers  number of raster layers
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Raster package object
.raster_rast <- function(rast, nlayers = 1, ...) {
    suppressWarnings(
        terra::rast(x = rast, nlyrs = nlayers, ...)
    )
}
#' @title Raster package internal open vector function
#' @name .raster_open_vect
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param sf_object    sf_object to convert to a SpatVector
#'
#' @return Raster package object
.raster_open_vect <- function(sf_object) {
    # set caller to show in errors
    .check_set_caller(".raster_open_vect")
    terra::vect(sf_object)
}
#' @title Raster package internal open raster function
#' @name .raster_open_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param file    raster file to be opened
#' @param ...     additional parameters to be passed to raster package
#'
#' @return Raster package object
.raster_open_rast <- function(file, ...) {
    # set caller to show in errors
    .check_set_caller(".raster_open_rast")
    rast <- suppressWarnings(
        terra::rast(x = .file_path_expand(file), ...)
    )
    .check_null_parameter(rast)
    # remove gain and offset applied by terra
    terra::scoff(rast) <- NULL
    rast
}

#' @title Raster package internal write raster file function
#' @name .raster_write_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast         raster package object to be written
#' @param file          file path to save raster file
#' @param format        GDAL file format string (e.g. GTiff)
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param overwrite     logical indicating if file can be overwritten
#' @param ...           additional parameters to be passed to raster package
#' @param missing_value A \code{integer} with image's missing value
#'
#' @return              No value, called for side effects.
.raster_write_rast <- function(rast,
                               file,
                               data_type,
                               overwrite, ...,
                               missing_value = NA) {
    # set caller to show in errors
    .check_set_caller(".raster_write_rast_terra")

    suppressWarnings(
        terra::writeRaster(
            x = rast,
            filename = path.expand(file),
            wopt = list(
                filetype = "GTiff",
                datatype = data_type,
                gdal = .conf("gdal_creation_options")
            ),
            NAflag = missing_value,
            overwrite = overwrite, ...
        )
    )
    # was the file written correctly?
    .check_file(file)
    return(invisible(rast))
}

#' @title Raster package internal create raster object function
#' @name .raster_new_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param nrows         Number of rows in the raster
#' @param ncols         Number of columns in the raster
#' @param xmin          X minimum of raster origin
#' @param xmax          X maximum of raster origin
#' @param ymin          Y minimum of raster origin
#' @param ymax          Y maximum of raster origin
#' @param nlayers       Number of layers of the raster
#' @param crs           Coordinate Reference System of the raster
#' @param xres          X resolution
#' @param yres          Y resolution
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
                             crs,
                             xres = NULL,
                             yres = NULL,
                             ...) {
    # prepare resolution
    resolution <- c(xres, yres)
    # prepare crs
    if (is.numeric(crs)) crs <- paste0("EPSG:", crs)
    # create new raster object if resolution is not provided
    if (is.null(resolution)) {
        # create a raster object
        rast <- suppressWarnings(
            terra::rast(
                nrows = nrows,
                ncols = ncols,
                nlyrs = nlayers,
                xmin  = xmin,
                xmax  = xmax,
                ymin  = ymin,
                ymax  = ymax,
                crs   = crs
            )
        )
    } else {
        # create a raster object
        rast <- suppressWarnings(
            terra::rast(
                nlyrs = nlayers,
                xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax,
                crs = crs,
                resolution = resolution
            )
        )
    }
    return(rast)
}

#' @title Raster package internal read raster file function
#' @name .raster_read_rast
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
    # create raster objects
    rast <- .raster_open_rast(file = path.expand(files), ...)

    # start read
    if (.has_not(block)) {
        # read values
        terra::readStart(rast)
        values <- terra::readValues(
            x   = rast,
            mat = TRUE
        )
        # close file descriptor
        terra::readStop(rast)
    } else {
        # read values
        terra::readStart(rast)
        values <- terra::readValues(
            x = rast,
            row = block[["row"]],
            nrows = block[["nrows"]],
            col = block[["col"]],
            ncols = block[["ncols"]],
            mat = TRUE
        )
        # close file descriptor
        terra::readStop(rast)
    }
    return(values)
}

#' @title Raster package internal crop raster function
#' @name .raster_crop
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast         Raster package object to be written
#' @param file          File name to save cropped raster.
#' @param data_type     sits internal raster data type. One of "INT1U",
#'                      "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S".
#' @param overwrite     logical indicating if file can be overwritten
#' @param mask         a valid block with (\code{col}, \code{row},
#'                      \code{ncols}, \code{nrows}).
#' @param missing_value A \code{integer} with image's missing value
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
.raster_crop <- function(rast,
                         file,
                         data_type,
                         overwrite,
                         mask,
                         missing_value = NA) {
    # pre-condition
    .check_null_parameter(mask)
    # check block
    if (.has_block(mask)) {
        .raster_check_block(block = mask)
    }
    # Update missing_value
    missing_value <- if (is.null(missing_value)) NA else missing_value
    # obtain coordinates from columns and rows
    # get extent
    if (.has_block(mask)) {
        xmin <- terra::xFromCol(
            object = rast,
            col = mask[["col"]]
        )
        xmax <- terra::xFromCol(
            object = rast,
            col = mask[["col"]] + mask[["ncols"]] - 1
        )
        ymax <- terra::yFromRow(
            object = rast,
            row = mask[["row"]]
        )
        ymin <- terra::yFromRow(
            object = rast,
            row = mask[["row"]] + mask[["nrows"]] - 1
        )

        # xmin, xmax, ymin, ymax
        extent <- c(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
        )
        mask <- .roi_as_sf(extent, default_crs = terra::crs(rast))
    }
    # in case of sf with another crs
    mask <- .roi_as_sf(mask, as_crs = terra::crs(rast))

    # crop raster
    suppressWarnings(
        terra::mask(
            x = rast,
            mask = terra::vect(mask),
            filename = path.expand(file),
            wopt = list(
                filetype = "GTiff",
                datatype = data_type,
                gdal = .conf("gdal_creation_options")
            ),
            NAflag = missing_value,
            overwrite = overwrite
        )
    )
}

#' @title Raster package internal crop raster function
#' @name .raster_crop_metadata
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast   raster package object to be written
#' @param block   a valid block with (\code{col}, \code{row},
#'                \code{ncols}, \code{nrows}).
#' @param bbox    numeric vector with (xmin, xmax, ymin, ymax).
#' @param ...     additional parameters to be passed to raster package
#'
#' @note block starts at (1, 1)
#'
#' @return        Subset of a raster object as defined by either block
#'                or bbox parameters
.raster_crop_metadata <- function(rast, ..., block = NULL, bbox = NULL) {
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
    # obtain coordinates from columns and rows
    if (!is.null(block)) {
        # get extent
        xmin <- terra::xFromCol(
            object = rast,
            col = block[["col"]]
        )
        xmax <- terra::xFromCol(
            object = rast,
            col = block[["col"]] + block[["ncols"]] - 1
        )
        ymax <- terra::yFromRow(
            object = rast,
            row = block[["row"]]
        )
        ymin <- terra::yFromRow(
            object = rast,
            row = block[["row"]] + block[["nrows"]] - 1
        )
    } else if (!is.null(bbox)) {
        xmin <- bbox[["xmin"]]
        xmax <- bbox[["xmax"]]
        ymin <- bbox[["ymin"]]
        ymax <- bbox[["ymax"]]
    }

    # xmin, xmax, ymin, ymax
    extent <- terra::ext(x = c(xmin, xmax, ymin, ymax))

    # crop raster
    suppressWarnings(
        terra::crop(x = rast, y = extent, snap = "out")
    )
}
#' @title Raster package project function
#' @name .raster_project
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast    raster package object
#' @param crs      CRS to project to
#' @param ...     Other parameters to be passed to terra
#'
#' @return projected raster
.raster_project <- function(rast, crs, ...) {
    terra::project(x = rast, y = crs, ...)
}
#' @title Return number of rows in a raster
#' @name .raster_nrows
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#'
#' @return number of rows in raster object
.raster_nrows <- function(rast, ...) {
    terra::nrow(x = rast)
}

#' @title Return number of columns in a raster
#' @name .raster_ncols
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         number of columns in a raster object
.raster_ncols <- function(rast, ...) {
    terra::ncol(x = rast)
}
#' @title Return number of layers in a raster
#' @name .raster_nlayers
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         number of layers in a raster object
.raster_nlayers <- function(rast, ...) {
    terra::nlyr(x = rast)
}
#' @name .raster_xmax
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         maximum x coord of raster object
.raster_xmax <- function(rast, ...) {
    terra::xmax(x = rast)
}

#' @name .raster_xmin
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         minimum x coord of raster object
.raster_xmin <- function(rast, ...) {
    terra::xmin(x = rast)
}
#' @name .raster_ymax
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         maximum y coord of raster object
.raster_ymax <- function(rast, ...) {
    terra::ymax(x = rast)
}

#' @name .raster_ymin
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         minimum y coord of raster object
.raster_ymin <- function(rast, ...) {
    terra::ymin(x = rast)
}
#' @name .raster_xres
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         resolution of raster object in x direction
.raster_xres <- function(rast, ...) {
    terra::xres(x = rast)
}
#' @name .raster_yres
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         resolution of raster object in y direction
.raster_yres <- function(rast, ...) {
    terra::yres(x = rast)
}
#' @name .raster_scale
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         scale of values in raster object
.raster_scale <- function(rast, ...) {
    # check value
    i <- 1
    while (is.na(rast[i])) {
        i <- i + 1
    }
    value <- rast[i]
    if (value > 1.0 && value <= 10000)
        scale_factor <- 0.0001
    else
        scale_factor <- 1.0
    return(scale_factor)
}
#' @name .raster_crs
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         crs of raster object
.raster_crs <- function(rast, ...) {
    crs <- suppressWarnings(
        terra::crs(x = rast, describe = TRUE)
    )
    if (!is.na(crs[["code"]])) {
        return(paste(crs[["authority"]], crs[["code"]], sep = ":"))
    }
    suppressWarnings(
        as.character(terra::crs(x = rast))
    )
}
#' @name .raster_bbox
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         bounding box of raster object
.raster_bbox <- function(rast, ...,
                         block = NULL) {
    if (is.null(block)) {
        # return a named bbox
        bbox <- c(
            xmin = .raster_xmin(rast),
            ymin = .raster_ymin(rast),
            xmax = .raster_xmax(rast),
            ymax = .raster_ymax(rast)
        )
    } else {
        r_crop <- .raster_crop_metadata(
            .raster_rast(rast = rast),
            block = block
        )
        bbox <- .raster_bbox(r_crop)
    }

    return(bbox)
}

#' @name .raster_res
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         resolution of raster object in x and y dimensions
.raster_res <- function(rast, ...) {
    # return a named resolution
    res <- list(
        xres = .raster_xres(rast),
        yres = .raster_yres(rast)
    )

    return(res)
}
#' @name .raster_extent_bbox
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param xmin,xmax,ymin,ymax numeric vector with bounding box
#' @return        a Spatial Extent object
.raster_extent_bbox <- function(xmin, xmax, ymin, ymax) {
    # return a Spatial Extent
    terra::ext(xmin, xmax, ymin, ymax)
}
#' @name .raster_extent_rast
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    a Spatial Raster object
#' @return        a Spatial Extent object
.raster_extent_rast <- function(rast) {
    # return a Spatial Extent
    terra::ext(rast)
}
#' @name .raster_size
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @param rast    raster package object
#' @param ...      additional parameters to be passed to raster package
#' @return         number of rows and cols of raster object
.raster_size <- function(rast, ...) {
    # return a named size
    size <- list(
        nrows = .raster_nrows(rast),
        ncols = .raster_ncols(rast)
    )

    return(size)
}
#' @title Raster package internal frequency values function
#' @name .raster_freq
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast    raster package object to count values
#' @param ...      additional parameters to be passed to raster package
#'
#' @return matrix with layer, value, and count columns
.raster_freq <- function(rast, ...) {
    terra::freq(x = rast, bylayer = TRUE)
}

#' @title Raster package internal raster data type
#' @name .raster_datatype
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param rast    raster package object
#' @param by_layer  A logical value indicating the type of return
#' @param ...      additional parameters to be passed to raster package
#'
#' @return A character value with data type
.raster_datatype <- function(rast, ..., by_layer = TRUE) {
    terra::datatype(x = rast, bylyr = by_layer)
}

#' @title Raster package internal summary values function
#' @name .raster_summary
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param rast    raster package object to count values
#' @param ...      additional parameters to be passed to raster package
#'
#' @return matrix with layer, value, and count columns
.raster_summary <- function(rast, ...) {
    terra::summary(rast, ...)
}

#' @title Return col value given an X coordinate
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast  raster package object
#' @param x      X coordinate in raster projection
#'
#' @return integer with column
.raster_col <- function(rast, x) {
    terra::colFromX(rast, x)
}
#' @title Return cell value given row and col
#' @name .raster_cell_from_rowcol
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast  raster package object
#' @param row    row
#' @param col    col
#'
#' @return cell
.raster_cell_from_rowcol <- function(rast, row, col) {
    terra::cellFromRowCol(rast, row, col)
}
#' @title Return XY values given a cell
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast  raster package object
#' @param cell   cell in raster object
#' @return       matrix of x and y coordinates
.raster_xy_from_cell <- function(rast, cell){
    terra::xyFromCell(rast, cell)
}
#' @title Return quantile value given an raster
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast    raster package object
#' @param quantile quantile value
#' @param na.rm    Remove NA values?
#' @param ...      additional parameters
#'
#' @return numeric values representing raster quantile.
.raster_quantile <- function(rast, quantile, na.rm = TRUE, ...) {
    terra::global(rast, fun = terra::quantile, probs = quantile, na.rm = na.rm)
}

#' @title Return row value given an Y coordinate
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param rast  raster object
#' @param y      Y coordinate in raster projection
#'
#' @return integer with row number
.raster_row <- function(rast, y) {
    terra::rowFromY(rast, y)
}
#' @title Raster-to-vector
#' @name .raster_extract_polygons
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param rast terra raster object
#' @param dissolve should the polygons be dissolved?
#' @return A set of polygons
.raster_extract_polygons <- function(rast, dissolve = TRUE, ...) {
    terra::as.polygons(rast, dissolve = TRUE, ...)
}

#' @title Determine the file params to write in the metadata
#' @name .raster_params_file
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
    rast <- .raster_open_rast(file = file)
    # build params file
    params <- tibble::tibble(
        nrows = .raster_nrows(rast = rast),
        ncols = .raster_ncols(rast = rast),
        xmin  = .raster_xmin(rast = rast),
        xmax  = .raster_xmax(rast = rast),
        ymin  = .raster_ymin(rast = rast),
        ymax  = .raster_ymax(rast = rast),
        xres  = .raster_xres(rast = rast),
        yres  = .raster_yres(rast = rast),
        crs   = .raster_crs(rast = rast)
    )
    return(params)
}
#' @title Template for creating a new raster
#' @name .raster_template
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @param base_file     File to use for template
#' @param out_file      Name of output file
#' @param nlayers       Number of layers in output file
#' @param data_type     Data type of output
#' @param missing_value Missing values in output file
#'
#' @return name of output file

.raster_template <- function(base_file, out_file, nlayers, data_type,
                             missing_value) {
    # Create an empty image template
    .gdal_translate(
        file = .file_path_expand(out_file),
        base_file = .file_path_expand(base_file),
        params = list(
            "-ot" = .raster_gdal_datatype(data_type),
            "-of" = .conf("gdal_presets", "image", "of"),
            "-b" = rep(1, nlayers),
            "-scale" = list(0, 1, missing_value, missing_value),
            "-a_nodata" = missing_value,
            "-co" = .conf("gdal_creation_options")
        ),
        quiet = TRUE
    )
    # Delete auxiliary files
    on.exit(unlink(paste0(out_file, ".aux.xml")), add = TRUE)
    return(out_file)
}

#' @title Merge all input files into one raster file
#' @name .raster_merge
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
        out_file <- .file_path_expand(out_files[[i]])
        # Check if out_file does not exist
        .check_that(!file.exists(out_file))
        # Get file paths
        merge_files <- purrr::map_chr(block_files, `[[`, i)
        # Expand paths for block_files
        merge_files <- .file_path_expand(merge_files)
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
                        .gdal_warp(
                            file = out_file,
                            base_files = merge_files,
                            params = list(
                                "-wo" = paste0("NUM_THREADS=", multicores),
                                "-co" = .conf("gdal_creation_options"),
                                "-multi" = TRUE,
                                "-overwrite" = TRUE
                            ),
                            quiet = TRUE
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
                        .gdal_warp(
                            file = out_file,
                            base_files = merge_files,
                            params = list(
                                "-wo" = paste0("NUM_THREADS=", multicores),
                                "-ot" = .raster_gdal_datatype(data_type),
                                "-multi" = TRUE,
                                "-of" = .conf("gdal_presets", "image", "of"),
                                "-co" = .conf("gdal_creation_options"),
                                "-overwrite" = FALSE
                            ),
                            quiet = TRUE
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
#' @title Clone an existing raster
#' @name .raster_clone
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @param file          Raster file to use for template
#' @param nlayers       Number of layers in output file
#'
#' @return cloned raster object
#'
.raster_clone <- function(file, nlayers = NULL) {
    rast <- .raster_open_rast(file = file)

    if (is.null(nlayers)) {
        nlayers <- .raster_nlayers(rast = rast)
    }
    rast <- .raster_rast(rast = rast, nlayers = nlayers, vals = NA)

    return(rast)
}
#' @title Check if raster is valid
#' @name .raster_is_valid
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @param files         Raster files
#' @param output_dir    Output file
#'
#' @return boolean value
#'
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
    rast <- .try(
        {
            .raster_open_rast(files)
        },
        .default = {
            unlink(files)
            NULL
        }
    )
    # File is not valid
    if (is.null(rast)) {
        return(FALSE)
    }
    # if file can be opened, check if the result is correct
    # this file will not be processed again
    # Verify if the raster is corrupted
    check <- .try(
        {
            rast[.raster_ncols(rast) * .raster_nrows(rast)]
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
#' @title Write block of raster
#' @name .raster_write_block
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @param files         Raster files to written
#' @param block         Block to be written to file
#' @param bbox          Bounding box of block
#' @param values        Values to be written
#' @param data_type     Data type of output
#' @param missing_value Missing value for output file
#' @param crop_block    Cropped area of block
#'
#' @return file paths
#'
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
        rast <- .raster_new_rast(
            nrows = block[["nrows"]], ncols = block[["ncols"]],
            xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]], ymax = bbox[["ymax"]],
            nlayers = nlayers, crs = bbox[["crs"]]
        )
        # Copy values
        rast <- .raster_set_values(
            rast = rast,
            values = values[, cols]
        )
        # If no crop_block provided write the probabilities to a raster file
        if (is.null(crop_block)) {
            .raster_write_rast(
                rast = rast, file = file, data_type = data_type,
                overwrite = TRUE, missing_value = missing_value
            )
        } else {
            # Crop removing overlaps
            .raster_crop(
                rast = rast, file = file, data_type = data_type,
                overwrite = TRUE, mask = crop_block,
                missing_value = missing_value
            )
        }
    }
    # Return file path
    files
}
#' @title  Prepare raster for RGB visualization
#' @name .raster_view_rgb_object
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  red_file      Image file to be shown in red color
#' @param  green_file    Image file to be shown in green color
#' @param  blue_file     Image file to be shown in blue color
#' @param  band_conf     Band configuration file
#' @return               A Spatial Raster object
#
.raster_view_rgb_object <- function(red_file, green_file, blue_file, band_conf){
    rgb_files <- c(r = red_file, g = green_file, b = blue_file)
    rast <- .raster_open_rast(rgb_files)

    # resample and warp the image
    rast <- .raster_project(
        rast = rast,
        crs = "EPSG:3857"
    )
    # get scale and offset
    band_scale <- .scale(band_conf)
    band_offset <- .offset(band_conf)

    # scale the data
    rast <- (rast * band_scale + band_offset) * 255

    # # stretch the raster
    rast <- .raster_stretch(rast, minv = 0, maxv = 255,
                            minq = 0.05, maxq = 0.95)
    # convert to RGB
    names(rast) <- c("red", "green", "blue")
    terra::RGB(rast) <- c(1,2,3)
    .raster_set_minmax(rast)
    return(rast)
}
