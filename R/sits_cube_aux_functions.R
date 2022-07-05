#' @title Creates the description of a data cube
#' @name .cube_create
#' @keywords internal
#'
#' @description Print information and save metadata about a data cube.
#'
#' @param source      Source of data
#' @param collection  Image collection
#' @param satellite   Name of satellite
#' @param sensor      Name of sensor
#' @param tile        Tile of the image collection
#' @param xmin        Spatial extent (xmin).
#' @param ymin        Spatial extent (ymin).
#' @param xmax        Spatial extent (xmax).
#' @param ymax        Spatial extent (ymin).
#' @param crs         CRS for cube (EPSG code or PROJ4 string).
#' @param file_info   Tibble with information about files
#'
#' @return  A tibble containing a data cube
#'
.cube_create <- function(source,
                         collection = NA_character_,
                         satellite,
                         sensor,
                         tile = NA_character_,
                         xmin,
                         xmax,
                         ymin,
                         ymax,
                         crs,
                         labels = NULL,
                         file_info = NULL) {


    # create a tibble to store the metadata (mandatory parameters)
    cube <- tibble::tibble(
        source = source,
        collection = collection,
        satellite = satellite,
        sensor = sensor,
        tile = tile,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        crs = crs
    )

    # if there are labels, include them
    if (!purrr::is_null(labels)) {
        cube <- tibble::add_column(cube, labels = list(labels))
    }

    if (!purrr::is_null(file_info)) {
        cube <- tibble::add_column(cube, file_info = list(file_info))
    }

    class(cube) <- unique(c("sits_cube", class(cube)))

    return(cube)
}


#' @title Given a labelled cube, return the band information
#' @name .cube_area_freq
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param cube   Data cube
#'
#' @return Frequency of each label in the data cube
#'
.cube_area_freq <- function(cube) {

    # set caller to show in errors
    .check_set_caller(".cube_area_freq")

    # pre-condition - one tile at a time
    .check_num(nrow(cube),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile at a time"
    )

    # precondition
    .check_that(
        x = inherits(cube, "classified_image"),
        msg = "requires a labelled cube"
    )

    # open first raster
    r_obj <- .raster_open_rast(.file_info_path(cube))

    # retrieve the frequency
    freq <- tibble::as_tibble(.raster_freq(r_obj))

    return(freq)
}

#' @title Return bands of a data cube
#' @keywords internal
#' @name .cube_bands
#' @param cube       Data cube
#' @param add_cloud  Include the cloud band?
#'
#' @return A \code{vector} with the cube bands.
.cube_bands <- function(cube, add_cloud = TRUE) {
    bands <- sits_bands(cube)

    # post-condition
    .check_chr(bands,
        min_len = 1, is_named = FALSE,
        msg = "inconsistent 'bands' information"
    )

    if (!add_cloud) {
        bands <- bands[bands != .source_cloud()]
    }

    return(bands)
}
#' @title Check if bands are part of a data cube
#' @keywords internal
#' @name .cube_bands_check
#' @param cube          Data cube
#' @param bands         Bands to be check
#' @param add_cloud     Include the cloud band?
#'
#' @return No return value, called for side effects.
#' @rdname cube_functions
#'
.cube_bands_check <- function(cube, bands, add_cloud = TRUE) {

    # all bands are upper case
    .check_chr_within(bands,
        within = .cube_bands(cube = cube, add_cloud = add_cloud),
        case_sensitive = FALSE,
        msg = "invalid 'bands' parameter"
    )
}


#' @title Missing value of a cube band
#' @keywords internal
#' @name .cube_band_missing_value
#' @param cube  Data cube
#' @param band  Band
#'
#' @return The missing value for the band.
#'
.cube_band_missing_value <- function(cube, band) {

    # pre-condition
    .check_chr(band,
        len_min = 1, len_max = 1,
        msg = "invalid 'band' parameter"
    )

    .check_chr_within(band,
        within = .cube_bands(cube = cube, add_cloud = FALSE),
        discriminator = "one_of",
        case_sensitive = FALSE,
        msg = "invalid 'band' parameter"
    )

    # bands names are upper case
    band <- toupper(band)

    mv <- .config_get(
        key = c(
            "sources", .cube_source(cube = cube),
            "collections", .cube_collection(cube = cube),
            "bands", band, "missing_value"
        ),
        default = .config_get(key = "raster_cube_missing_value")
    )

    # post-condition
    .check_num(mv,
        len_min = 1, len_max = 1,
        msg = "invalid 'missing_value' value"
    )

    return(mv)
}

#' @title Minimum value of a cube band
#' @keywords internal
#' @name .cube_band_minimum_value
#' @param cube  Data cube
#' @param band  Band
#'
#' @return Minimum value for the band in the data cube
#'
.cube_band_minimum_value <- function(cube, band) {

    # pre-condition
    .check_chr(band,
        len_min = 1, len_max = 1,
        msg = "invalid 'band' parameter"
    )

    .check_chr_within(band,
        within = .cube_bands(cube = cube, add_cloud = FALSE),
        discriminator = "one_of",
        case_sensitive = FALSE,
        msg = "invalid 'band' parameter"
    )

    # bands names are upper case
    band <- toupper(band)

    mv <- .config_get(
        key = c(
            "sources", .cube_source(cube = cube),
            "collections", .cube_collection(cube = cube),
            "bands", band, "minimum_value"
        ),
        default = .config_get(key = "raster_cube_minimum_value")
    )

    # post-condition
    .check_num(mv,
        len_min = 1, len_max = 1,
        msg = "invalid 'minimum_value' value"
    )

    return(mv)
}

#' @title Maximum value of a cube band
#' @keywords internal
#' @name .cube_band_maximum_value
#' @param cube  Data cube
#' @param band  Band
#'
#' @return Maximum value for the band in the data cube.
#'
.cube_band_maximum_value <- function(cube, band) {

    # pre-condition
    .check_chr(band,
        len_min = 1, len_max = 1,
        msg = "invalid 'band' parameter"
    )

    .check_chr_within(band,
        within = .cube_bands(cube = cube, add_cloud = FALSE),
        discriminator = "one_of",
        case_sensitive = FALSE,
        msg = "invalid 'band' parameter"
    )

    # bands names are upper case
    band <- toupper(band)

    mv <- .config_get(
        key = c(
            "sources", .cube_source(cube = cube),
            "collections", .cube_collection(cube = cube),
            "bands", band, "maximum_value"
        ),
        default = .config_get(key = "raster_cube_maximum_value")
    )

    # post-condition
    .check_num(mv,
        len_min = 1, len_max = 1,
        msg = "invalid 'maximum_value' value"
    )

    return(mv)
}
#' @title Scale factor of a cube band
#' @keywords internal
#' @name .cube_band_scale_factor
#'
#' @param cube  Data cube
#' @param band  Band
#'
#' @return Scale factor for the band in the data cube.
#'
.cube_band_scale_factor <- function(cube, band) {

    # pre-condition
    .check_chr(band,
        len_min = 1, len_max = 1,
        msg = "invalid 'band' parameter"
    )

    .check_chr_within(band,
        within = .cube_bands(cube = cube, add_cloud = FALSE),
        discriminator = "one_of",
        case_sensitive = FALSE,
        msg = "invalid 'band' parameter"
    )
    # bands names are upper case
    band <- toupper(band)

    sf <- .config_get(
        key = c(
            "sources", .cube_source(cube = cube),
            "collections", .cube_collection(cube = cube),
            "bands", band, "scale_factor"
        ),
        default = .config_get(key = "raster_cube_scale_factor")
    )

    # post-condition
    .check_num(
        x = sf,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'scale_factor' value"
    )

    return(sf)
}

#' @title Offset value of a cube band
#' @keywords internal
#' @name .cube_band_offset_value
#' @param cube  Data cube
#' @param band  Band
#' @return offset value for the band in the data cube
#'
.cube_band_offset_value <- function(cube, band) {

    # pre-condition
    .check_chr(band,
        len_min = 1, len_max = 1,
        msg = "invalid 'band' parameter"
    )

    .check_chr_within(band,
        within = .cube_bands(cube = cube, add_cloud = FALSE),
        discriminator = "one_of",
        case_sensitive = FALSE,
        msg = "invalid 'band' parameter"
    )

    # bands names are upper case
    band <- toupper(band)

    ov <- .config_get(
        key = c(
            "sources", .cube_source(cube = cube),
            "collections", .cube_collection(cube = cube),
            "bands", band, "offset_value"
        ),
        default = .config_get(key = "raster_cube_offset_value")
    )

    # post-condition
    .check_num(ov,
        len_min = 1, len_max = 1,
        msg = "invalid 'offset_value' value"
    )

    return(ov)
}


#' @title Check if R object is a data cube
#' @keywords internal
#' @name .cube_check
#' @param cube  R object to be checked
#' @return TRUE/FALSE
.cube_check <- function(cube) {
    return(inherits(cube, "sits_cube"))
}

#' @title Return collection of a data cube
#' @keywords internal
#' @name .cube_collection
#' @param cube  data cube
#' @return collection associated to the cube
.cube_collection <- function(cube) {
    col <- unique(cube[["collection"]])

    # post-condition
    .check_chr(col,
        allow_empty = FALSE, len_min = 1, len_max = 1,
        msg = "invalid collection value"
    )

    return(col)
}
#' @title Return crs of a data cube
#' @keywords internal
#' @name .cube_crs
#' @param cube  data cube
#' @return crs associated to the cube
.cube_crs <- function(cube) {
    crs <- unique(cube[["crs"]])

    # post-condition
    .check_chr(crs,
        allow_empty = FALSE, allow_NA = FALSE,
        len_min = 1, len_max = 1,
        msg = "invalid crs value"
    )

    return(crs)
}
#' @title Create a data cube derived from another
#' @name .cube_derived_create
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Take a tibble containing metadata about a data cube
#' containing time series and create a
#' set of files to store the result of a classification or smoothing
#'
#' @param cube         input data cube
#' @param cube_class   class to be attributed to created cube
#' @param band_name    name of band in created cube
#' @param labels       labels of derived cube
#' @param start_date   start date of the cube interval
#' @param end_date     end date of the cube interval
#' @param bbox         bounding box of the ROI
#' @param output_dir   prefix of the output files.
#' @param version      version of the output files
#'
#' @return output data cube
#'
.cube_derived_create <- function(cube,
                                 cube_class,
                                 band_name,
                                 labels,
                                 start_date,
                                 end_date,
                                 bbox,
                                 output_dir,
                                 version) {

    # set caller to show in errors
    .check_set_caller(".cube_derived_create")

    # ensure metadata tibble exists
    .check_that(
        x = nrow(cube) == 1,
        msg = "accepts only one tile at a time"
    )

    # output filename
    file_name <- paste0(
        cube[["satellite"]], "_",
        cube[["sensor"]], "_",
        cube[["tile"]], "_",
        start_date, "_",
        end_date, "_",
        band_name, "_",
        version, ".tif"
    )

    file_name <- file.path(output_dir, file_name)

    res <- .cube_resolution(cube)

    if (!purrr::is_null(bbox)) {
        sub_image <- .sits_raster_sub_image(tile = cube, roi = bbox)
        nrows_cube_class <- sub_image[["nrows"]]
        ncols_cube_class <- sub_image[["ncols"]]
    } else {
        nrows_cube_class <- .file_info_nrows(cube)
        ncols_cube_class <- .file_info_ncols(cube)
    }

    # set the file information
    file_info <- tibble::tibble(
        band       = band_name,
        start_date = start_date,
        end_date   = end_date,
        xmin       = bbox[["xmin"]],
        ymin       = bbox[["ymin"]],
        xmax       = bbox[["xmax"]],
        ymax       = bbox[["ymax"]],
        xres       = res[["xres"]],
        yres       = res[["yres"]],
        nrows      = nrows_cube_class,
        ncols      = ncols_cube_class,
        path       = file_name
    )

    # set the metadata for the probability cube
    dev_cube <- .cube_create(
        source     = .cube_source(cube),
        collection = .cube_collection(cube),
        satellite  = cube$satellite,
        sensor     = cube$sensor,
        tile       = cube$tile,
        xmin       = bbox[["xmin"]],
        xmax       = bbox[["xmax"]],
        ymin       = bbox[["ymin"]],
        ymax       = bbox[["ymax"]],
        crs        = cube$crs,
        labels     = labels,
        file_info  = file_info
    )

    class(dev_cube) <- unique(c(cube_class, "raster_cube", class(dev_cube)))

    return(dev_cube)
}

#' @title Given a band, return a set of values for chosen location
#' @name .cube_extract
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a data cube, retrieve the time series of XY locations
#'
#' @param cube        Metadata about a data cube
#' @param band_cube   Name of the band to the retrieved
#' @param xy          Matrix with XY location
#'
#' @return Numeric matrix with raster values for each coordinate.
#'
.cube_extract <- function(cube, band_cube, xy) {


    # set caller to show in errors
    .check_set_caller(".cube_extract")

    # pre-condition - one tile at a time
    .check_num(nrow(cube),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile at a time"
    )

    # precondition 2
    .check_chr_within(
        x = band_cube,
        within = sits_bands(cube),
        discriminator = "one_of",
        msg = paste(
            "band", band_cube,
            "is not available in the cube"
        )
    )

    # filter the files that contain the band
    band <- dplyr::filter(.file_info(cube), .data[["band"]] == band_cube)

    # create a stack object
    r_obj <- .raster_open_stack(band$path)

    # extract the values
    values <- .raster_extract(r_obj, xy)

    # is the data valid?
    .check_that(
        x = nrow(values) == nrow(xy),
        msg = "error in retrieving data"
    )
    return(values)
}

#' @title Verify if two cubes are equal
#'
#' @name .cube_is_equal
#'
#' @keywords internal
#'
#' @description Given two cubes verify if they are equal
#'
#' @param x,y   a sits cube
#'
#' @return a \code{logical} value.
.cube_is_equal <- function(x, y) {
    if (nrow(x) != nrow(y)) {
        return(FALSE)
    }

    slider::slide2_lgl(x, y, function(xtile, ytile) {
        test_metadata <- isTRUE(dplyr::all_equal(
            dplyr::select(xtile, -.data[["file_info"]], -.data[["crs"]]),
            dplyr::select(ytile, -.data[["file_info"]], -.data[["crs"]])
        ))

        test_file_info <- isTRUE(dplyr::all_equal(
            xtile[["file_info"]][[1]],
            ytile[["file_info"]][[1]]
        ))

        test_crs <- sf::st_crs(xtile[["crs"]]) == sf::st_crs(ytile[["crs"]])

        return(all(c(test_metadata, test_file_info, test_crs)))
    })
}

#' @title Check if cube is regular
#' @name .cube_is_regular
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_is_regular <- function(cube) {
    source <- .source_new(source = .cube_source(cube))

    # Dispatch
    UseMethod(".cube_is_regular", source)
}

#' @name .cube_is_regular
#' @export
.cube_is_regular.raster_cube <- function(cube) {
    if (!.cube_unique_bands(cube)) {
        return(FALSE)
    }

    if (!.cube_unique_bbox(cube)) {
        return(FALSE)
    }

    if (!.cube_unique_tile_size(cube)) {
        return(FALSE)
    }

    if (!.cube_unique_timeline(cube)) {
        return(FALSE)
    }

    return(TRUE)
}

#' @name .cube_is_regular
#' @export
.cube_is_regular.default <- function(cube) {
    return(TRUE)
}

#' @title Check if the bands of all tiles of the cube are the same
#' @name .cube_unique_bands
#' @keywords internal
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_unique_bands <- function(cube) {
    # check if all tiles have the same bands
    bands <- slider::slide(cube, function(tile) {
        return(.cube_bands(tile))
    })
    if (length(unique(bands)) != 1) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
#' @title Check if bboxes of all tiles of the cube are the same
#' @name .cube_unique_bbox
#' @keywords internal
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_unique_bbox <- function(cube) {
    tolerance <- .config_get(
        key = c(
            "sources", .cube_source(cube),
            "collections", .cube_collection(cube),
            "ext_tolerance"
        )
    )

    # check if the resolutions are unique
    equal_bbox <- slider::slide_lgl(cube, function(tile) {
        file_info <- .file_info(tile)

        test <-
            (.is_eq(max(file_info[["xmax"]]),
                min(file_info[["xmax"]]),
                tolerance = tolerance
            ) &&
                .is_eq(max(file_info[["xmin"]]),
                    min(file_info[["xmin"]]),
                    tolerance = tolerance
                ) &&
                .is_eq(max(file_info[["ymin"]]),
                    min(file_info[["ymin"]]),
                    tolerance = tolerance
                ) &&
                .is_eq(max(file_info[["ymax"]]),
                    min(file_info[["ymax"]]),
                    tolerance = tolerance
                ))

        return(test)
    })

    if (!all(equal_bbox)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
#' @title Check if sizes of all tiles of the cube are the same
#' @name .cube_unique_tile_size
#' @keywords internal
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_unique_tile_size <- function(cube) {
    # check if the sizes of all tiles are the same
    test_cube_size <- slider::slide_lgl(cube, function(tile) {
        if (length(unique(.file_info(tile)[["nrows"]])) > 1 ||
            length(unique(.file_info(tile)[["ncols"]])) > 1) {
            return(FALSE)
        }
        return(TRUE)
    })

    if (!all(test_cube_size)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' @title Check if timelines all tiles of the cube are the same
#' @name .cube_unique_timeline
#' @keywords internal
#' @param  cube         input data cube
#' @return TRUE/FALSE
.cube_unique_timeline <- function(cube) {
    # get the bands
    bands <- slider::slide(cube, function(tile) {
        return(.cube_bands(tile))
    })
    # check if timelines are unique
    timelines <- slider::slide(cube, function(tile) {
        unique(purrr::map(unlist(unique(bands)), function(band) {
            tile_band <- sits_select(tile, bands = band)
            sits_timeline(tile_band)
        }))
    })

    # function to test timelines
    return(length(unique(timelines)) == 1 &&
        any(purrr::map_dbl(timelines, length) == 1))
}

#' @title Return the labels of the cube
#' @name .cube_labels
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return a vector with the labels of the cube
.cube_labels <- function(cube) {
    labs <- unique(cube[["labels"]])

    # post-condition
    .check_lst(labs,
        min_len = 1, max_len = 1,
        is_named = FALSE,
        msg = "invalid 'labels' value"
    )

    labs <- unlist(labs, use.names = FALSE)

    # post-condition
    .check_chr(labs,
        allow_na = TRUE, allow_empty = FALSE,
        len_min = 1, allow_null = TRUE,
        msg = "invalid 'labels' value"
    )

    return(labs)
}

#' @title Determine the block spatial parameters of a given cube
#' @name .cube_params_block
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on the R object associated to a raster object,
#'                 determine its parameters
#' @param cube     A valid cube
#' @param block    A block inside the cube
#' @return A tibble with the cube parameters
.cube_params_block <- function(cube, block) {

    # pre-condition - one tile at a time
    .check_num(nrow(cube),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile at a time"
    )

    # pre-conditions
    .check_num(block[["first_row"]],
        min = 1, max = .cube_size(cube)[["nrows"]],
        msg = "invalid block value"
    )

    .check_num(block[["first_col"]],
        min = 1, max = .cube_size(cube)[["ncols"]],
        msg = "invalid block value"
    )

    .check_num(block[["nrows"]],
        min = 1, max = .cube_size(cube)[["nrows"]],
        msg = "invalid block value"
    )

    .check_num(block[["ncols"]],
        min = 1, max = .cube_size(cube)[["ncols"]],
        msg = "invalid block value"
    )

    params <- .sits_raster_sub_image_from_block(block = block, tile = cube)

    tolerance <- .config_get(key = c(
        "sources", .cube_source(cube),
        "collections", .cube_collection(cube),
        "ext_tolerance"
    ))

    # post-conditions
    .check_num(params[["xmin"]],
        min = cube[["xmin"]], max = cube[["xmax"]],
        tolerance = tolerance, msg = "invalid params value"
    )

    .check_num(params[["xmax"]],
        min = cube[["xmin"]], max = cube[["xmax"]],
        tolerance = tolerance, msg = "invalid params value"
    )

    .check_num(params[["ymin"]],
        min = cube[["ymin"]], max = cube[["ymax"]],
        tolerance = tolerance, msg = "invalid params value"
    )

    .check_num(params[["ymax"]],
        min = cube[["ymin"]], max = cube[["ymax"]],
        tolerance = tolerance, msg = "invalid params value"
    )

    return(params)
}

#' @title Return the cube resolution of the x axis
#' @name .cube_xres
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return a numeric with the x resolution
.cube_xres <- function(cube, bands = NULL) {

    # tile template
    xres <- .file_info_xres(cube, bands = bands)


    # post-condition
    .check_num(
        x = xres,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'xres' value"
    )

    return(xres)
}

#' @title Return the cube resolution of the y axis
#' @name .cube_yres
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return a numeric with the y resolution
.cube_yres <- function(cube, bands = NULL) {

    # tile template
    yres <- .file_info_yres(cube, bands = bands)

    # post-condition
    .check_num(
        x = yres,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'yres' value"
    )

    return(yres)
}

#' @title Return the resolution of the cube
#' @name .cube_resolution
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return a vector with the x and y resolution
.cube_resolution <- function(cube, bands = NULL) {
    cube <- sits_select(cube, bands = bands)
    return(c(xres = .cube_xres(cube), yres = .cube_yres(cube)))
}

#' @title Return the S3 class of the cube
#' @name .cube_s3class
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return class of the cube
.cube_s3class <- function(cube) {
    unique(c(
        .source_s3class(source = .cube_source(cube = cube)),
        class(cube)
    ))
}

#' @title Return the size of the cube (nrows x ncols)
#' @name .cube_size
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube         input data cube
#' @return a vector with the labels of the cube
.cube_size <- function(cube, ..., bands = NULL) {

    # get the file size
    nrows <- .file_info_nrows(cube, bands = bands)
    ncols <- .file_info_ncols(cube, bands = bands)

    # post-conditions
    .check_num(nrows,
        min = 1, len_min = 1, len_max = 1,
        is_integer = TRUE, msg = "invalid number of rows"
    )

    .check_num(ncols,
        min = 1, len_min = 1, len_max = 1,
        is_integer = TRUE, msg = "invalid number of columns"
    )

    size <- c(nrows = nrows, ncols = ncols)

    return(size)
}
#' @title Get cube source
#' @name .cube_source
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube input data cube
#'
#' @return A character string
.cube_source <- function(cube) {

    # set caller to show in errors
    .check_set_caller(".cube_source")

    src <- unique(cube$source)

    .check_length(
        x = src,
        len_max = 1,
        msg = "cube has different sources."
    )

    return(src)
}

#' @title Get cube tiles
#' @name .cube_tiles
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube input data cube
#'
#' @return A vector with tile names
.cube_tiles <- function(cube) {
    tiles <- unique(cube[["tile"]])

    # post-condition
    .check_chr(tiles,
        allow_empty = FALSE, len_min = nrow(cube),
        len_max = nrow(cube),
        msg = "invalid cube 'tile' values"
    )

    return(tiles)
}

#' @title Get bbox of a cube (single tile)
#' @name .cube_tile_bbox
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube input data cube
#'
#' @return A vector with tile names
.cube_tile_bbox <- function(cube) {

    # pre-condition - one tile at a time
    .check_num(nrow(cube),
        min = 1, max = 1, is_integer = TRUE,
        msg = "process one tile at a time"
    )

    bbox <- vector("double", length = 4)
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")

    bbox["xmin"] <- cube["xmin"]
    bbox["ymin"] <- cube["ymin"]
    bbox["xmax"] <- cube["xmax"]
    bbox["ymax"] <- cube["ymax"]

    # post-condition
    .check_lst(bbox,
        min_len = 4, max_len = 4, fn_check = .check_num,
        len_min = 1, len_max = 1,
        msg = "invalid tile 'bbox' value"
    )

    return(bbox)
}

#' @title Generate token to cube
#' @name .cube_token_generator
#' @keywords internal
#'
#' @param  cube input data cube
#'
#' @return A sits cube
.cube_token_generator <- function(cube) {
    source <- .source_new(
        source = .cube_source(cube),
        collection = .cube_collection(cube)
    )

    UseMethod(".cube_token_generator", source)
}

#' @export
.cube_token_generator.mpc_cube <- function(cube) {
    file_info <- cube[["file_info"]][[1]]
    fi_paths <- file_info[["path"]]

    are_local_paths <- !grepl(pattern = "^/vsi", x = fi_paths)
    # ignore in case of regularized and local cubes
    if (all(are_local_paths)) {
        return(cube)
    }

    if ("token_expires" %in% colnames(file_info)) {
        difftime_token <- difftime(
            time1 = file_info[["token_expires"]][[1]],
            time2 = as.POSIXlt(Sys.time(), tz = "UTC"),
            units = "mins"
        )

        # verify if there are still 25 minutes left to expire
        if (difftime_token - 15 > 25) {
            return(cube)
        }
    }

    token_endpoint <- .config_get(c("sources", .cube_source(cube), "token_url"))
    url <- paste0(token_endpoint, "/", tolower(.cube_collection(cube)))

    res_content <- NULL
    n_tries <- .config_get("cube_token_generator_n_tries")
    while (is.null(res_content) && n_tries > 0) {
        res_content <- tryCatch(
            {
                httr::content(httr::GET(url), encoding = "UTF-8")
            },
            error = function(e) {
                return(NULL)
            }
        )

        if (is.null(res_content)) {
            Sys.sleep(10)
        }
        n_tries <- n_tries - 1
    }

    .check_that(
        !is.null(res_content),
        msg = "invalid mpc token."
    )

    token_parsed <- httr::parse_url(paste0("?", res_content[["token"]]))
    file_info[["path"]] <- purrr::map_chr(seq_along(fi_paths), function(i) {
        path <- fi_paths[[i]]

        if (are_local_paths[[i]]) {
            return(path)
        }

        url_parsed <- httr::parse_url(path)
        url_parsed[["query"]] <- utils::modifyList(
            url_parsed[["query"]],
            token_parsed[["query"]]
        )

        # remove the additional chars added by httr
        new_path <- gsub("^://", "", httr::build_url(url_parsed))
        new_path
    })

    file_info[["token_expires"]] <- strptime(
        x = res_content[["msft:expiry"]],
        format = "%Y-%m-%dT%H:%M:%SZ"
    )

    cube[["file_info"]][[1]] <- file_info

    return(cube)
}
#' @export
.cube_token_generator.default <- function(cube) {
    return(cube)
}
