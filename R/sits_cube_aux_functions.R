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
    .check_chr_parameter(bands, len_max = 2^31 - 1)

    if (!add_cloud) {
        bands <- bands[bands != .source_cloud()]
    }

    return(bands)
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
    .check_chr_parameter(band)
    .check_band_in_cube(band, cube)

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
    .check_num_parameter(mv, allow_na = TRUE)

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

    # pre-conditions
    .check_chr_parameter(band)
    .check_band_in_cube(band, cube)

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
    .check_num_parameter(mv)

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

    # pre-conditions
    .check_chr_parameter(band)
    .check_band_in_cube(band, cube)

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
    .check_num_parameter(mv)

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
    .check_chr_parameter(band)
    .check_band_in_cube(band, cube)

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
    .check_num_parameter(sf, exclusive_min = 0)

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
    .check_chr_parameter(band)
    .check_band_in_cube(band, cube)

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
    .check_num_parameter(ov)

    return(ov)
}


#' @title Return collection of a data cube
#' @keywords internal
#' @name .cube_collection
#' @param cube  data cube
#' @return collection associated to the cube
.cube_collection <- function(cube) {
    collection <- unique(cube[["collection"]])

    # post-condition
    .check_chr_parameter(collection)

    return(collection)
}
#' @title Return crs of a data cube
#' @keywords internal
#' @name .cube_crs
#' @param cube  data cube
#' @return crs associated to the cube
.cube_crs <- function(cube) {
    crs <- unique(cube[["crs"]])

    # post-condition
    .check_chr_parameter(crs, len_max = nrow(cube))

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

    # only one tile at a time is processed
    .check_cube_has_one_tile(cube)

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
    .check_cube_has_one_tile(cube)
    # does the cube contain the band?
    .check_band_in_cube(band_cube, cube)

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

#' @title Given a tile, fid, and band, return a new cube with bounding box
#' values updated.
#' @name .cube_filter
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a data cube and a tile, fid, and band, return a new
#' cube with bounding box updated
#'
#' @param cube Metadata about a data cube
#' @param tile A tile name
#' @param fid  A feature fid
#' @param band A band name
#'
#' @return a sits cube with bounding box values updated.
#'
.cube_filter <- function(cube, tile = NULL, fid = NULL, band = NULL) {

    if (!is.null(tile)) {
        cube <- dplyr::filter(cube, .data[["tile"]] == !!tile)
        .check_cube_has_one_tile(cube)
    }

    # get file_info for a given fid
    fi_cube <- .file_info(cube, fid = fid)

    # cube filtered by bands
    cube <- sits_select(cube, bands = band)

    source <- .cube_source(cube)
    col <- .cube_collection(cube)

    cube_tile <- .cube_create(
        source = source,
        collection = col,
        satellite = .source_collection_satellite(source, col),
        sensor = .source_collection_sensor(source, col),
        tile = tile,
        xmin = max(fi_cube[["xmin"]]),
        xmax = min(fi_cube[["xmax"]]),
        ymin = max(fi_cube[["ymin"]]),
        ymax = min(fi_cube[["ymax"]]),
        crs = .cube_crs(cube),
        file_info = fi_cube
    )

    class(cube_tile) <- class(cube)

    return(cube_tile)
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
    .check_cube_has_one_tile(cube)

    # pre-conditions
    .check_int_parameter(block[["first_row"]],
                         min = 1, max = .cube_size(cube)[["nrows"]]
    )
    .check_int_parameter(block[["first_col"]],
        min = 1, max = .cube_size(cube)[["ncols"]],
    )

    .check_int_parameter(block[["nrows"]],
        min = 1, max = .cube_size(cube)[["nrows"]]
    )

    .check_int_parameter(block[["ncols"]],
        min = 1, max = .cube_size(cube)[["ncols"]]
    )

    source <- .cube_source(cube)
    collection <- .cube_collection(cube)

    params <- .sits_raster_sub_image_from_block(
        block = block,
        source = source,
        collection = collection,
        tile = cube
    )

    tolerance <- .config_get(key = c(
        "sources", source,
        "collections", collection,
        "ext_tolerance"
    ))

    # post-conditions
    xmin <- params[["xmin"]]
    .check_num_parameter(
        xmin,
        min = cube[["xmin"]], max = cube[["xmax"]],
        tolerance = tolerance
    )
    xmax <- params[["xmax"]]
    .check_num_parameter(
        xmax,
        min = cube[["xmin"]], max = cube[["xmax"]],
        tolerance = tolerance
    )
    ymin <- params[["ymin"]]
    .check_num_parameter(
        ymin,
        min = cube[["ymin"]], max = cube[["ymax"]],
        tolerance = tolerance
    )
    ymax <- params[["ymax"]]
    .check_num_parameter(
        ymax,
        min = cube[["ymin"]], max = cube[["ymax"]],
        tolerance = tolerance
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
    .check_num_parameter(xres, exclusive_min = 0)

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
    .check_num_parameter(yres, exclusive_min = 0)

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
    .check_int_parameter(nrows)
    .check_int_parameter(ncols)

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
    .check_cube_has_one_tile(cube)

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
#' @title Get tolerance of a cube for comparison purposes
#' @name .cube_tolerance
#' @keywords internal
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube input data cube
#'
#' @return The numerical tolerance value
#'
.cube_tolerance <- function(cube){
    tolerance <- .config_get(key = c(
        "sources", .cube_source(cube),
        "collections", .cube_collection(cube),
        "ext_tolerance"
    ))
    return(tolerance)
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

    # we consider token is expired when the remaining time is
    # less than 5 minutes
    if ("token_expires" %in% colnames(file_info) &&
        !.cube_is_token_expired(cube)) {
        return(cube)
    }

    token_endpoint <- .config_get(c("sources", .cube_source(cube), "token_url"))
    url <- paste0(token_endpoint, "/", tolower(.cube_collection(cube)))

    res_content <- NULL

    n_tries <- .config_get("cube_token_generator_n_tries")
    sleep_time <- .config_get("cube_token_generator_sleep_time")
    while (is.null(res_content) && n_tries > 0) {
        res_content <- tryCatch(
            {
                res <- httr::stop_for_status(httr::GET(url))
                httr::content(res, encoding = "UTF-8")
            },
            error = function(e) {
                return(NULL)
            }
        )

        if (is.null(res_content)) {
            Sys.sleep(sleep_time)
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

#' @title Check if a cube token was expired
#' @name .cube_is_token_expires
#' @keywords internal
#'
#' @param cube input data cube
#'
#' @return a boolean value.
.cube_is_token_expired <- function(cube) {
    source <- .source_new(
        source = .cube_source(cube),
        collection = .cube_collection(cube)
    )

    UseMethod(".cube_is_token_expired", source)
}

#' @export
.cube_is_token_expired.mpc_cube <- function(cube) {
    file_info <- cube[["file_info"]][[1]]
    fi_paths <- file_info[["path"]]

    min_remaining_time <- .config_get(
        "cube_token_generator_min_remaining_time",
        default = 0
    )

    are_local_paths <- !grepl(pattern = "^/vsi", x = fi_paths)
    # ignore in case of regularized and local cubes
    if (all(are_local_paths)) {
        return(FALSE)
    }

    if ("token_expires" %in% colnames(file_info)) {
        difftime_token <- difftime(
            time1 = file_info[["token_expires"]][[1]],
            time2 = as.POSIXlt(Sys.time(), tz = "UTC"),
            units = "mins"
        )

        return(difftime_token < min_remaining_time)
    }

    return(FALSE)
}

#' @export
.cube_is_token_expired.default <- function(cube) {
    return(FALSE)
}

#' @export
.cube_token_generator.default <- function(cube) {
    return(cube)
}


