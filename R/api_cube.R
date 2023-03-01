#' @title Creates the description of a data cube
#' @name .cube_create
#' @keywords internal
#' @noRd
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

    class(cube) <- unique(c("raster_cube", class(cube)))

    return(cube)
}
#' @title Return bands of a data cube
#' @keywords internal
#' @noRd
#' @name .cube_bands
#' @param cube       Data cube
#' @param add_cloud  Include the cloud band?
#'
#' @return A \code{vector} with the cube bands.
.cube_bands <- function(cube, add_cloud = TRUE) {
    bands <- .tile_bands(cube)

    # post-condition
    .check_chr_parameter(bands, len_max = 2^31 - 1)

    if (!add_cloud) {
        bands <- bands[bands != .source_cloud()]
    }

    return(bands)
}

#' @title Return collection of a data cube
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
.cube_derived_create <- function(tile,
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
    .check_has_one_tile(tile)

    # output filename
    file_name <- paste0(
        tile[["satellite"]], "_",
        tile[["sensor"]], "_",
        tile[["tile"]], "_",
        start_date, "_",
        end_date, "_",
        band_name, "_",
        version, ".tif"
    )

    file_name <- file.path(output_dir, file_name)

    if (!purrr::is_null(bbox)) {
        sub_image <- .raster_sub_image(tile = tile, roi = bbox)
        nrows_cube_class <- sub_image[["nrows"]]
        ncols_cube_class <- sub_image[["ncols"]]
    } else {
        nrows_cube_class <- .tile_nrows(tile)
        ncols_cube_class <- .tile_ncols(tile)
    }

    # set the file information
    file_info <- tibble::tibble(
        band       = band_name,
        start_date = start_date,
        end_date   = end_date,
        ncols      = ncols_cube_class,
        nrows      = nrows_cube_class,
        xres       = .tile_xres(tile),
        yres       = .tile_yres(tile),
        xmin       = bbox[["xmin"]],
        xmax       = bbox[["xmax"]],
        ymin       = bbox[["ymin"]],
        ymax       = bbox[["ymax"]],
        crs        = tile[["crs"]],
        path       = file_name
    )

    # set the metadata for the probability cube
    dev_cube <- .cube_create(
        source     = .tile_source(tile),
        collection = .tile_collection(tile),
        satellite  = tile[["satellite"]],
        sensor     = tile[["sensor"]],
        tile       = tile[["tile"]],
        xmin       = bbox[["xmin"]],
        xmax       = bbox[["xmax"]],
        ymin       = bbox[["ymin"]],
        ymax       = bbox[["ymax"]],
        crs        = tile$crs,
        labels     = labels,
        file_info  = file_info
    )

    class(dev_cube) <- unique(c(cube_class, "derived_cube", "raster_cube",
                                class(dev_cube)))

    return(dev_cube)
}

#' @title Get derived class of a cube
#' @name .cube_derived_class
#' @keywords internal
#' @noRd
#' @param cube A cube
#'
#' @return derived class
.cube_derived_class <- function(cube) {
    UseMethod(".cube_derived_class", cube)
}

#' @export
.cube_derived_class.derived_cube <- function(cube) {
    unique(slider::slide_chr(cube, .tile_derived_class))
}

#' @title Return the S3 class of the cube
#' @name .cube_s3class
#' @keywords internal
#' @noRd
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

#' @title Return the column size of each tile
#' @name .cube_ncols
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube  input data cube
#' @return integer
.cube_ncols <- function(cube) {
    slider::slide_int(cube, .tile_ncols)
}
#' @title Return the row size of each tile
#' @name .cube_nrows
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cube  input data cube
#' @return integer
.cube_nrows <- function(cube) {
    slider::slide_int(cube, .tile_nrows)
}
#' @title Get cube source
#' @name .cube_source
#' @keywords internal
#' @noRd
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
#' @title Verify if cube is regular
#' @name .cube_is_regular
#' @keywords internal
#' @noRd
#' @param cube  datacube
#' @return logical
.cube_is_regular <- function(cube) {
    if (!.check_has_unique_bands(cube)) {
        return(FALSE)
    }
    if (!.check_has_unique_bbox(cube)) {
        return(FALSE)
    }
    if (!.check_has_unique_tile_size(cube)) {
        return(FALSE)
    }
    if (!.check_has_unique_timeline(cube)) {
        return(FALSE)
    }
    return(TRUE)
}
#' @title Generate token to cube
#' @name .cube_token_generator
#' @keywords internal
#' @noRd
#' @param  cube input data cube
#'
#' @return A sits cube
.cube_token_generator <- function(cube) {

    UseMethod(".cube_token_generator", cube)
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

    token_endpoint <- .conf("sources", .cube_source(cube), "token_url")
    url <- paste0(token_endpoint, "/", tolower(.cube_collection(cube)))

    res_content <- NULL

    n_tries <- .conf("cube_token_generator_n_tries")
    sleep_time <- .conf("cube_token_generator_sleep_time")
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
#' @export
.cube_token_generator.default <- function(cube) {
    return(cube)
}

#' @title Check if a cube token was expired
#' @name .cube_is_token_expires
#' @keywords internal
#' @noRd
#' @param cube input data cube
#'
#' @return a boolean value.
.cube_is_token_expired <- function(cube) {

    UseMethod(".cube_is_token_expired", cube)
}

#' @export
.cube_is_token_expired.mpc_cube <- function(cube) {
    file_info <- cube[["file_info"]][[1]]
    fi_paths <- file_info[["path"]]

    min_remaining_time <- .conf(
        "cube_token_generator_min_remaining_time"
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



#' Cube API
#'
#' A \code{cube} is a \code{tibble} containing information on how to access
#' some data cube. Each row of a \code{cube} is a \code{tile}, which represents
#' a rectangular spatial region of space in some projection.
#' For more details, see tiles API.
#'
#' @param fn     A function.
#' @param roi    A region of interest (ROI).
#' @param start_date,end_date Date of start and end.
#' @param bands  A set of band names.
#' @param tiles  A set of tile names.
#' @param ...    Additional arguments (see details).
#'
#' @returns See description of each function.
#' @family cube and tile functions
#' @keywords internal
#' @name cube_api
#' @noRd
NULL

#' @title Sets the class of a data cube
#' @noRd
#' @param cube  A data cube.
#' @param ...  Provide additional class names.
#' @returns  An updated data cube.
.cube_set_class <- function(cube, ...) {
    .set_class(cube, ..., c("raster_cube", "tbl_df", "tbl", "data.frame"))
}
#' @title Get start date from each tile in a cube
#' @noRd
#' @param cube  A data cube.
#' @return  A vector of dates.
.cube_start_date <- function(cube) {
    UseMethod(".cube_start_date", cube)
}
#' @export
.cube_start_date.raster_cube <- function(cube) {
    .compact(.as_date(slider::slide(cube, .tile_start_date)))
}
#' @title Get end date from each tile in a cube
#' @noRd
#' @param cube  A data cube.
#' @return  A vector of dates.
.cube_end_date <- function(cube) {
    UseMethod(".cube_end_date", cube)
}
#' @export
.cube_end_date.raster_cube <- function(cube) {
    .compact(.as_date(slider::slide(cube, .tile_end_date)))
}
#' @title Get timeline from each tile in a cube
#' @noRd
#' @param cube  A cube.
#' @details
#' Returns a unique timeline if there are a unique value. If there are at
#' least two different timelines, all timelines will be returned in a list.
#' @return A vector of dates.
.cube_timeline <- function(cube) {
    UseMethod(".cube_timeline", cube)
}
#' @export
.cube_timeline.raster_cube <- function(cube) {
    values <- .compact(slider::slide(cube, .tile_timeline))
    if (length(values) != 1) {
        return(values)
    }
    .as_date(values)
}
#' @title Find out how many images are in cube during a period
#' @noRd
#' @param cube  A data cube.
#' @param period  Period character vector in ISO format.
#' @param origin  The first date to start count.
#' @details
#' Compute how many images were acquired in different periods
#' and different tiles.
#' @returns A tibble
.cube_timeline_acquisition <- function(cube, period, origin) {
    UseMethod(".cube_timeline_acquisition", cube)
}
#' @export
.cube_timeline_acquisition.raster_cube <- function(cube,
                                                   period = "P1D",
                                                   origin = NULL) {
    if (!.has(origin)) {
        origin <- .cube_start_date(cube)
    }
    values <- slider::slide_dfr(cube, function(tile) {
        tibble::tibble(
            tile = tile[["tile"]],
            dates = .tile_timeline(!!tile)
        )
    })
    values <- dplyr::filter(values, !!origin <= .data[["dates"]])
    values <- dplyr::arrange(values, .data[["dates"]])
    values <- slider::slide_period_dfr(
        values, values[["dates"]], .period_unit(period),
        function(x) {
            x[["from_date"]] <- min(x[["dates"]])
            x[["to_date"]] <- max(x[["dates"]])
            dplyr::count(
                x, .data[["from_date"]], .data[["to_date"]],
                .data[["tile"]]
            )
        },
        .every = .period_val(period), .origin = origin, .complete = TRUE
    )
    id_cols <- c("from_date", "to_date")
    if (all(values[["from_date"]] == values[["to_date"]])) {
        values[["date"]] <- values[["from_date"]]
        id_cols <- "date"
    }
    tidyr::pivot_wider(
        values,
        id_cols = dplyr::all_of(id_cols),
        names_from = "tile",
        values_from = "n"
    )
}

#' @title Tile iteration
#' @noRd
#' @param cube  A data cube.
#' @param fn  A function that receives and return a tile.
#' @param ...  Additional arguments to be passed to `fn`.
#' @details
#' Iterates over each cube tile, passing tile to function's first argument.
#' @returns  A processed data cube.
.cube_foreach_tile <- function(cube, fn, ...) {
    UseMethod(".cube_foreach_tile", cube)
}
#' @export
.cube_foreach_tile.raster_cube <- function(cube, fn, ...) {
    slider::slide_dfr(cube, fn, ...)
}

#' @title What tiles intersect \code{roi} parameter?
#' @noRd
#' @param cube  A data cube.
#' @param roi  A region of interest (ROI).
#' @return A logical vector.
.cube_intersects <- function(cube, roi) {
    UseMethod(".cube_intersects", cube)
}
#' @export
.cube_intersects.raster_cube <- function(cube, roi) {
    slider::slide_lgl(cube, .tile_intersects, roi = .roi_as_sf(roi))
}
#' @title Filter tiles that intersect \code{roi} parameter.
#' @noRd
#' @param cube  A data cube.
#' @param roi  A region of interest (ROI).
#' @return  A filtered data cube.
.cube_filter_spatial <- function(cube, roi) {
    UseMethod(".cube_filter_spatial", cube)
}
#' @export
.cube_filter_spatial.raster_cube <- function(cube, roi) {
    intersecting <- .cube_intersects(cube, roi)
    if (!any(intersecting)) {
        stop("spatial region does not intersect cube")
    }
    cube[intersecting, ]
}
#' @title Test tiles with images during an interval
#' @noRd
#' @param cube  A data cube.
#' @param start_date,end_date  Dates of interval.
#' @return A logical vector
.cube_during <- function(cube, start_date, end_date) {
    UseMethod(".cube_during", cube)
}
#' @export
.cube_during.raster_cube <- function(cube, start_date, end_date) {
    slider::slide_lgl(
        cube, .tile_during, start_date = start_date, end_date = end_date
    )
}
#' @title Filter tiles inside a temporal interval
#' @noRd
#' @param cube  A data cube.
#' @param start_date,end_date  Dates of interval.
#' @return  A filtered data cube.
.cube_filter_interval <- function(cube, start_date, end_date) {
    UseMethod(".cube_filter_interval", cube)
}

#' @export
.cube_filter_interval.raster_cube <- function(cube, start_date, end_date) {
    during <- .cube_during(cube, start_date, end_date)
    if (!any(during)) {
        stop("informed interval does not interesect cube")
    }
    cube <- slider::slide_dfr(cube[during,], function(tile)
        .tile_filter_interval(tile, start_date, end_date)
    )
}
#' @title Filter cube based on a set of bands
#' @noRd
#' @param cube  A data cube.
#' @param bands  Band names.
#' @return  Filtered data cube.
.cube_filter_bands <- function(cube, bands) {
    UseMethod(".cube_filter_bands", cube)
}
#' @export
.cube_filter_bands.raster_cube <- function(cube, bands) {
    .cube_foreach_tile(cube, function(tile) {
        .tile_filter_bands(tile = tile, bands = bands)
    })
}
#' @title Returns the tile names of a data cube
#' @noRd
#' @param cube  A data cube.
#' @return  Names of tiles.
.cube_tiles <- function(cube) {
    UseMethod(".cube_tiles", cube)
}
#' @export
.cube_tiles.raster_cube <- function(cube) {
    .as_chr(cube[["tile"]])
}

#' @title Returns the cube paths
#' @noRd
#' @param cube  A data cube.
#' @return  Character with cube paths.
.cube_paths <- function(cube) {
    UseMethod(".cube_paths", cube)
}
#' @export
.cube_paths.raster_cube <- function(cube) {
    slider::slide_chr(cube, function(tile) {
        .fi_path(.fi(.tile(tile)))
    })
}
.cube_is_local <- function(cube) {
    all(.file_is_local(.file_remove_vsi(.cube_paths(cube))))
}
#' @title Filter the cube using tile names
#' @noRd
#' @param cube  A data cube.
#' @param tiles  Tile names.
#' @return  Filtered data cube.
.cube_filter_tiles <- function(cube, tiles) {
    UseMethod(".cube_filter_tiles", cube)
}
#' @export
.cube_filter_tiles.raster_cube <- function(cube, tiles) {
    cube[.cube_tiles(cube) %in% tiles, ]
}

#' @title Create internal cube features with ID
#' @noRd
#' @param cube  data cube
#' @return cube with feature ID in file info
.cube_split_features <- function(cube) {
    # Process for each tile and return a cube
    .cube_foreach_tile(cube, function(tile) {
        features <- tile[, c("tile", "file_info")]
        features <- tidyr::unnest(features, "file_info")
        features[["feature"]] <- features[["fid"]]
        features <- tidyr::nest(features, file_info = -c("tile", "feature"))
        # Replicate each tile so that we can copy file_info to cube
        tile <- tile[rep(1, nrow(features)), ]
        tile[["file_info"]] <- features[["file_info"]]
        tile
    })
}

#' @title create assets for a data cube by assigning a unique ID
#' @noRd
#' @param  cube  datacube
#' @return a data cube with assets (file ID)
#'
.cube_split_assets <- function(cube) {
    # Process for each tile and return a cube
    .cube_foreach_tile(cube, function(tile) {
        assets <- tile[, c("tile", "file_info")]
        assets <- tidyr::unnest(assets, "file_info")
        assets[["feature"]] <- assets[["fid"]]
        assets[["asset"]] <- assets[["band"]]
        assets <- tidyr::nest(
            assets, file_info = -c("tile", "feature", "asset")
        )
        # Replicate each tile so that we can copy file_info to cube
        tile <- tile[rep(1, nrow(assets)), ]
        tile[["file_info"]] <- assets[["file_info"]]
        tile
    })
}
#' @title Merge features into a data cube
#' @noRd
#' @param features  cube features
#' @return merged data cube
.cube_merge_tiles <- function(cube) {
    class_orig <- class(cube)
    cube <- tidyr::unnest(cube, "file_info", names_sep = ".")
    cube <- dplyr::arrange(
        cube, .data[["file_info.date"]], .data[["file_info.band"]]
    )
    cube <- tidyr::nest(
        cube, file_info = tidyr::starts_with("file_info"),
        .names_sep = "."
    )
    # Set class features for the cube
    class(cube) <- class_orig
    # Return cube
    cube
}
