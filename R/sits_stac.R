#' @title Get information from collection
#' @name .sits_stac_collection
#' @keywords internal
#'
#' @param url         a \code{character} representing a URL for the BDC catalog.
#' @param collection  a \code{character} with the collection to be searched.
#' @param bands       a \code{character} with the bands names to be filtered.
#' @param ...         other parameters to be passed for specific types.
#'
#' @return            a \code{STACCollection} object returned by rstac.
.sits_stac_collection <- function(url,
                                  collection,
                                  bands = NULL, ...) {

    # creating a rstac object and making the requisition
    collection_info <- rstac::stac(url) %>%
        rstac::collections(collection_id = collection) %>%
        rstac::get_request(...)

    # converts bands name to upper case
    collection_info <- .sits_stac_toupper(collection_info)

    # get instrument name
    sensor <- collection_info$properties$instruments[[1]]

    # get default bands parameter
    if (purrr::is_null(bands)) {

        bands <- .config_bands_band_name(source = "BDC",
                                          collection = collection)
    }

    # checks if the supplied bands match the product bands
    # converting to upper bands
    bands <- toupper(bands)

    # convert bands to those known by provider
    bands <- .sits_config_bands_guess(source     = "BDC",
                                      collection = collection,
                                      bands      = bands)

    # select subset bands
    collection_info$bands <-
        collection_info$bands[collection_info$bands %in% bands]

    return(collection_info)
}
#' @title Get information from items
#' @name .sits_stac_items
#' @keywords internal
#'
#' @param url        a \code{character} representing a URL for the BDC catalog.
#' @param collection a \code{character} with the collection to be searched.
#' @param tiles      a \code{character} with the names of the tiles.
#' @param roi        defines a region of interest. It can be
#'                   an \code{sfc} or \code{sf} object from sf package,
#'                   a \code{character} with a GeoJSON using RFC 7946,
#'                   or a \code{vector} bounding box \code{vector}
#'                   with named XY values ("xmin", "xmax", "ymin", "ymax").
#' @param start_date a \code{character} corresponds to the initial date
#'                   when the cube will be created.
#' @param end_date   a \code{character} corresponds to the final date when the
#'                   cube will be created.
#' @param ...        other parameters to be passed for specific types.
#'
#' @return           a \code{STACItemCollection} object
#'                   representing the search by rstac.
.sits_stac_items <- function(url        = NULL,
                             collection = NULL,
                             tiles      = NULL,
                             roi        = NULL,
                             start_date = NULL,
                             end_date   = NULL, ...) {

    # obtain the datetime parameter for STAC like parameter
    datetime <- .sits_stac_datetime(start_date, end_date)

    # obtain the bbox and intersects parameters
    if (!is.null(roi)) {
        roi <- .sits_stac_roi(roi)
    } else {
        roi[c("bbox", "intersects")] <- list(NULL, NULL)
    }

    # get the limit items to be returned in each page
    limit_items <- .config_rstac_limit()

    # creating a rstac object
    rstac_query <- rstac::stac(url) %>%
        rstac::stac_search(collection = collection,
                           bbox       = roi$bbox,
                           intersects = roi$intersects,
                           datetime   = datetime,
                           limit      = limit_items)

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles))
        rstac_query <- rstac_query %>%
        rstac::ext_query("bdc:tile" %in% tiles)

    # making the request
    items_info <- rstac_query %>% rstac::post_request(...)

    # check if matched items
    assertthat::assert_that(
        rstac::items_matched(items_info) > 0,
        msg = ".sits_stac_items: no items matched the query criteria"
    )

    # progress bar status
    pgr_fetch  <- FALSE

    # if more than 1000 items are found the progress bar is displayed
    if (rstac::items_matched(items_info) > 1000)
        pgr_fetch <- TRUE

    # fetching all the metadata
    items_info <- items_info %>% rstac::items_fetch(progress = pgr_fetch)

    # converting to upper names
    items_info$features <- purrr::map(items_info$features, function(x) {
        names(x$assets) <- toupper(names(x$assets))

        return(x)
    })

    return(items_info)
}
#' @title Get bands names from items
#' @name .sits_stac_bands
#' @keywords internal
#'
#' @param items      a \code{STACItemCollection} object returned by rstac
#' package.
#' @param bands      a \code{character} vector with the bands name.
#' @param source     Data source
#' @param collection a \code{character} with the collection to be searched.
#'
#' @return           a \code{STACItemCollection} object representing the search
#'                   by rstac.
.sits_stac_bands <- function(items, bands, source, collection) {

    if (is.null(bands))
        bands <- .sits_config_collection_bands(source = source,
                                               collection = collection)
    # get bands from source
    bands_source <- .sits_config_collection_bands_field(source = source,
                                                        collection = collection,
                                                        field = "band_name",
                                                        cloud = TRUE)

    # get bands name from assets list name property
    bands_asset <- rstac::items_fields(items, "assets")

    # subset of supported bands
    bands_asset <- bands_source[bands_source %in% bands_asset]

    assertthat::assert_that(all(bands_source %in% bands_asset),
                            msg = paste("The bands contained in this product",
                                        "are not mapped in the SITS package,",
                                        "if you want to include them please",
                                        "provide it in configuration file."))


    items$bands <- .sits_config_bands_reverse(source, collection, bands)

    # TODO: filtrar os assets NA
    items <- .sits_stac_bands_mutate(items = items,
                                     reverse_bands = items$bands)

    return(items)
}

#' TODO: document
.sits_stac_bands_select <- function(items, bands_source, bands_converter) {

    assertthat::assert_that(
        all(bands_source %in% rstac::items_fields(items, "assets")),
        msg = paste("The bands contained in this product",
                    "are not mapped in the SITS package,",
                    "if you want to include them please",
                    "provide it in configuration file."))


    items$features <- purrr::map(items$features, function(item){

        item$assets <- item$assets[bands_source]
        names(item$assets) <- bands_converter[names(item$assets)]

        item
    })

    items
}

#' @title Converts bands name to upper case
#' @name .sits_stac_toupper
#' @keywords internal
#'
#' @param collection_info a \code{STACCollection} object returned by rstac.
#'  package.
#'
#' @return       a \code{STACCollection} object with the bands in upper case.
.sits_stac_toupper <- function(collection_info) {

    collection_info$properties[["eo:bands"]] <-
        purrr::map(collection_info$properties[["eo:bands"]], function(x) {
            x$name <- toupper(x$name)
            return(x)
        })

    collection_info$bands <-
        purrr::map_chr(collection_info$properties[["eo:bands"]],
                       `[[`, c("name"))


    return(collection_info)
}
#' @title Create a group of items
#' @name .sits_stac_group
#' @keywords internal
#'
#' @param items  a \code{STACItemCollection} object returned by rstac package.
#' @param fields a \code{character} vector with the names of fields to be
#'  grouped.
#'
#' @return       a \code{list} in which each index corresponds to a group with
#'  its corresponding \code{STACItemCollection} objects.
.sits_stac_group <- function(items, fields) {

    # grouping the items according to fields provided
    items_grouped <- rstac::items_group(items  = items,
                                        field  = fields)

    # adding a tile attribute in the root
    items_grouped <- purrr::map(items_grouped, function(x) {
        x$tile <- rstac::items_reap(x, field = fields)[[1]]

        # resolution
        x$xres <- x$features[[1]]$properties[["eo:gsd"]]
        x$yres <- x$features[[1]]$properties[["eo:gsd"]]

        # size raster
        attrib <- "bdc:raster_size"
        if (is.null(x$features[[1]]$assets[[1]][[attrib]]))
            attrib <- "raster_size"
        x$ncols <- x$features[[1]]$assets[[1]][[attrib]]$x
        x$nrows <- x$features[[1]]$assets[[1]][[attrib]]$y

        return(x)
    })

    return(items_grouped)
}

#' @title Checks if the crs provided is valid
#' @name .sits_check_crs
#' @keywords internal
#'
#' @param stac_crs a \code{numeric} or \code{character} with CRS provided by
#'  STAC.
#'
#' @return  a \code{character} with the formatted CRS.
.sits_format_crs <- function(stac_crs) {

    if (is.null(stac_crs))
        stop(paste("sits_cube: The CRS in this catalog is null, please",
                   "enter a valid CRS."))
    return(sf::st_crs(stac_crs)[["input"]])
}
#' @title Get bbox and intersects parameters
#' @name .sits_stac_roi
#' @keywords internal
#'
#' @param roi  the "roi" parameter defines a region of interest. It can be
#'  an \code{sfc} or \code{sf} object from sf package, a \code{character} with
#'  GeoJSON following the rules from RFC 7946, or a \code{vector}
#'  bounding box \code{vector} with named XY values
#'  ("xmin", "xmax", "ymin", "ymax").
#'
#' @return     A named \code{list} with the values of the intersection and bbox
#'             parameters. If bbox is supplied, the intersection parameter gets
#'             NULL, otherwise bbox gets NULL if intersects is specified.
.sits_stac_roi <- function(roi) {

    # list to store parameters values
    roi_list <- list()

    # verify the provided parameters
    if (!inherits(roi, "sf")) {
        if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(roi)))
            roi_list[c("bbox", "intersects")] <-
                list(roi[c("xmin", "ymin", "xmax", "ymax")], NULL)

        else if (typeof(roi) == "character")
            roi_list[c("bbox", "intersects")] <- list(NULL, roi)
    } else {
        roi_list[c("bbox", "intersects")] <- list(as.vector(sf::st_bbox(roi)),
                                                  NULL)
    }

    # checks if the specified parameters names is contained in the list
    assertthat::assert_that(
        !purrr::is_null(names(roi_list)),
        msg = ".sits_stac_roi: invalid definition of ROI"
    )

    return(roi_list)
}
#' @title Datetime format
#' @name .sits_stac_datetime
#' @keywords internal
#'
#' @param start_date a \code{character} corresponds to the initial date when the
#'  cube will be created.
#' @param end_date   a \code{character} corresponds to the final date when the
#'  cube will be created.
#'
#' @return      a \code{character} formatted as parameter to STAC requisition.
.sits_stac_datetime <- function(start_date, end_date) {

    # ensuring that start_date and end_date were provided
    assertthat::assert_that(
        !purrr::is_null(start_date) && !purrr::is_null(end_date),
        msg = paste("sits_cube: for STAC_CUBE start_date",
                    "and end_date must be provided"))

    # adding the dates according to RFC 3339
    datetime <- paste(start_date, end_date, sep = "/")

    return(datetime)
}
#' @title Format assets
#' @name .sits_stac_to_fileinfo
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac package.
#'
#' @return      a \code{tibble} with date, band and path information, arranged
#'  by the date.
.sits_stac_to_fileinfo <- function(items) {

    assets_info <- items %>%
        rstac::assets_list() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(date = lubridate::as_date(as.character(date)))

    return(assets_info)
}

#' @title Get the STAC information corresponding to a bbox extent
#' @name .sits_stac_get_bbox
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac.
#' @param crs   a \code{character} with proj code.
#'
#' @return  a \code{bbox} object from the sf package representing the tile bbox.
.sits_stac_get_bbox <- function(items, crs) {

    # get the extent points
    extent_points <- items$features[[1]]$geometry$coordinates[[1]]

    # create a polygon and transform the proj
    polygon_ext <- sf::st_polygon(list(do.call(rbind, extent_points)))
    polygon_ext <- sf::st_sfc(polygon_ext, crs = 4326) %>%
        sf::st_transform(., crs)

    bbox_ext <- sf::st_bbox(polygon_ext)

    return(bbox_ext)
}
#' @title Get the STAC information corresponding to a tile.
#' @name .sits_stac_tile_cube
#' @keywords internal
#'
#' @param name             Name of output data cube.
#' @param collection       Name of image collection in  BDC.
#' @param collection_info  STACCollection object returned by rstac.
#' @param items            STACItemCollection object returned by rstac.
#' @param file_info        Tibble with the information from STAC.
#'
#' @return                a \code{tibble} with metadata information about a
#'  raster data set.
.sits_stac_tile_cube <- function(name,
                                 collection,
                                 collection_info,
                                 items,
                                 file_info) {

    # obtain bbox extent
    bbox <- .sits_stac_get_bbox(items, collection_info[["bdc:crs"]])

    # add resolution to file_info
    file_info <- dplyr::mutate(file_info, res = as.numeric(items$xres),
                               .before = path)

    # create a tibble to store the metadata
    tile <- .sits_cube_create(
        name       = name,
        source     = "BDC",
        collection = collection,
        satellite  = collection_info$properties$platform,
        sensor     = collection_info$properties$instruments[[1]],
        tile       = items$tile,
        bands      = collection_info$bands,
        nrows      = items$nrows,
        ncols      = items$ncols,
        xmin       = bbox$xmin[[1]],
        xmax       = bbox$xmax[[1]],
        ymin       = bbox$ymin[[1]],
        ymax       = bbox$ymax[[1]],
        xres       = items$xres,
        yres       = items$yres,
        crs        = collection_info[["bdc:crs"]],
        file_info  = file_info)

    bands_sits <- .sits_config_collection_bands(source = tile$source,
                                          collection = tile$collection)
    if (!all(tile$bands[[1]] %in% bands_sits)) {
        bands_sits <- .sits_config_bands_reverse(source = tile$source,
                                                 collection = tile$collection,
                                                 bands = tile$bands[[1]])

        tile$bands[[1]] <- unname(bands_sits[tile$bands[[1]]])
        tile$file_info[[1]]$band <- unname(bands_sits[tile$file_info[[1]]$band])
    }

    return(tile)
}
