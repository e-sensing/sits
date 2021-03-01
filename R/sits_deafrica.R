#' @title Get information from items
#' @name .sits_deafrica_items
#' @keywords internal
#'
#' @param url        a \code{character} representing a URL for the DEA catalog.
#' @param collection a \code{character} with the collection to be searched.
#' @param tiles      a \code{character} with the names of the tiles.
#' @param roi        selects images (tiles) that intersect according to the
#'  region of interest provided. Expressed either as an \code{sfc} or \code{sf}
#'  object from sf package, a \code{character} with GeoJSON following the rules
#'  from RFC 7946, or a \code{vector} with bounding box named XY values in
#'  WGS 84 ("xmin", "xmax", "ymin", "ymax").
#' @param start_date a \code{character} corresponds to the initial date
#'                   when the cube will be created.
#' @param end_date   a \code{character} corresponds to the final date when the
#'                   cube will be created.
#' @param bands      a \code{character} vector with the bands name.
#' @param ...        other parameters to be passed for specific types.
#'
#' @return           a \code{STACItemCollection} object representing the search
#'                   by rstac.
.sits_deafrica_items <- function(url        = NULL,
                                 collection = NULL,
                                 tiles      = NULL,
                                 roi        = NULL,
                                 start_date = NULL,
                                 end_date   = NULL,
                                 bands      = NULL,
                                 ...) {


    # obtain the datetime parameter for STAC like parameter
    datetime <- .sits_stac_datetime(start_date, end_date)

    # obtain the bbox and intersects parameters
    if (!is.null(roi)) {
        roi <- .sits_stac_roi(roi)
    } else {
        roi[c("bbox", "intersects")] <- list(NULL, NULL)
    }

    # get the limit items to be returned in each page
    limit_items <- .sits_config_rstac_limit()

    # creating a rstac object
    rstac_query <- rstac::stac(url) %>%
        rstac::stac_search(collection = collection,
                           bbox       = roi$bbox,
                           intersects = roi$intersects,
                           datetime   = datetime,
                           limit      = limit_items)

    # making the request
    items_info <- rstac_query %>% rstac::post_request(...)

    # progress bar status
    pgr_fetch  <- FALSE

    # if more than 1000 items are found the progress bar is displayed
    if (rstac::items_matched(items_info) > 1000)
        pgr_fetch <- TRUE

    # fetching all the metadata and updating to upper case instruments
    items_info <- items_info %>% rstac::items_fetch(progress = pgr_fetch) %>%
        .sits_deafrica_upper()

    # searching for tiles in the items
    if (!is.null(tiles))
        items_info <- .sits_deafrica_search_tiles(items_info, tiles)

    # checks if the collection returned any items
    assertthat::assert_that(!(rstac::items_length(items_info) == 0),
                            msg = "The provided search returned 0 items found.")

    # getting bands name
    items_info <- .sits_deafrica_bands(items_info, bands)

    return(items_info)
}
#' @title  Converts bands name to upper case
#' @name .sits_deafrica_upper
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac package.
#'  grouped.
#'
#' @return      a \code{STACItemCollection} object representing the search
#'              by rstac.
.sits_deafrica_upper <- function(items) {

    # converts the sensor of each item to upper case
    items$features <- purrr::map(items$features, function(x) {
        x$properties$instruments <- toupper(x$properties$instruments)
        x
    })

    return(items)
}
#' @title Search items tiles
#' @name .sits_deafrica_search_tiles
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac package.
#'  grouped.
#' @param tiles a \code{character} with the names of the tiles.
#'
#' @return      a \code{STACItemCollection} object representing the search
#'              by rstac.
.sits_deafrica_search_tiles <- function(items, tiles) {

    # checks if the supplied tiles are in the searched items
    index_features <- purrr::map_lgl(items$features, function(feature){
        region_code <- feature[["properties"]][["odc:region_code"]]
        if (region_code %in% tiles)
            return(TRUE)
        return(FALSE)
    })

    # selects the tiles found in the search
    items$features <- items$features[index_features]

    # checks if the search return zero items
    assertthat::assert_that(!(rstac::items_length(items) == 0),
                            msg = "The supplied tile(s) were not found.")

    return(items)
}
#' @title Get bands names from items
#' @name sits_deafrica_bands
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac package.
#'  grouped.
#' @param bands a \code{character} vector with the bands name.
#'
#' @return      a \code{STACItemCollection} object representing the search
#'              by rstac.
.sits_deafrica_bands <- function(items, bands) {

    # checks if the instrument is in the catalog
    if (length(items$features[[1]]$properties$instruments) == 0) {
        sat <- toupper(items$features[[1]]$properties$platform)

        items$features <- purrr::map(items$features, function(feature) {
            feature$properties$instruments <- .sits_config_sensors(sat)

            feature
        })
    }

    # store items properties attributes
    item_prop <- items$features[[1]]$properties

    # get bands from sensor and application
    bands_sensor <- .sits_config_sensor_bands(sensor = item_prop$instruments,
                                              cube = "DEAFRICA")

    # get bands name from assets list name property
    bands_product <- purrr::map_chr(items$features[[1]]$assets, function(bands){
        bands[["eo:bands"]][[1]][["name"]]
    }) %>% unname()

    # get bands name from href links property
    bands_href <- purrr::map_chr(items$features[[1]]$assets, function(bands){

        # regex pattern to extract band name
        regex_pattern <- "[^\\/|^_\\&\\?]+\\.\\w{3,4}(?=([\\?&].*$|$))"
        file_name <- bands[["href"]][[1]]

        # extract band name and extension
        reg_band <- regexpr(regex_pattern, file_name, perl = TRUE)

        # remove extension
        tools::file_path_sans_ext(substring(file_name, reg_band))

    }) %>% unname()

    # checks if the bands in the name property match with the name of the bands
    # in the href links. This is necessary because some products have bands with
    # different href names
    if (!all(toupper(bands_product) %in% toupper(bands_href))) {

        bands_product <- bands_href
        items$features <- purrr::map(items$features, function(features) {
            names(features$assets) <- bands_product

            features
        })
    }

    # selects the subset of bands supported by sits
    bands_product <- bands_product[bands_product %in% bands_sensor]

    if (length(bands_product) == 0)
        stop(paste("The bands contained in this product are not mapped",
                   "in the SITS package, if you want to include them,",
                   "please provide a configuration file."))

    # store bands product in bands attribute
    items$bands <- toupper(bands_product)

    # checks if the supplied bands match the product bands
    if (!purrr::is_null(bands)) {

        # converting to upper bands
        assertthat::assert_that(all(bands %in% items$bands),
                                msg = paste("The supplied bands do not match",
                                            "the data cube bands."))

        items$bands <- items$bands[items$bands %in% bands]
    }

    return(items)
}
#' @title Get the STAC information corresponding to a tile.
#' @name .sits_deafrica_tile_cube
#' @keywords internal
#'
#' @param url       a \code{character} representing URL for the DEA STAC.
#' @param name      a \code{character} representing the output data cube.
#' @param items     a \code{STACItemCollection} object returned by rstac.
#' @param cube      a \code{character} with name input data cube in DEA
#' @param file_info a \code{tbl_df} with the information from STAC.
#'
#' @return          a \code{tibble} with metadata information about a
#'                  raster data set.
.sits_deafrica_tile_cube <- function(url, name, items, cube, file_info) {
    # store items properties attributes
    item_prop <- items$features[[1]]$properties

    # obtain the timeline
    timeline <- unique(lubridate::as_date(file_info$date))

    # set the labels
    labels <- c("NoClass")

    # obtain bbox extent
    bbox <- .sits_stac_get_bbox(items, item_prop[["proj:epsg"]])

    # get the bands
    bands <- items[["bands"]]

    # get the instrument name
    sensor <- item_prop[["instruments"]]

    # get resolution
    res <- list(xres = item_prop[["gsd"]], yres = item_prop[["gsd"]])
    if (length(item_prop[["gsd"]]) == 0)
        res[c("xres", "yres")] <- .sits_config_resolution(sensor)

    # create a tibble to store the metadata
    cube <- .sits_cube_create(
        type  = "DEAFRICA",
        URL       = url,
        satellite = item_prop[["platform"]],
        sensor    = sensor,
        name      = name,
        cube      = cube,
        tile      = item_prop[["odc:region_code"]],
        bands     = bands,
        labels    = labels,
        scale_factors  = .sits_config_scale_factors(sensor, bands),
        missing_values = .sits_config_missing_values(sensor, bands),
        minimum_values = .sits_config_minimum_values(sensor, bands),
        maximum_values = .sits_config_maximum_values(sensor, bands),
        timelines = list(timeline),
        nrows = item_prop[["proj:shape"]][[1]],
        ncols = item_prop[["proj:shape"]][[2]],
        xmin = bbox$xmin,
        xmax = bbox$xmax,
        ymin = bbox$ymin,
        ymax = bbox$ymax,
        xres = res[["xres"]],
        yres = res[["yres"]],
        crs = item_prop[["proj:epsg"]],
        file_info = file_info)

    return(cube)
}
