#' @title Get information from items
#' @name .sits_aws_items
#' @keywords internal
#'
#' @param url        a \code{character} representing a URL for the AWS catalog.
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
.sits_aws_items <- function(url        = NULL,
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

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles)) {
        sep_tile <- .sits_aws_tiles(tiles)

        rstac_query <- rstac_query %>%
            rstac::ext_query("sentinel:utm_zone" %in% sep_tile$utm_zone,
                             "sentinel:latitude_band" %in% sep_tile$lat_band,
                             "sentinel:grid_square" %in% sep_tile$grid_square,
                             ...)
    }

    # making the request
    items_info <- rstac_query %>% rstac::post_request()

    # checks if the collection returned any items
    assertthat::assert_that(!(rstac::items_length(items_info) == 0),
                            msg = "The provided search returned 0 items found.")

    # progress bar status
    pgr_fetch  <- FALSE

    # if more than 1000 items are found the progress bar is displayed
    if (rstac::items_matched(items_info) > 1000)
        pgr_fetch <- TRUE

    # fetching all the metadata and updating to upper case instruments
    items_info <- items_info %>% rstac::items_fetch(progress = pgr_fetch)

    # getting bands name
    items_info <- .sits_aws_bands(items_info, bands)

    # store tile info in items object
    items_info$features <- purrr::map(items_info$features, function(features) {
        features$properties$tile <- paste0(
            features$properties[["sentinel:utm_zone"]],
            features$properties[["sentinel:latitude_band"]],
            features$properties[["sentinel:grid_square"]])

        features
    })

    return(items_info)
}
#' @title Verify items tiles
#' @keywords internal
#'
#' @param tiles  Tile names to be searched.
#'
#' @return a \code{tibble} with information of tiles to be searched in STAC AWS.
.sits_aws_tiles <- function(tiles) {

    # regex pattern
    pattern_s2 <- "[0-9]{2}[A-Z]{3}"

    # verify tile pattern
    if (!any(grepl(pattern_s2, tiles, perl = TRUE)))
        stop(paste("The specified tiles do not match the Sentinel-2A grid",
                   "pattern. See the user guide for more information."))

    # list to store the info about the tiles to provide the query in STAC
    list_tiles <- list()
    list_tiles <- purrr::map(tiles, function(tile) {
        list_tiles$utm_zone = substring(tile, 1, 2)
        list_tiles$lat_band = substring(tile, 3, 3)
        list_tiles$grid_square = substring(tile, 4, 5)

        list_tiles
    })

    tiles_tbl <- dplyr::bind_rows(list_tiles)

    return(tiles_tbl)
}
#' @title Get bands names from items
#' @name .sits_aws_bands
#' @keywords internal
#'
#' @param items      a \code{STACItemCollection} object returned by rstac
#' package.
#' @param bands      a \code{character} vector with the bands name.
#'
#' @return           a \code{STACItemCollection} object representing the search
#'                   by rstac.
.sits_aws_bands <- function(items, bands) {

    sensor <- toupper(items$features[[1]]$properties$instruments)

    # get bands from sensor
    bands_sensor <- .sits_config_sensor_bands(sensor = sensor,
                                              cube = "S2_L2_AWS")

    # get bands name from assets list name property
    bands_product <- names(items$features[[1]]$assets)

    # selects the subset of bands supported by sits
    bands_product <- bands_product[bands_product %in% bands_sensor]

    if (length(bands_product) == 0)
        stop(paste("The bands contained in this product are not mapped",
                   "in the SITS package, if you want to include them,",
                   "please provide a configuration file."))

    # store bands product in bands attribute
    items$bands <- bands_product

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
#' @title Check bands by resolution
#' @name .sits_aws_check_bands
#' @keywords internal
#'
#' @param bands         a \code{character} vector with the bands name.
#' @param s2_resolution a \code{character} withh resolution of S2 images
#'  ("10m", "20m" or "60m")
#'
#' @return           a \code{character} vector with the selected bands.
.sits_aws_check_bands <- function(bands, s2_resolution) {

    # bands supported by provided resolution
    bands_s2 <- .sits_config_s2_bands(s2_resolution)

    if (!purrr::is_null(bands)) {
        assertthat::assert_that(all(bands %in% bands_s2),
                                msg = paste("The provided bands do not match",
                                            "to the bands supported by the",
                                            s2_resolution, "resolution")
        )
        bands_s2 <- bands_s2[bands_s2 %in% bands]
    }

    return(bands_s2)
}
#' @title Get bands names from items
#' @name .sits_aws_bands
#' @keywords internal
#'
#' @param fileinfo   a \code{tibble} with date, band and path information.
#' @param resolution a \code{character} with resolution of S2 images
#'  ("10m", "20m" or "60m")
#'
#' @return a \code{tibble} with date, band, res and path information,
#'  arranged by the date.
.sits_aws_add_res <- function(file_info, resolution) {

    # Adding the spatial resolution in the band URL
    file_info <-
        dplyr::mutate(file_info,
                      path = gsub("R[0-9]{2}m", paste0("R", resolution), path),
                      res = resolution, .before = "date")
    return(file_info)
}

#' @title Get the STAC information corresponding to a tile.
#' @name .sits_aws_tile_cube
#' @keywords internal
#'
#' @param url        a \code{character} representing URL for the AWS STAC.
#' @param name       a \code{character} representing the output data cube.
#' @param items      a \code{STACItemCollection} object returned by rstac.
#' @param cube       a \code{character} with name input data cube in AWS
#' @param resolution a \code{character} withh resolution of S2 images
#'  ("10m", "20m" or "60m")
#' @param file_info  a \code{tbl_df} with the information from STAC.
#'
#' @return           a \code{tibble} with metadata information about a
#'                   raster data set.
.sits_aws_tile_cube <- function(url, name, items, cube, resolution, file_info) {

    # store items properties attributes
    item_prop <- items$features[[1]]$properties

    # obtain the timeline
    timeline <- unique(lubridate::as_date(file_info$date))

    # set the labels
    labels <- c("NoClass")

    file_info <-.sits_aws_add_res(file_info, resolution)

    # get the bands
    bands <- items[["bands"]]

    # get the first image
    # obtain the parameters
    params <- .sits_raster_api_params_file(file_info$path[1])

    # get the instrument name
    sensor <- toupper(item_prop[["instruments"]])

    # get resolution
    res <- list(xres = item_prop[["gsd"]], yres = item_prop[["gsd"]])
    if (length(item_prop[["gsd"]]) == 0)
        res[c("xres", "yres")] <- .sits_config_resolution(sensor)

    # create a tibble to store the metadata
    cube <- .sits_cube_create(
        type  = "S2_L2A_AWS",
        URL       = url,
        satellite = toupper(item_prop[["constellation"]]),
        sensor    = sensor,
        name      = name,
        cube      = cube,
        tile      = item_prop$tile,
        bands     = bands,
        labels    = labels,
        scale_factors  = .sits_config_scale_factors(sensor, bands),
        missing_values = .sits_config_missing_values(sensor, bands),
        minimum_values = .sits_config_minimum_values(sensor, bands),
        maximum_values = .sits_config_maximum_values(sensor, bands),
        timelines = list(timeline),
        nrows = params$nrows,
        ncols = params$ncols,
        xmin = params$xmin,
        xmax = params$xmax,
        ymin = params$ymin,
        ymax = params$ymax,
        xres = res[["xres"]],
        yres = res[["yres"]],
        crs = item_prop[["proj:epsg"]],
        file_info = file_info)

    return(cube)
}
