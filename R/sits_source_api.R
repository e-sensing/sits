#' @title Test access in API services
#' @name .source_access_test
#'
#' @keywords internal
#'
#' @description Checks that online API services like STAC and wtss are available
#'  for access.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS", "USGS"
#'                    "DEAFRICA", "PROBS").
#' @param collection Collection to be searched in the data source
#'
#' @return An error if service is unreachable or a invisible null otherwise.
.source_access_test <- function(source, collection, ...) {

    s <- .source_new(source = source)

    UseMethod(".source_access_test", s)
}

#' @title Convert bands names as sits bands
#' @name .source_bands_to_sits
#' @keywords internal
#'
#' @description Convert bands used by the origin data cube to the name used by
#'  SITS
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param collection Collection to be searched in the data source
#' @param bands      Bands requested to be read
#'
#' @return  an \code{character} with converted bands.
.source_bands_to_sits <- function(source, collection, bands) {

    # bands sits
    bands_sits <- .config_bands(source, collection)
    names(bands_sits) <- bands_sits

    # bands source
    bands_to_sits <- bands_sits
    names(bands_to_sits) <-  .config_bands_band_name(source = source,
                                                     collection = collection)

    bands_converter <- c(bands_to_sits, bands_sits)

    # are the bands specified as cloud provider bands or as sits bands?
    assertthat::assert_that(
        all(bands %in% names(bands_converter)),
        msg = paste(".source_bands_to_sits: required bands not",
                    "available in", source))

    return(unname(bands_converter[bands]))
}

#' @title Convert bands names as source names
#' @name .source_bands_to_source
#' @keywords internal
#'
#' @description Convert bands used by sits to source names.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param collection Collection to be searched in the data source.
#' @param bands      Bands requested to be read.
#'
#' @return an \code{character} with converted bands.
.source_bands_to_source <- function(source, collection, bands) {

    # bands sits
    bands_source <- .config_bands_band_name(source = source,
                                            collection = collection)
    names(bands_source) <- bands_source

    # bands source
    bands_to_source <- bands_source
    names(bands_to_source) <- .config_bands(source, collection)

    bands_converter <- c(bands_to_source, bands_source)

    # are the bands specified as cloud provider bands or as sits bands?
    assertthat::assert_that(
        all(bands %in% names(bands_converter)),
        msg = paste(".source_bands_to_source: required bands not",
                    "available in", source))

    return(unname(bands_converter[bands]))
}

#' @title Create a data cube
#' @name .source_cube
#' @keywords internal
#'
#' @description Generic function responsible for creating data cubes in sits.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param ...    Additional parameters.
#'
#' @return a sits cube.
.source_cube <- function(source, ...) {

    s <- .source_new(source)

    UseMethod(".source_cube", s)
}

#' @title Function for retrieving information from a single item
#' @name source_item_get_functions
#' @keywords internal
#'
#' @description Generic function for retrieving information in item returned by
#'  APIs. The concept of items is used in STAC APIs, where each item corresponds
#'  to a satellite image (scene), with a single time and multiple bands.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param items a \code{STACItemCollection} object returned by rstac.
#' @param ... Additional parameters.
#' @param collection Collection to be searched in the data source.
#'
#' @return an atomic \code{vector} with the required information.
NULL

#' @rdname source_item_get_functions
.source_item_get_date <- function(source, item, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_date", s)
}

#' @rdname source_item_get_functions
.source_item_get_hrefs <- function(source, item, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_hrefs", s)
}

#' @rdname source_item_get_functions
.source_item_get_bands <- function(source, item, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_bands", s)
}

#' @rdname source_item_get_functions
.source_item_get_resolutions <- function(source, item, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_resolutions", s)
}

#' @title Create an items object
#' @name .source_items_new
#' @keywords internal
#'
#' @description Create an items object. In the case of STAC APIs, this function
#'  is responsible for making the request to the server.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param collection Collection to be searched in the data source.
#' @param ...        Additional parameters.
#'
#' @return a \code{STACItemCollection} object returned by rstac.
.source_items_new <- function(source, collection, ...) {

    s <- .source_new(source)

    UseMethod(".source_items_new", s)
}

#' @title Item selection from Bands
#' @name .source_items_bands_select
#' @keywords internal
#'
#' @description Selection of items from specific bands by the user.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param collection Collection to be searched in the data source.
#' @param items      A \code{STACItemCollection} object returned by rstac.
#' @param bands      A \code{character} with bands to be select in items object.
#' @param ...        Additional parameters.
#'
#' @return A \code{STACItemCollection} object returned by rstac with items
#'  selected.
.source_items_bands_select <- function(source, collection, items, bands, ...) {

    s <- .source_new(source)

    UseMethod(".source_items_bands_select", s)
}

#' @title Create an info file from items
#' @name .source_items_fileinfo
#' @keywords internal
#'
#' @description Creates the fileinfo specification from user-supplied items. In
#'  case of STAC cubes, the items are rstac objects. In case of local cubes, the
#'  items are user-supplied directories.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param items      A \code{STACItemCollection} object returned by rstac or
#'  \code{character} vector with directories to be search.
#' @param ...        Additional parameters.
#' @param collection Collection to be searched in the data source.
#'
#' @return ...
.source_items_fileinfo <- function(source, items, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_fileinfo", s)
}

#' @title Function for retrieving information from multiples items
#' @name source_items_get_functions
#' @keywords internal
#'
#' @description Retrieves information from the STACItemCollection object in the
#' rstac package. Generic function created to handle different STAC providers.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param items      A \code{STACItemCollection} object returned by rstac or
#'  \code{character} vector with directories to be search.
#' @param ...        Additional parameters.
#' @param collection Collection to be searched in the data source.
#'
#' @return an \code{atomic} vector with informations retrived from items.
NULL

#' @rdname source_items_get_functions
.source_items_tiles_group <- function(source, items, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_tiles_group", s)
}

#' @rdname source_items_get_functions
.source_items_get_sensor <- function(source, items, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_get_sensor", s)
}

#' @rdname source_items_get_functions
.source_items_get_satellite <- function(source, items, ..., collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_get_satellite", s)
}

#' @title #' @title Function for retrieving information from multiples items
#' related to a single tile.
#' @name source_tile_items_get_functions
#' @keywords internal
#'
#' @description Function to retrieves informations from the STACItemCollection
#' object in the rstac package, but related from a single tile.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param items      A \code{STACItemCollection} object returned by rstac or
#'  \code{character} vector with directories to be search.
#' @param ...        Additional parameters.
#' @param collection Collection to be searched in the data source.
#'
#' @return an \code{atomic} vector with informations retrived from items.
NULL

#' @rdname source_tile_items_get_functions
.source_items_tile_get_crs <- function(source,
                                       tile_items, ...,
                                       collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_tile_get_crs", s)
}

#' @rdname source_tile_items_get_functions
.source_items_tile_get_name <- function(source,
                                        tile_items, ...,
                                        collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_tile_get_name", s)
}

#' @rdname source_tile_items_get_functions
.source_items_tile_get_bbox <- function(source,
                                        tile_items, ...,
                                        collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_tile_get_bbox", s)
}

#' @rdname source_tile_items_get_functions
.source_items_tile_get_size <- function(source,
                                        tile_items, ...,
                                        collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_tile_get_size", s)
}

#' @title Create a cube object for each tile
#' @name .source_items_cube
#' @keywords internal
#'
#' @description Create a sits cube object for tile, at the end the tiles are
#'  merged to form a single object.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#' @param collection Collection to be searched in the data source.
#' @param name       A \code{character} with cube name.
#' @param items      A \code{STACItemCollection} object returned by rstac or
#'  \code{character} vector with directories to be search.
#' @param file_info  A \code{tibble} with informations about datetime, bands,
#' res, and path.
#' @param ...        Additional parameters.
#'
#' @return A \code{tibble} with the cube class created.
.source_items_cube <- function(source,
                               collection,
                               name,
                               items,
                               file_info, ...) {

    s <- .source_new(source)

    UseMethod(".source_items_cube", s)
}

#' @title Creates a s3 class
#' @name .source_new
#' @keywords internal
#'
#' @description Create an S3 class with information returned from the
#'  configuration file.
#'
#' @param source     Data source (one of "SATVEG", "LOCAL", "BDC", "AWS",
#'                   "USGS", "DEAFRICA", "PROBS").
#'
#' @return a \code{character} with a specified class.
.source_new <- function(source) {

    class(source) <- .config_source_s3class(source)

    return(source)
}
