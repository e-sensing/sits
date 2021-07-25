#' @title ...
#' @name .source_access_test
#' @keywords internal
#'
#' @description ...
#'
#' @param source     Name of the provider
#'
#' @return   ...
.source_access_test <- function(source, collection, ...) {

    s <- .source_new(source = source)

    UseMethod(".source_access_test", s)
}

#' @title Convert bands names from cube to SITS
#' @name .source_bands_to_sits
#' @keywords internal
#'
#' @description Convert the name of the band used by the origin data cube
#'              to the name used by SITS
#' @param source     Name of the STAC provider
#' @param collection Name of sensor
#' @param bands      Bands requested to be read
#'
#' @return          Data cube tile with SITS bands
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

#' @title Convert bands names from cube to SITS
#' @name .source_bands_to_source
#' @keywords internal
#'
#' @description Convert the name of the band used by the origin data cube
#'              to the name used by SITS
#' @param source     Name of the STAC provider
#' @param collection Name of sensor
#' @param bands      Bands requested to be read
#'
#' @return          Data cube tile with SITS bands
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

#' @title Convert bands names from cube to SITS
#' @name .source_cube
#' @keywords internal
#'
#' @description TODO: add
#'
#' @param source Name of the STAC provider
#' @param ...    Aditional parameters
#'
#' @return Data cube tile with SITS bands
.source_cube <- function(source, ...) {

    s <- .source_new(source)

    UseMethod(".source_cube", s)
}

#' @title ...
#' @name source_item_get_functions
#' @keywords internal
#'
#' @description TODO: add
#'
#' @param source Name of the STAC provider
#' @param item ...
#' @param ... Aditional parameters
#' @param collection ...
#'
#' @return ...
NULL

#' @rdname source_item_get_functions
.source_item_get_date <- function(source,
                                  item, ...,
                                  collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_date", s)
}

#' @rdname source_item_get_functions
.source_item_get_hrefs <- function(source,
                                   item, ...,
                                   collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_hrefs", s)
}

#' @rdname source_item_get_functions
.source_item_get_bands <- function(source,
                                   item, ...,
                                   collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_bands", s)
}

#' @rdname source_item_get_functions
.source_item_get_resolutions <- function(source,
                                         item, ...,
                                         collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_item_get_resolutions", s)
}

#' @title ...
#' @name .source_items_new
#' @keywords internal
#'
#' @description TODO: document
#'
#' @param source     Name of the STAC provider
#' @param collection ...
#' @param ... ...
#'
#' @return ...
.source_items_new <- function(source, collection, ...) {

    s <- .source_new(source)

    UseMethod(".source_items_new", s)
}

#' @title ...
#' @name .source_items_bands_select
#' @keywords internal
#'
#' @description TODO: document
#'
#' @param source Name of the STAC provider
#' @param collection ...
#' @param items ...
#' @param bands ...
#' @param ... ...
#'
#' @return ...
.source_items_bands_select <- function(source,
                                       collection,
                                       items,
                                       bands, ...) {

    s <- .source_new(source)

    UseMethod(".source_items_bands_select", s)
}

#' @title ...
#' @name .source_items_fileinfo
#' @keywords internal
#'
#' @description TODO: document
#'
#' @param source Name of the STAC provider
#' @param items ...
#' @param collection ...
#'
#' @return ...
.source_items_fileinfo <- function(source,
                                   items, ...,
                                   collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_fileinfo", s)
}

#' @title ...
#' @name source_items_get_functions
#' @keywords internal
#'
#' @description TODO: add
#'
#' @param source Name of the STAC provider
#' @param items ...
#' @param ... Aditional parameters
#' @param collection ...
#'
#' @return ...
NULL

#' @rdname source_items_get_functions
.source_items_tiles_group <- function(source,
                                      items, ...,
                                      collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_tiles_group", s)
}

#' @rdname source_items_get_functions
.source_items_get_sensor <- function(source,
                                     items, ...,
                                     collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_get_sensor", s)
}

#' @rdname source_items_get_functions
.source_items_get_satellite <- function(source,
                                        items, ...,
                                        collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_get_satellite", s)
}

#' @rdname source_items_get_functions
.source_items_tile_get_crs <- function(source,
                                       tile_items, ...,
                                       collection = NULL) {

    s <- .source_new(source)

    UseMethod(".source_items_tile_get_crs", s)
}

#' @title ...
#' @name source_tile_items_get_functions
#' @keywords internal
#'
#' @description TODO: add
#'
#' @param source Name of the STAC provider
#' @param items ...
#' @param ... Aditional parameters
#' @param collection ...
#'
#' @return ...
NULL

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

#' @title ...
#' @name .source_items_tile_cube
#' @keywords internal
#'
#' @description TODO: document
#'
#' @param source Name of the STAC provider
#' @param collection ...
#' @param name ...
#' @param tile_items ...
#' @param file_info ...
#'
#' @return ...
.source_items_tile_cube <- function(source,
                                    collection,
                                    name,
                                    tile_items,
                                    file_info) {

    s <- .source_new(source)

    UseMethod(".source_items_tile_cube", s)
}

#' @title ...
#' @name .source_new
#' @keywords internal
#'
#' @description TODO: document
#'
#' @param source Name of the STAC provider
#'
#' @return ...
.source_new <- function(source) {

    class(source) <- .config_source_s3class(source)

    return(source)
}
