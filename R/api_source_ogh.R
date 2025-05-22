#' @title Test access to collection in Open Geo Hub
#' @keywords internal
#' @noRd
#' @description
#' These functions provide an API to handle/retrieve data from source's
#' collections.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param bands      Band names
#' @param ...        Other parameters to be passed for specific types.
#' @param start_date Start date.
#' @param end_date   End date.
#' @param dry_run    TRUE/FALSE
#' @return           Called for side effects
#' @export
.source_collection_access_test.ogh_cube <- function(source, collection,
                                                     bands, ...,
                                                     start_date = NULL,
                                                     end_date = NULL,
                                                     dry_run = FALSE) {
    # require package
    .check_require_packages("rstac")
    # query items
    items <- .try(
        {
            .stac_static_items_query(
                source = source,
                collection = collection,
                start_date = start_date,
                end_date = end_date,
                limit = 1
            )
        },
        .default = NULL
    )
    # check items
    .check_stac_items(items)
    # select bands
    items <- .source_items_bands_select(
        source = source,
        items = items,
        bands = bands[[1L]],
        collection = collection, ...
    )
    # get hrefs available
    href <- .source_item_get_hrefs(
        source = source,
        item = items[["features"]][[1L]],
        collection = collection, ...
    )
    # assert that token and/or href is valid
    if (dry_run) {
        rast <- .try(
            {
                .raster_open_rast(href)
            },
            default = NULL
        )
        .check_null_parameter(rast)
    }
    return(invisible(source))
}

#' @title Create an items object using items STAC Static from Open Geo Hub
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param ...        Other parameters to be passed for specific types.
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
.source_items_new.ogh_cube <- function(source,
                                       collection,
                                       stac_query, ...,
                                       tiles = NULL,
                                       platform = NULL) {
    .check_set_caller(".source_items_new_ogh_cube")
    # query items
    datetime <- stac_query[["params"]][["datetime"]]
    datetime <- stringr::str_split(datetime, "/")
    datetime <- datetime[[1]]

    start_date <- datetime[[1]]
    end_date <- datetime[[2]]

    items <- .try(
        {
            .stac_static_items_query(
                source = source,
                collection = collection,
                start_date = start_date,
                end_date = end_date
            )
        },
        .default = NULL
    )
    # check results
    .check_stac_items(items)
    # return
    return(items)
}

#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.ogh_cube_landsat-glad-2m` <- function(source,
                                                         items, ...,
                                                         collection = NULL) {
    rep("NoTilingSystem", rstac::items_length(items))
}
