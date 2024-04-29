# ---- source api ----
#' @title Create an items object in an DEAfrica cube
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
.source_items_new.deafrica_cube <- function(source, ...,
                                            collection,
                                            stac_query,
                                            tiles = NULL,
                                            platform = NULL) {
    # Convert roi to bbox
    roi <- .stac_intersects_as_bbox(stac_query)
    stac_query[["params"]][["intersects"]] <- NULL
    stac_query[["params"]][["bbox"]] <- roi$bbox
    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    .check_stac_items(items_info)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) >
        2 * .conf("rstac_pagination_limit")
    # check documentation mode
    progress <- .check_documentation(progress)

    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = progress)
    # checks if the items returned any items
    .check_stac_items(items_info)
    return(items_info)
}
#' @title Create an items object in an DEAfrica cube
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param tiles      Selected tiles (optional)
#' @param platform   Satellite platform (optional).
#' @return An object referring the images of a sits cube.
#' @export
`.source_items_new.deafrica_cube_sentinel-2-l2a` <- function(source, ...,
                                                     collection,
                                                     stac_query,
                                                     tiles = NULL,
                                                     platform = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new")
    # check platform
    if (!is.null(platform)) {
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )
    }
    # check spatial extensions
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- c(roi[["lon_min"]],
                                              roi[["lat_min"]],
                                              roi[["lon_max"]],
                                              roi[["lat_max"]])
    } else {
        roi <- .stac_intersects_as_bbox(stac_query)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- roi$bbox
    }
    # make request
    items_info <- rstac::post_request(q = stac_query, ...)
    items_info <- rstac::items_fetch(items = items_info, progress = FALSE)
    # filter items
    items_info <- rstac::items_filter(items_info,
                                      filter_fn = function(feature) {
        lgl_res <- TRUE

        if (!is.null(platform)) {
            lgl_res <- feature[["properties"]][["platform"]] == platform
        }

        lgl_res
    })
    # check results
    .check_stac_items(items_info)
    # done
    items_info
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_new.deafrica_cube_sentinel-1-rtc` <- function(
                                                         source, ...,
                                                         collection,
                                                         stac_query,
                                                         tiles = NULL,
                                                         platform = NULL,
                                                         orbit = NULL) {
    # set caller to show in errors
    .check_set_caller(".source_items_new")
    # check orbits
    orbits <- .conf("sources", source, "collections", collection, "orbits")
    .check_chr_within(orbit, orbits)
    # check platform
    if (!is.null(platform)) {
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )
    }
    # check spatial extensions
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- c(roi[["lon_min"]],
                                              roi[["lat_min"]],
                                              roi[["lon_max"]],
                                              roi[["lat_max"]])
    } else {
        roi <- .stac_intersects_as_bbox(stac_query)
        stac_query[["params"]][["intersects"]] <- NULL
        stac_query[["params"]][["bbox"]] <- roi$bbox
    }
    # make request
    items_info <- rstac::post_request(q = stac_query, ...)
    items_info <- rstac::items_fetch(items = items_info, progress = FALSE)
    # filter items
    items_info <- rstac::items_filter(items_info,
                                      filter_fn = function(feature) {
        lgl_res <- feature[["properties"]][["sat:orbit_state"]]     == orbit &&
                   feature[["properties"]][["sar:instrument_mode"]] == "IW"  &&
                   feature[["properties"]][["sar:frequency_band"]]  == "C"

        if (!is.null(platform)) {
            lgl_res <- lgl_res &&
                       feature[["properties"]][["platform"]] == platform
        }

        lgl_res
    })
    # check results
    .check_stac_items(items_info)
    # done
    items_info
}
#' @keywords internal
#' @noRd
#' @export
`.source_filter_tiles.deafrica_cube_sentinel-1-rtc` <- function(source,
                                                                collection,
                                                                cube,
                                                                tiles) {
    return(cube)
}
#' @keywords internal
#' @noRd
#' @export
.source_items_tile.deafrica_cube <- function(source, ...,
                                             items,
                                             collection = NULL) {
    rstac::items_reap(items, field = c("properties", "odc:region_code"))
}
#' @keywords internal
#' @noRd
#' @export
.source_item_get_date.deafrica_cube <- function(source,
                                                item,
                                                ...,
                                                collection = NULL) {
    item_date <- item[[c("properties", "datetime")]]

    # Digital Earth Africa provides some products with the `properties.datetime`
    # property `null`. In those cases, it is required to use other date
    # parameter available
    if (is.null(item_date))
        item_date <- item[[c("properties", "start_datetime")]]

    suppressWarnings(
        lubridate::as_date(item_date)
    )
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.deafrica_cube_rainfall-chirps-daily` <-
    function(source, items, ..., collection = NULL) {
        rep("NoTilingSystem", rstac::items_length(items))
}
#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.deafrica_cube_rainfall-chirps-monthly` <-
    function(source, items, ..., collection = NULL) {
    rep("NoTilingSystem", rstac::items_length(items))
}
