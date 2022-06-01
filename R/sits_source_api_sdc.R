#' @keywords internal
#' @export
.source_items_new.sdc_cube <- function(source, ...,
                                       collection,
                                       stac_query,
                                       tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.sdc_cube")

    if (!is.null(tiles)) {
        stop(paste("SDC cubes do not support searching for tiles, use",
                   "'roi' parameter instead.",
                   call. = FALSE
        ))
    }

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)
    # if more than 2 times items pagination are found the progress bar
    # is displayed
    progress <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # check documentation mode
    progress <- .check_documentation(progress)

    # fetching all the metadata and updating to upper case instruments
    items_info <- rstac::items_fetch(items = items_info, progress = progress)
    # checks if the items returned any items
    .check_that(
        x = rstac::items_length(items_info) != 0,
        msg = paste(
            "the provided search returned 0 items. Please, verify",
            "the provided parameters."
        )
    )
    return(items_info)
}

#' @keywords internal
#' @export
.source_items_tile.sdc_cube <- function(source, ...,
                                             items,
                                             collection = NULL) {
    rstac::items_reap(items, field = c("properties", "cubedash:region_code"))
}


#' @keywords internal
#' @export
.source_item_get_hrefs.sdc_cube <- function(source,
                                            item, ...,
                                            collection = NULL) {
    hrefs <- unname(purrr::map_chr(item[["assets"]], `[[`, "href"))
    asset_names <- unlist(
        purrr::map(item[["assets"]], `[[`, c("eo:bands")),
        use.names = FALSE
    )

    # post-conditions
    .check_chr(hrefs, allow_empty = FALSE)
    # add gdal VSI in href urls
    vsi_hrefs <- .stac_add_gdal_fs(hrefs)
    vsi_hrefs <- sprintf('%s:"%s":%s', "NETCDF", vsi_hrefs, asset_names)

    return(hrefs)
}
