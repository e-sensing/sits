#' @keywords internal
#' @export
.source_item_get_hrefs.bdc_cube <- function(source, ...,
                                            item,
                                            collection = NULL) {

    access_key <- Sys.getenv("BDC_ACCESS_KEY")

    href <- paste0(unname(purrr::map_chr(item[["assets"]], `[[`, "href")),
                   "?access_token=", access_key)

    # add gdal vsi in href urls
    return(.stac_add_gdal_fs(href))
}



#' @keywords internal
#' @export
.source_items_new.bdc_cube <- function(source, ...,
                                       collection,
                                       stac_query,
                                       tiles = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.bdc_cube")

    # if specified, a filter per tile is added to the query
    if (!is.null(tiles))
        stac_query <- rstac::ext_query(q = stac_query, "bdc:tile" %in% tiles)

    # making the request
    items_info <- rstac::post_request(q = stac_query, ...)

    # check if matched items
    .check_that(
        x = rstac::items_matched(items_info) > 0,
        msg = "no items matched the query criteria."
    )

    # if more than 2 times items pagination are found the progress bar
    # is displayed
    pgr_fetch <- rstac::items_matched(items_info) > 2 * .config_rstac_limit()

    # fetching all the metadata
    items_info <- rstac::items_fetch(items = items_info, progress = pgr_fetch)

    return(items_info)
}

#' @keywords internal
#' @export
.source_items_tile.bdc_cube <- function(source, ...,
                                        items,
                                        collection = NULL) {

    rstac::items_reap(items, field = c("properties", "bdc:tiles"))
}

#' @keywords internal
#' @export
.source_items_cube.bdc_cube <- function(source,
                                        items, ...,
                                        collection = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_cube.bdc_cube")

    items_lst <- rstac::items_group(items, field = c("properties", "bdc:tiles"))

    cube <- purrr::map_dfr(items_lst, function(items) {

        file_info <- purrr::map_dfr(items$features, function(item){

            date <-  .source_item_get_date(source = source,
                                           item = item, ...,
                                           collection = collection)

            tile <- .source_item_get_tile(source = source, ...,
                                          item = item,
                                          collection = collection)

            fid <- .source_item_get_fid(source = source, ...,
                                        item = item,
                                        collection = collection)

            bands <- .source_item_get_bands(source = source, ...,
                                            item = item,
                                            collection = collection)

            res <- .source_item_get_resolution(source = source, ...,
                                               item = item,
                                               collection = collection)

            paths <- .source_item_get_hrefs(source = source, ...,
                                            item = item,
                                            collection = collection)

            bbox <- .source_item_get_bbox(source = source, ...,
                                          item = item,
                                          collection = collection)

            rast_size <- .source_item_get_size(source = source, ...,
                                               item = item,
                                               collection = collection)

            cloud_cover <- .source_item_get_cloud_cover(source = source, ...,
                                                        item = item,
                                                        collection = collection)

            crs <- .source_item_get_crs(source = source, ...,
                                        item = item,
                                        collection = collection)

            .check_that(
                x = !is.na(tile),
                msg = "invalid tile format."
            )

            .check_that(
                x = !is.na(fid),
                msg = "invalid fid format."
            )

            .check_that(
                x = !is.na(date),
                msg = "invalid date format."
            )

            .check_that(
                x = is.character(bands),
                msg = "invalid band format."
            )

            .check_that(
                x = is.numeric(res),
                msg = "invalid res format."
            )

            .check_that(
                x = is.character(paths),
                msg = "invalid path format."
            )

            .check_that(
                x = is.numeric(bbox),
                msg = "invalid bbox format."
            )

            .check_that(
                x = is.numeric(rast_size),
                msg = "invalid bbox format."
            )

            .check_that(
                x = length(cloud_cover) == 1,
                msg = "invalid cloud cover."
            )

            tidyr::unnest(
                tibble::tibble(
                    tile = tile,
                    fid = fid,
                    date = date,
                    band = list(bands),
                    res = tibble::as_tibble_row(res),
                    bbox = tibble::as_tibble_row(bbox),
                    size = tibble::as_tibble_row(rast_size),
                    crs = crs,
                    path = list(paths),
                    cloud_cover = cloud_cover
                ), cols = c("band", "res", "bbox", "size", "path", "cloud_cover")
            )
        })

        bbox <- .source_tile_get_bbox(source = source,
                                      file_info = file_info,
                                      collection = collection)

        file_info <-  dplyr::arrange(file_info, .data[["date"]],
                                     .data[["fid"]], .data[["band"]])

        # create cube row
        tile <- .cube_create(
            source     = source,
            collection = collection,
            satellite  = .source_collection_satellite(source, collection),
            sensor     = .source_collection_sensor(source, collection),
            tile       = file_info[["tile"]][[1]],
            xmin       = bbox[["xmin"]],
            xmax       = bbox[["xmax"]],
            ymin       = bbox[["ymin"]],
            ymax       = bbox[["ymax"]],
            crs        = file_info[["crs"]][[1]],
            file_info  = file_info)

        return(tile)
    })

    return(cube)
}

#' @keywords internal
#' @export
.source_item_get_resolution.bdc_cube <- function(source,
                                                 item, ...,
                                                 collection = NULL) {
    return(
        c(xres = item[[c("properties", "eo:gsd")]],
          yres = item[[c("properties", "eo:gsd")]])
    )
}

#' @keywords internal
#' @export
.source_item_get_crs.bdc_cube <- function(source, ...,
                                          item,
                                          collection = NULL) {

    # making request to collection endpoint to get crs info
    url <- .source_url(source = source)
    query_search <- rstac::collections(q = rstac::stac(url),
                                       collection_id = collection)

    col <- rstac::get_request(q = query_search)

    return(col[["bdc:crs"]])
}

#' @keywords internal
#' @export
.source_item_get_bbox.bdc_cube <- function(source,
                                           item, ...,
                                           collection = NULL) {

    extent_points <- item[["geometry"]][["coordinates"]][[1]]

    crs <- .source_item_get_crs(source = source, ...,
                                tile_items = item,
                                collection = collection)

    polygon_ext <- sf::st_polygon(list(do.call(rbind, extent_points)))
    polygon_ext <- sf::st_transform(sf::st_sfc(polygon_ext, crs = 4326), crs)

    return(c(sf::st_bbox(polygon_ext)))
}

#' @keywords internal
#' @export
.source_item_get_size.bdc_cube <- function(source,
                                           item, ...,
                                           collection = NULL) {

    return(
        c(nrows = item[["assets"]][[1]][["bdc:raster_size"]][["y"]],
          ncols = item[["assets"]][[1]][["bdc:raster_size"]][["x"]])
    )
}

#' @keywords internal
#' @export
.source_item_get_tile.bdc_cube <- function(source, ...,
                                           item,
                                           collection = NULL) {

    return(item[["properties"]][["bdc:tiles"]])
}


#' @keywords internal
#' @export
.source_item_get_fid.bdc_cube <- function(source, ...,
                                          item,
                                          collection = NULL) {

    return(item[["id"]])
}
