# ---- general cdse utilities ----
#' @title Extract item type of a given `collection`.
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function reads the item type of a `collection` and prepares it for use
#'  in the CDSE Source API's internal methods. This is required as for some
#'  `collection`, CDSE returns multiple kinds of items.
#' @param source     Data source.
#' @param collection Image collection.
#' @return           item type of a given `collection`.
.cdse_item_type <- function(source, collection) {
    item_type <- .conf(
        "sources",
        source,
        "collections",
        collection,
        "item_type"
    )
    class(item_type) <- c("character", item_type)
    item_type
}

#' @title Fix STAC Items from CDSE with assets metadata.
#' @keywords internal
#' @noRd
#'
#' @description
#'  This auxiliary function creates `STAC Assets` for each of the bands selected
#'  by user. This is necessary since CDSE STAC does not return band files as
#'  `STAC Assets` The information is extracted from files available in the
#'  CDSE S3 API.
#' @param source     Data source.
#' @param items      STAC items.
#' @param bands      Band names.
#' @param collection Image collection.
#' @return           STAC Items updated with `assets` property.
.cdse_stac_fix_items <- function(source, items, bands, collection, multicores) {
    .check_set_caller(".cdse_stac_fix_items")
    # Start parallel workers
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop(), add = TRUE)
    # Define path used to extract the product prefix in CDSE S3
    s3_path <- c("properties", "productIdentifier")
    # Define name of the CDSE products bucket
    s3_bucket <- "/eodata/"
    # Define protocol to access S3 data
    s3_protocol <- "/vsis3"
    # CDSE does not provide files directly in the `assets` property`. As a
    # workaround, we build the `assets` property using files in the CDSE S3 API
    features_fixed <- .parallel_map(items[["features"]], function(feature) {
        # Prepare item path in CDSE S3
        item_s3_path <- rstac::items_reap(feature, s3_path) |>
            stringr::str_replace(s3_bucket, "")
        # Read the item's content from CDSE S3
        item_s3_content <- aws.s3::get_bucket_df(
            bucket = "eodata",
            use_https = TRUE,
            region = "",
            prefix = item_s3_path
        )
        # Extract the address of the files associated with bands selected by
        # users.
        item_bands <- purrr::map(bands, function(band) {
            band_conf <- .conf_eo_band(source, collection, band)
            # Create pattern to select file associated with the band.
            band_pattern <- band_conf[["pattern"]]
            # Filter the S3 content to get files from the band
            band_item <-
                dplyr::filter(item_s3_content,
                              stringr::str_detect(.data[["Key"]], band_pattern))
            # Check if the correct file was selected.
            .check_that(nrow(band_item) == 1)
            # Prepare the file address
            band_path_s3 <- paste0(s3_protocol, s3_bucket, band_item[["Key"]])
            # Prepare result and return it
            # As this auxiliary function only needs to provide the right content
            # to other parts of `sits`, only the `href` of the image is returned.
            # The other necessary actions are managed by `sits.`
            stats::setNames(list(band = list(href = band_path_s3)), band)
        })
        feature[["assets"]] <- purrr::list_flatten(item_bands)
        feature
    })
    # Remove duplicates
    features_id <- unlist(purrr::map(features_fixed, "id"))
    features_fixed <- features_fixed[!duplicated(features_id)]
    # Update items object
    items[["features"]] <- features_fixed
    items
}

# ---- source api ----
#' @title Test access to STAC collection
#' @keywords internal
#' @noRd
#'
#' @description
#' These functions provide an API to handle/retrieve data from source's
#' collections.
#'
#' @param source     Data source.
#' @param collection Image collection.
#' @param bands      Band names.
#' @param ...        Other parameters to be passed for specific types.
#' @param start_date Start date.
#' @param end_date   End date.
#' @param dry_run    TRUE/FALSE.
#' @return           Called for side effects
#' @export
.source_collection_access_test.cdse_cube <- function(source,
                                                     collection,
                                                     bands, ...,
                                                     start_date = NULL,
                                                     end_date = NULL,
                                                     dry_run = TRUE) {
    # check if `aws.s3` is installed
    .check_require_packages("aws.s3")
    # as CDSE STAC returns many types of items in the same collection,
    # it is required to filter the content by a specific type.
    item_type <- .cdse_item_type(source, collection)
    # extract collection endpoint
    collection_endpoint <- .conf(
        "sources",
        source,
        "collections",
        collection,
        "collection_name"
    )
    # query Open Search
    items <- .try({
        .opensearch_cdse_search(
            product_type = item_type,
            source = source,
            collection = collection_endpoint,
            start_date = start_date,
            end_date = end_date,
            bbox = NULL,
            paginate = FALSE,
            limit = 1,
            ...
        )
    }, .default = NULL)
    # Check items
    .check_stac_items(items)
    # Test bands and accessibility
    items <- .source_items_bands_select(
        source = source,
        items  = items,
        bands  = bands[[1]],
        collection = collection, ...
    )
    href <- .source_item_get_hrefs(
        source = source,
        item = items$feature[[1]],
        collection = collection, ...
    )
    # assert that token and/or href is valid
    if (dry_run) {
        rast <- .try({.raster_open_rast(href)},
                     default = NULL
        )
        .check_null_parameter(rast)
    }
    return(invisible(source))
}

#' @title Transform an items object in a CDSE cube
#' @keywords internal
#' @noRd
#' @description \code{.source_items_new()} this function is called to create
#' an items object. In case of Web services, this function is responsible for
#' making the Web requests to the server.
#' @param source     Name of the STAC provider.
#' @param ...        Other parameters to be passed for specific types.
#' @param collection Collection to be searched in the data source.
#' @param stac_query Query that follows the STAC protocol
#' @param tiles      Selected tiles.
#' @param orbit      Selected orbit.
#' @param multicores Number of workers used to create the CDSE cube.
#' @param platform   Satellite platform (not supported).
#' @return An object referring the images of a sits cube.
#'
#' @export
.source_items_new.cdse_cube <- function(source, ...,
                                        collection,
                                        stac_query,
                                        tiles,
                                        multicores,
                                        platform) {
    # set caller to show in errors
    .check_set_caller(".source_items_new_cdse_cube")
    # check multicores
    .check_int_parameter(multicores, min = 1, max = 2048)
    # check platform (filter available for CDSE collections supported by sits)
    if (!is.null(platform)) {
        platform <- .stac_format_platform(
            source = source,
            collection = collection,
            platform = platform
        )
    }
    # define the maximum number of records per request
    cdse_query_limit <- 1000
    # as CDSE STAC returns many types of items in the same collection,
    # it is required to filter the content by a specific type.
    item_type <- .cdse_item_type(source, collection)
    # extract collection endpoint
    collection_endpoint <- .conf(
        "sources",
        source,
        "collections",
        collection,
        "collection_name"
    )
    # extract query parameters from STAC Query
    query_bbox <- .stac_intersects_as_bbox(stac_query)
    query_date <- .stac_datetime_as_dates(stac_query)
    # CDSE does not support tiles - convert to ROI
    # Currently, `sits` only supports Sentinel products. In the future, with
    # other products, this must be revised.
    if (!is.null(tiles)) {
        roi <- .s2_mgrs_to_roi(tiles)
        query_bbox$bbox <- c(roi[["lon_min"]],
                             roi[["lat_min"]],
                             roi[["lon_max"]],
                             roi[["lat_max"]]
        )
    }
    .check_null(query_bbox$bbox)
    # Currently CDSE STAC filters are limited. As there is no possibility of
    # using specific selections, sometimes using the first item returned from
    # STAC can be a problem (e.g., Auxiliary products). To avoid this problem,
    # we use the Open Search API.
    items <- .opensearch_cdse_search(
        product_type = item_type,
        source       = source,
        collection   = collection_endpoint,
        start_date   = query_date$start_date,
        end_date     = query_date$end_date,
        bbox         = query_bbox$bbox,
        limit        = cdse_query_limit,
        platform     = platform,
        ...
    )
    # Validate results
    .check_length(items[["features"]], len_min = 1)
    # Done!
    items
}

#' @title Select bands from a STAC item
#' @keywords internal
#' @noRd
#'
#' @param source     Data source
#' @param ...        Additional parameters.
#' @param items      STAC items
#' @param bands      Bands to be selected in the collection.
#' @param collection Image collection
#' @param multicores Number of workers used to extract metadata from CDSE S3.
#' @return           List of STAC items
#' @export
.source_items_bands_select.cdse_cube <- function(source, ...,
                                                 items, bands,
                                                 collection, multicores = 1L) {
    # CDSE does not provide files in the `assets` property. So, it is
    # required to fix this using content from CDSE S3 API.
    items <- .cdse_stac_fix_items(source, items, bands, collection, multicores)
    # With the correct metadata, it is possible to use the base workflow.
    .source_items_bands_select.stac_cube(
        source,
        items,
        bands,
        collection,
        ...
    )
}

#' @keywords internal
#' @noRd
#' @export
.source_items_tile.cdse_cube <- function(source,
                                         ...,
                                         items,
                                         collection = NULL) {
    rstac::items_reap(items, field = c("properties", "tile"))
}

#' @keywords internal
#' @noRd
#' @export
.source_item_get_date.cdse_cube <- function(source,
                                            item,
                                            ...,
                                            collection = NULL) {
    as.Date(rstac::items_reap(item, field = c("properties", "startDate")))
}

#' @keywords internal
#' @noRd
#' @export
.source_item_get_cloud_cover.cdse_cube <- function(source, ...,
                                                   item,
                                                   collection = NULL) {
    rstac::items_reap(item, field = c("properties", "cloudCover"))
}

#' @keywords internal
#' @noRd
#' @export
`.source_items_tile.cdse_cube_sentinel-1-rtc` <- function(source,
                                                          items, ...,
                                                          collection = NULL) {
    rep("NoTilingSystem", rstac::items_length(items))
}

#' @keywords internal
#' @noRd
#' @export
`.source_filter_tiles.cdse_cube_sentinel-1-rtc` <- function(source,
                                                            collection,
                                                            cube,
                                                            tiles) {
    return(cube)
}

#' @keywords internal
#' @noRd
#' @export
`.source_tile_get_bbox.cdse_cube_sentinel-1-rtc` <-
    function(source, file_info, ..., collection = NULL) {
    .check_set_caller(".source_tile_get_bbox_cdse_s1_rtc")
    # pre-condition
    .check_num(nrow(file_info), min = 1)

    # get bbox based on file_info
    xmin <- min(file_info[["xmin"]])
    ymin <- min(file_info[["ymin"]])
    xmax <- max(file_info[["xmax"]])
    ymax <- max(file_info[["ymax"]])

    # post-condition
    .check_that(xmin < xmax && ymin < ymax)
    # create a bbox
    bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    return(bbox)
}
