
# ---- general utilities ----
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

# ---- stac utilities ----
#' @title Extract datetime from a STAC Query.
#' @keywords internal
#' @noRd
#'
#' @param stac_query Query that follows the STAC protocol.
#' @return           List with `start_date` and `end_date` properties.
.stac_cdse_datetime_as_dates <- function(stac_query) {
    query_datetime <- stringr::str_split(
        stac_query[["params"]][["datetime"]], "/"
    )
    list(
        start_date = query_datetime[[1]][1],
        end_date = query_datetime[[1]][2]
    )
}

#' @title Extract bounding box from a STAC Query.
#' @keywords internal
#' @noRd
#'
#' @param stac_query Query that follows the STAC protocol.
#' @return           List with `bbox` property.
.stac_cdse_intersects_as_bbox <- function(stac_query) {
    result <- list(bbox = NULL)
    # Extract spatial reference from STAC object
    intersects <- stac_query[["params"]][["intersects"]]
    coordinates <- intersects[["coordinates"]]
    # Check if query is valid
    if (is.null(coordinates)) {
        return(result)
    }
    # Extract x-coordinates and y-coordinates
    coordinates_x <- coordinates[,,1]
    coordinates_y <- coordinates[,,2]
    # Calculate bounding box
    min_x <- min(coordinates_x)
    max_x <- max(coordinates_x)
    min_y <- min(coordinates_y)
    max_y <- max(coordinates_y)
    # Create bbox object
    result[["bbox"]] <- c(min_x, min_y, max_x, max_y)
    result
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
.stac_cdse_fix_items <- function(source, items, bands, collection, multicores) {
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
            .check_that(nrow(band_item) == 1,
                        msg = paste0("Invalid band: ", band))
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

# ---- open search utilities ----
#' @title Prepare Open Search feature as STAC Item (Compatible with `rstac`).
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function prepares an Open Search feature to act as an STAC Item,
#'  compatible with the `rstac` package. This is required to avoid changes in
#'  other parts of the `sits` validation structure and to use features
#'  from `rstac`.
#' @param features     List of features.
#' @param product_type Type of product associated with the features.
#' @return             List of features compatible with
#'                     `rstac` (`rstac::doc_item`).
.opensearch_as_stac_item <- function(features, product_type) {
    purrr::map(features, function(feature) {
        # Use `rstac` class, including `product type`
        class(feature) <- c("doc_item", "rstac_doc", "list", product_type)
        # Extract tile
        feature_tile <- .opensearch_cdse_extract_tile(feature)
        feature_tile <- unlist(feature_tile)
        # Save tile
        feature[["properties"]][["tile"]] <- feature_tile
        feature
    })
}

#' @title Prepare Open Search features as STAC Items (Compatible with `rstac`).
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function prepares an Open Search item to act as an STAC Item,
#'  compatible with the `rstac` package. This is required to avoid changes in
#'  other parts of the `sits` validation structure and to use features
#'  from `rstac`.
#' @param features     List of features.
#' @param product_type Type of product associated with the features.
#' @return             List of features compatible with
#'                     `rstac` (`rstac::doc_items`).
.opensearch_as_stac_items <- function(features) {
    # Create `rstac` compatible object
    items <- list()
    # Type is required to facilitate item visualization
    items[["type"]] <- "FeatureCollection"
    # Include features
    items[["features"]] <- features
    # Include `rstac` classes
    class(items) <- c("doc_items", "rstac_doc", "list")
    items
}

#' @title Query scenes available in the CDSE Open Search.
#' @keywords internal
#' @noRd
#'
#' @description
#'  This auxiliary function is used to get query the CDSE Open Search API. This
#'  is required as the current version of the CDSE STAC does
#'  not support fields / advanced search (e.g., search by product type, cloud
#'  coverage).
#' @param product_type Type of the CDSE Product (e.g., S2MSI2A)
#' @param ...          Additional query parameters.
#' @param source       Data source.
#' @param collection   Open Search collection endpoint.
#' @param start_date   Start date.
#' @param end_date     End date.
#' @param bbox         Bounding box of the area from data must be from
#' @param paginate     A Boolean flag that indicates whether pagination
#'                     should be used.
#' @param limit        Limit of content to be retrieved per page. Use `paginate`
#'                     to manage if multiple pages should be requested.
#' @return             List of features compatible with.
#'                     `rstac` (`rstac::doc_items`).
.opensearch_cdse_client <- function(product_type,
                                    source, collection,
                                    start_date, end_date,
                                    bbox,
                                    paginate = TRUE,
                                    limit = 1000, ...) {
    # CDSE Open Search configurations
    cdse_opensearch_base_url <- .conf(
        "sources",
        source,
        "url"
    )
    cdse_opensearch_max_items <- limit
    cdse_opensearch_endpoint <- "search.json"
    # Create the Open Search endpoint for the collection
    # Selected by user
    collection_url <- paste(
        cdse_opensearch_base_url,
        collection,
        cdse_opensearch_endpoint,
        sep = "/"
    )
    # Define features to save content from Open Search
    features_result <- c()
    # Define variables to support the pagination in the Open Search
    current_page <- 1
    is_to_fetch_more <- TRUE
    # Prepare bounding box in the format required by Open Search
    if (!is.null(bbox)) {
        bbox <- paste(bbox, collapse = ",")
    }
    # Prepare query object
    query <- list(
        startDate      = start_date,
        completionDate = end_date,
        maxRecords     = cdse_opensearch_max_items,
        page           = current_page,
        box            = bbox,
        productType    = product_type,
        ...
    )
    query <- purrr::discard(query, is.null)
    # Get items from Open Search (with pagination)
    while(is_to_fetch_more) {
        # Get raw content from Open Search API
        response <- httr::GET(url = collection_url, query = query)
        .check_int_parameter(httr::status_code(response),
                             min = 200,
                             max = 200,
                             msg = "Failed to fetch data")
        # Extract data from the response
        page_data <- httr::content(response, "parsed")
        # Extract features from response data
        features <- page_data[["features"]]
        features <- .opensearch_as_stac_item(features, product_type)
        # Save results
        features_result <- c(features_result, features)
        # Check if is required to fetch more
        links <- page_data[["properties"]][["links"]]
        is_to_fetch_more <- purrr::map_lgl(links, function(x) {
            x[["rel"]] == "next"
        })
        is_to_fetch_more <- paginate && any(is_to_fetch_more)
        # Prepare next page fetch
        current_page <- current_page + as.numeric(is_to_fetch_more)
        # Update query
        query[["page"]] <- current_page
    }
    .opensearch_as_stac_items(features_result)
}

#' @title Extract `tile` from Open Search Items.
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function prepares an Open Search item to act as an STAC Item,
#'  compatible with the `rstac` package. This is required to avoid changes in
#'  other parts of the `sits` validation structure and to use features
#'  from `rstac`.
#' @param items List of features compatible with `rstac` (`rstac::doc_items`).
#' @return      List of tiles.
.opensearch_cdse_extract_tile <- function(items) {
    UseMethod(".opensearch_cdse_extract_tile")
}

#' @keywords internal
#' @noRd
.opensearch_cdse_extract_tile.S2MSI2A <- function(items) {
    items_titles <- rstac::items_reap(items, field = c("properties", "title"))
    purrr::map(items_titles, function(item_title) {
        tile_name <- stringr::str_split(item_title, "_")[[1]][6]
        tile_name <- stringr::str_replace(tile_name, "T", "")
        tile_name
    })
}

#' @keywords internal
#' @noRd
.opensearch_cdse_extract_tile.RTC <- function(items) {
    "NoTilingSystem"
}

#' @title Search data using CDSE Open Search.
#' @keywords internal
#' @noRd
#'
#' @description
#'  This auxiliary function is used to query the CDSE Open Search API. This is
#'  a specialization of the `.opensearch_cdse_client` to handle the
#'  requirements of each CDSE data product.
#' @param product_type Type of the CDSE Product (e.g., S2MSI2A)
#' @param ...          Additional query parameters.
#' @param source       Data source.
#' @param collection   Open Search collection endpoint.
#' @param start_date   Start date.
#' @param end_date     End date.
#' @param bbox         Bounding box of the area from data must be from
#' @param paginate     A Boolean flag that indicates whether pagination
#'                     should be used.
#' @param limit        Limit of content to be retrieved per page. Use `paginate`
#'                     to manage if multiple pages should be requested.
#' @return             List of features compatible with
#'                     `rstac` (`rstac::doc_items`).
.opensearch_cdse_search <- function(product_type,
                                    source,
                                    collection,
                                    start_date,
                                    end_date,
                                    bbox,
                                    paginate = TRUE,
                                    limit = 1000, ...) {
    UseMethod(".opensearch_cdse_search")
}

#' @keywords internal
#' @noRd
.opensearch_cdse_search.S2MSI2A <- function(product_type, ...,
                                            source,
                                            collection,
                                            start_date,
                                            end_date,
                                            bbox,
                                            paginate = TRUE,
                                            limit = 1000) {
    .opensearch_cdse_client(
        product_type,
        source,
        collection,
        start_date,
        end_date,
        bbox,
        paginate,
        limit,
        status = "ONLINE"
    )
}

#' @keywords internal
#' @noRd
.opensearch_cdse_search.RTC <- function(product_type, ...,
                                        source,
                                        collection,
                                        start_date,
                                        end_date,
                                        bbox,
                                        paginate = TRUE,
                                        limit = 1000,
                                        orbit = "descending") {
    # Checks - Orbit
    orbits <- .conf("sources", source, "collections", collection, "orbits")
    .check_chr_within(orbit, orbits, msg = "Invalid `orbit` value")
    # Search!
    .opensearch_cdse_client(
        product_type,
        source,
        collection,
        start_date,
        end_date,
        bbox,
        paginate,
        limit,
        status = "ONLINE",
        orbitDirection = stringr::str_to_upper(orbit)
    )
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
            limit = 1
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
                                        orbit) {
    # set caller to show in errors
    .check_set_caller(".source_items_new_cdse_cube")
    # check parameters
    .check_int_parameter(multicores, min = 1, max = 2048)
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
    query_bbox <- .stac_cdse_intersects_as_bbox(stac_query)
    query_date <- .stac_cdse_datetime_as_dates(stac_query)
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
        orbit        = orbit
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
                                                 collection, multicores = 2L) {
    # CDSE does not provide files in the `assets` property. So, it is
    # required to fix this using content from CDSE S3 API.
    items <- .stac_cdse_fix_items(source, items, bands, collection, multicores)
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
    # pre-condition
    .check_num(nrow(file_info), min = 1, msg = "invalid 'file_info' value")

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
