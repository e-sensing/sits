#' @title Prepare Open Search feature as STAC Item (Compatible with `rstac`).
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
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
                                    limit = 1000L, ...) {
    .check_set_caller(".opensearch_cdse_client")
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
    collection_url <- file.path(
        cdse_opensearch_base_url,
        collection,
        cdse_opensearch_endpoint
    )
    # Define features to save content from Open Search
    features_result <- NULL
    # Define variables to support the pagination in the Open Search
    current_page <- 1L
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
    while (is_to_fetch_more) {
        # Get raw content from Open Search API
        response <- .get_request(url = collection_url, query = query)
        .check_int_parameter(.response_status(response),
            min = 200L,
            max = 200L
        )
        # Extract data from the response
        page_data <- .response_content(response)
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
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
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

#' @title Extract `tile` from Open Search Items for Sentinel-2
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @export
.opensearch_cdse_extract_tile.S2MSI2A <- function(items) {
    items_titles <- rstac::items_reap(items, field = c("properties", "title"))
    purrr::map(items_titles, function(item_title) {
        tile_name <- stringr::str_split(item_title, "_")[[1L]][6L]
        tile_name <- stringr::str_replace(tile_name, "T", "")
        tile_name
    })
}
#' @title Extract `tile` from Open Search Items for Sentinel-1 RTC
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @export
.opensearch_cdse_extract_tile.RTC <- function(items) {
    "NoTilingSystem"
}

#' @title Search data using CDSE Open Search
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
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
#' @param bbox         Bounding box of the area from data must be from.
#' @param platform     Optional parameter specifying the platform in case of
#'                     collections that include more than one satellite
#' @param orbit        Orbit name ("ascending", "descending") for SAR cubes.
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
                                    platform,
                                    orbit = NULL,
                                    paginate = TRUE,
                                    limit = 1000L, ...) {
    UseMethod(".opensearch_cdse_search")
}

#' @title Search data using CDSE Open Search for Sentinel-2
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @export
.opensearch_cdse_search.S2MSI2A <- function(product_type,
                                            source, collection,
                                            start_date, end_date,
                                            bbox,
                                            platform = NULL,
                                            orbit = NULL,
                                            paginate = TRUE,
                                            limit = 1000L, ...) {
    .check_set_caller(".opensearch_cdse_search_s2msi2a")
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
        instrument = "MSI",
        platform = platform,
        processingLevel = "S2MSI2A"
    )
}
#' @title Search data using CDSE Open Search for Sentinel-1 RTC
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @keywords internal
#' @noRd
#' @export
.opensearch_cdse_search.RTC <- function(product_type,
                                        source, collection,
                                        start_date, end_date,
                                        bbox,
                                        platform = NULL,
                                        orbit = NULL,
                                        paginate = TRUE, limit = 1000L, ...) {
    .check_set_caller(".opensearch_cdse_search_rtc")
    # check orbit
    if (!is.null(orbit)) {
        orbits <- .conf("sources", source, "collections", collection, "orbits")
        .check_chr_within(x = orbit, within = orbits)
    }
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
        sensorMode = "IW",
        instrument = "C-SAR",
        platform = platform,
        orbitDirection = stringr::str_to_upper(orbit)
    )
}
