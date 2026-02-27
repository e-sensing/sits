#' @title Prepare OData product as STAC Item (Compatible with `rstac`).
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function prepares a CDSE OData product to act as a STAC Item,
#'  compatible with the `rstac` package. This avoids changes in other parts of
#'  the `sits` validation structure and enables use of `rstac` features.
#'
#' @param product      OData product entry (single element from `value` array).
#' @param product_type Type of product associated with the features.
#'
#' @return             Feature compatible with `rstac` (`rstac::doc_item`).
.odata_as_stac_item <- function(product, product_type) {
    # Define feature
    feature <- list()
    # Add `type`
    feature[["type"]] <- "Feature"
    # Add `id`
    feature[["id"]] <- product[["Id"]]
    # Add `geometry`
    feature[["geometry"]] <- product[["GeoFootprint"]]
    # Add `properties`
    feature[["properties"]] <- list(
        startDate         = product[["ContentDate"]][["Start"]],
        completionDate    = product[["ContentDate"]][["End"]],
        productIdentifier = product[["S3Path"]]
    )
    # Use `rstac` class, including `product_type`
    class(feature) <- c("doc_item", "rstac_doc", "list", product_type)
    # Extract tile and attach to properties
    feature_tile <- .odata_cdse_extract_tile(feature)
    # Set `tile`
    feature[["properties"]][["tile"]] <- unlist(feature_tile)
    # Return!
    feature
}

#' @title Prepare OData products as STAC Items (Compatible with `rstac`).
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @description
#'  This function wraps a list of OData-derived features into a STAC
#'  FeatureCollection, compatible with `rstac` (`rstac::doc_items`).
#'
#' @param features List of features (from `.odata_as_stac_item`).
#' @return         List compatible with `rstac` (`rstac::doc_items`).
.odata_as_stac_items <- function(features) {
    # Define stac item
    items <- list()
    # Define `type`
    items[["type"]] <- "FeatureCollection"
    # Add `features`
    items[["features"]] <- features
    # Define classes
    class(items) <- c("doc_items", "rstac_doc", "list")
    # Return!
    items
}

#' @title Query scenes available in the CDSE OData API.
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @description
#'  Low-level HTTP client that queries the CDSE OData Products endpoint.
#'
#' @param product_type Type of the CDSE product (e.g., `"RTC"`).
#' @param filter       OData `$filter` string.
#' @param paginate     Logical; whether to follow pagination.
#' @param limit        Number of records per request (max 1000).
#' @return             List of features compatible with `rstac`
#'                     (`rstac::doc_items`).
.odata_cdse_client <- function(source,
                               collection,
                               product_type,
                               filter,
                               paginate = TRUE,
                               limit = 1000L) {
    # Define caller
    .check_set_caller(".odata_cdse_client")
    # Get OData URL
    odata_url <- .conf(
        "sources",
        source,
        "url"
    )
    # Define accumulators
    # Feature results
    features_result <- NULL
    # Current skip
    current_skip <- 0L
    # Flag indicating if it is required to
    # fetch more data
    is_to_fetch_more <- TRUE
    # While is required, fetch items
    while (is_to_fetch_more) {
        # Define query object
        query <- list(
            `$filter` = filter,
            `$top`    = limit,
            `$skip`   = current_skip
        )
        # Get raw content from OData API
        response <- .get_request(url = odata_url, query = query)
        .check_int_parameter(
            .response_status(response),
            min = 200L,
            max = 200L
        )
        # Extract data from the response
        page_data <- .response_content(response)
        # Extract products
        products <- page_data[["value"]]
        # Prepare result as STAC Item
        items <- purrr::map(products, function(p) {
            .odata_as_stac_item(p, product_type)
        })
        # Save results
        features_result <- c(features_result, items)
        # Get number of returned products
        n_returned <- length(products)
        # Get next link
        next_link <- page_data[["@OData.nextLink"]]
        # Check if is required to fetch more
        is_to_fetch_more <- paginate && !is.null(next_link) && nzchar(next_link)
        # Update skip
        current_skip <- current_skip + n_returned
    }
    # Create STAC Item
    .odata_as_stac_items(features_result)
}

#' @title Extract `tile` from OData Items.
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @description
#'  Dispatch function for extracting tile information from OData-derived
#'  STAC items. Specialisations are registered per `product_type`.
#'
#' @param items OData-derived feature.
#' @return      Tile identifier.
.odata_cdse_extract_tile <- function(items) {
    UseMethod(".odata_cdse_extract_tile")
}

#' @title Extract `tile` from OData Items for Sentinel-1 RTC.
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @export
.odata_cdse_extract_tile.RTC <- function(items) {
    "NoTilingSystem"
}

#' @title Search data using CDSE OData.
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @description
#'  Dispatch function for querying the CDSE OData API. Each `product_type`
#'  registers its own method that builds the appropriate OData filter.
#'
#' @param product_type Type of the CDSE product (e.g., `"RTC"`).
#' @param ...          Additional parameters forwarded to the method.
#' @return             List of features compatible with `rstac`
#'                     (`rstac::doc_items`).
.odata_cdse_search <- function(product_type, ...) {
    UseMethod(".odata_cdse_search")
}

#' @title Search data using CDSE OData for Sentinel-1 RTC.
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @export
.odata_cdse_search.RTC <- function(product_type,
                                    source,
                                    collection,
                                    start_date,
                                    end_date,
                                    bbox,
                                    platform = NULL,
                                    orbit = NULL,
                                    paginate = TRUE,
                                    limit = 1000L, ...) {
    # Define caller
    .check_set_caller(".odata_cdse_search_rtc")
    # check orbit
    if (!is.null(orbit)) {
        orbits <- .conf("sources", source, "collections", collection, "orbits")
        .check_chr_within(x = orbit, within = orbits)
    }
    # Build OData `$filter`
    # Collection
    filter <- paste0("Collection/Name eq '", collection, "'")
    # Date range
    if (!is.null(start_date)) {
        filter <- paste0(
            filter,
            " and ContentDate/Start ge ", start_date, "T00:00:00.000Z"
        )
    }
    if (!is.null(end_date)) {
        filter <- paste0(
            filter,
            " and ContentDate/Start lt ", end_date, "T00:00:00.000Z"
        )
    }
    # Geographic intersection
    if (!is.null(bbox)) {
        # Prepare polygon in the OData format
        polygon <- sprintf(
            "POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))",
            bbox[[1L]], bbox[[2L]],
            bbox[[3L]], bbox[[2L]],
            bbox[[3L]], bbox[[4L]],
            bbox[[1L]], bbox[[4L]],
            bbox[[1L]], bbox[[2L]]
        )
        # Add polygon as CSC Intersects
        filter <- paste0(
            filter,
            " and OData.CSC.Intersects(area=geography'SRID=4326;",
            polygon, "')"
        )
    }
    # Platform attribute filter
    if (!is.null(platform)) {
        filter <- paste0(
            filter,
            " and Attributes/OData.CSC.StringAttribute/any(",
            "att:att/Name eq 'platformSerialIdentifier'",
            " and att/OData.CSC.StringAttribute/Value eq '",
            platform, "')"
        )
    }
    # Orbit attribute filter
    if (!is.null(orbit)) {
        filter <- paste0(
            filter,
            " and Attributes/OData.CSC.StringAttribute/any(",
            "att:att/Name eq 'orbitDirection'",
            " and att/OData.CSC.StringAttribute/Value eq '",
            stringr::str_to_upper(orbit), "')"
        )
    }
    # Query OData endpoint
    .odata_cdse_client(
        source       = source,
        collection   = collection,
        product_type = product_type,
        filter       = filter,
        paginate     = paginate,
        limit        = limit
    )
}
