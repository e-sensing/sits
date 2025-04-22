#' @title Select stac items by sits bands.
#'
#' @description Select bands in stac items by sits bands.
#' @keywords internal
#' @noRd
#' @param items        \code{STACItemcollection} object from rstac package.
#' @param bands_source Source bands (provider bands).
#' @param bands_sits   Sits bands.
#'
#' @return a \code{STACItemcollection} object with selected items by bands.
.stac_select_bands <- function(items, bands_source, bands_sits) {
    .check_set_caller(".stac_select_bands")
    # verify if the mapped band in on item assets
    .check_chr_within(
        x = bands_source,
        within = rstac::items_fields(items, "assets"),
        case_sensitive = FALSE
    )
    # covert bands to sits names
    bands_source <- toupper(bands_source)
    bands_converter <- toupper(bands_sits)
    names(bands_converter) <- bands_source

    # organize the items
    items[["features"]] <- purrr::map(items[["features"]], function(item) {
        names(item[["assets"]]) <- toupper(names(item[["assets"]]))
        item[["assets"]] <- item[["assets"]][bands_source]
        names(item[["assets"]]) <- unname(
            bands_converter[names(item[["assets"]])]
        )
        item
    })
    items
}

#' @title Datetime format
#' @name .stac_format_datetime
#' @keywords internal
#' @noRd
#' @param start_date Initial date for cube.
#' @param end_date   Final date for the cube.
#'
#' @return      a \code{character} formatted as parameter to STAC requisition.
.stac_format_datetime <- function(start_date, end_date) {
    datetime <- NULL
    # adding the dates according to RFC 3339
    if (.has(start_date) && .has(end_date)) {
        datetime <- paste(start_date, end_date, sep = "/")
    }
    datetime
}

#' @title Platform format
#' @name .stac_format_platform
#' @keywords internal
#' @noRd
#' @param source     A \code{character} value referring to a valid data source.
#' @param collection Image collection.
#' @param platform   Sensor platform
#'
#' @return      a \code{character} formatted as parameter to STAC requisition.
.stac_format_platform <- function(source, collection, platform) {
    .check_set_caller(".stac_format_platform")
    platforms <- .conf(
        "sources", source, "collections", collection, "platforms"
    )
    platform_source <- platforms[platform]
    .check_that(length(platform_source) == 1L)

    unlist(platform_source, use.names = FALSE)
}

#' @title Add href locator to gdal file
#' @name .stac_add_gdal_vsi
#' @keywords internal
#' @noRd
#' @description Currently, HTTP, S3 (AWS), and google storage (gs)
#'  links are supported.
#'
#' @param href        Link to the asset object.
#'
#' @return a \code{character} with the signed href.
.stac_add_gdal_fs <- function(href) {
    # reference for HHTP (generic)
    index <- grepl("^http|[s]://.*", href)
    if (any(index)) {
        href[index] <- file.path("/vsicurl", href[index])
    }
    # reference for AWS S3
    index <- grepl("^s3://.*", href)
    if (any(index)) {
        href[index] <- file.path(
            "/vsis3",
            gsub("^s3://(.*)$", "\\1", href[index])
        )
    }
    # reference for google cloud
    index <- grepl("^gs://.*", href)
    if (any(index)) {
        href[index] <- file.path(
            "/vsigs",
            gsub("^gs://(.*)$", "\\1", href[index])
        )
    }
    href
}

#' @title Creates a query to send to  the STAC API
#' @name .stac_items_query
#' @description Creates a query using rstac package to send to STAC API.
#' @keywords internal
#' @noRd
#' @param source     Name of the STAC provider.
#' @param collection Collection to be searched in the data source.
#' @param ...        Other parameters to be passed for specific types.
#' @param roi        Region of interest as sf object.
#' @param start_date Initial date for the cube (optional).
#' @param end_date   Final date for the cube  (optional).
#' @param limit      Limit items to be returned in requisition.
#'
#' @return an \code{RSTACQuery} object.
.stac_create_items_query <- function(source,
                                     collection, ...,
                                     roi = NULL,
                                     start_date = NULL,
                                     end_date = NULL,
                                     limit = NULL) {
    # get collection original name
    collection <- .source_collection_name(
        source = source,
        collection = collection
    )
    # get the URL
    url <- .source_url(source = source)
    # obtain the datetime parameter for STAC like parameter
    datetime <- .stac_format_datetime(start_date, end_date)
    # by default, roi is NULL
    roi_geojson <- NULL
    # obtain the bounding box and intersects parameters
    if (.has(roi)) {
        # convert to geojson
        roi_geojson <- .roi_sf_to_geojson(roi)
    }
    # get the limit items to be returned in each page
    if (is.null(limit)) {
        limit <- .conf("rstac_pagination_limit")
    }
    # create a query object to be searched by STAC
    rstac_query <- rstac::stac_search(
        q = rstac::stac(url),
        collections = collection,
        bbox = NULL,
        intersects = roi_geojson,
        datetime = datetime,
        limit = limit
    )
    # adjust limit datatype
    rstac_query[["params"]][["limit"]] <- as.numeric(limit)
    # return!
    rstac_query
}
#' @title Extract bounding box from a STAC Query.
#' @keywords internal
#' @noRd
#'
#' @param stac_query Query that follows the STAC protocol.
#' @return           List with `bbox` property.
.stac_intersects_as_bbox <- function(stac_query) {
    result <- list(bbox = NULL)
    # Extract spatial reference from STAC object
    intersects <- stac_query[["params"]][["intersects"]]
    coordinates <- intersects[["coordinates"]]
    # Check if query is valid
    if (is.null(coordinates)) {
        return(result)
    }
    # Extract x-coordinates and y-coordinates
    coordinates_x <- coordinates[, , 1L]
    coordinates_y <- coordinates[, , 2L]
    # Calculate bounding box
    min_x <- min(coordinates_x)
    max_x <- max(coordinates_x)
    min_y <- min(coordinates_y)
    max_y <- max(coordinates_y)
    # Create bbox object
    result[["bbox"]] <- c(min_x, min_y, max_x, max_y)
    result
}
#' @title Extract datetime from a STAC Query.
#' @keywords internal
#' @noRd
#'
#' @param stac_query Query that follows the STAC protocol.
#' @return           List with `start_date` and `end_date` properties.
.stac_datetime_as_dates <- function(stac_query) {
    query_datetime <- stringr::str_split(
        stac_query[["params"]][["datetime"]], "/"
    )
    list(
        start_date = query_datetime[[1L]][1L],
        end_date = query_datetime[[1L]][2L]
    )
}
#' @title Extract dates as datetime from a STAC Query.
#' @keywords internal
#' @noRd
#'
#' @param stac_query Query that follows the STAC protocol.
#' @return           List with `start_date` and `end_date` properties.
.stac_dates_as_datetimes <- function(stac_query) {
    # get start and end date
    date_time <- strsplit(
        stac_query[["params"]][["datetime"]],
        split = "/"
    )
    dates_chr <- date_time[[1L]]
    # format as datetime (RFC 3339)
    paste(
        format(as.Date(dates_chr), "%Y-%m-%dT%H:%M:%SZ"),
        collapse = "/"
    )
}
