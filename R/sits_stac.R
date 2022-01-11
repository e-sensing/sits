#' @title Select stac items by sits bands.
#'
#' @description Select bands in stac items by sits bands.
#' @keywords internal
#'
#' @param items        a \code{STACItemcollection} object from rstac package.
#' @param bands_source a \code{character} with source bands (provider bands).
#' @param bands_sits   a \code{character} with sits bands.
#'
#' @return a \code{STACItemcollection} object with selected items by bands.
.stac_select_bands <- function(items, bands_source, bands_sits) {

    # verify if the mapped band in on item assets
    .check_chr_within(x = bands_source,
                      within = rstac::items_fields(items, "assets"),
                      case_sensitive = FALSE,
                      msg = paste("The bands contained in this product",
                                  "are not mapped in the SITS package,",
                                  "if you want to include them please",
                                  "provide it in configuration file."))
    bands_source <- toupper(bands_source)
    bands_converter <- toupper(bands_sits)
    names(bands_converter) <- bands_source


    items$features <- purrr::map(items$features, function(item){

        names(item$assets) <- toupper(names(item$assets))
        item$assets <- item$assets[bands_source]
        names(item$assets) <- unname(bands_converter[names(item$assets)])

        item
    })

    items
}

#' @title Datetime format
#' @name .stac_format_datetime
#' @keywords internal
#'
#' @param start_date a \code{character} corresponds to the initial date when the
#'  cube will be created.
#' @param end_date   a \code{character} corresponds to the final date when the
#'  cube will be created.
#'
#' @return      a \code{character} formatted as parameter to STAC requisition.
.stac_format_datetime <- function(start_date, end_date) {

    datetime <- NULL
    # adding the dates according to RFC 3339
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date))
        datetime <- paste(start_date, end_date, sep = "/")

    return(datetime)
}

#' @title Add gdal file system in a href
#' @name .stac_add_gdal_vsi
#' @keywords internal
#'
#' @description Currently, HTTP, S3 (AWS), and google storage (gs)
#'  links are supported.
#'
#' @param href a \code{character} with link to the asset object.
#'
#' @return a \code{character} with the signed href.
.stac_add_gdal_fs <- function(href) {

    index <- grepl("^http|[s]://.*", href)
    if (any(index))
        href[index] <- paste("/vsicurl", href[index], sep = "/")

    index <- grepl("^s3://.*", href)
    if (any(index))
        href[index] <- paste("/vsis3",
                             gsub("^s3://(.*)$", "\\1", href[index]),
                             sep = "/")

    index <- grepl("^gs://.*", href)
    if (any(index))
        href[index] <- paste("/vsigs",
                             gsub("^gs://(.*)$", "\\1", href[index]),
                             sep = "/")

    return(href)
}

#' @title Creates a query to send to STAC api
#' @name .stac_items_query
#' @description Creates a query using rstac package to send to STAC API.
#' @keywords internal
#'
#' @param source     Name of the STAC provider
#' @param collection Collection to be searched in the data source
#' @param ...        Other parameters to be passed for specific types.
#' @param roi_sf     Region of interest as sf object.
#' @param start_date Initial date for the cube (optional).
#' @param end_date   Final date for the cube  (optional).
#' @param limit      limit items to be returned in requisition.
#'
#' @return an \code{RSTACQuery} object.
.stac_create_items_query <- function(source,
                                     collection, ...,
                                     roi_sf = NULL,
                                     start_date = NULL,
                                     end_date = NULL,
                                     limit = NULL) {

    # get collection original name
    collection <- .source_collection_name(source = source,
                                          collection = collection)

    url <- .source_url(source = source)


    # obtain the datetime parameter for STAC like parameter
    datetime <- .stac_format_datetime(start_date, end_date)

    # by default, roi is NULL
    roi_geojson <- NULL
    # bbox <- NULL

    # obtain the bounding box and intersects parameters
    if (!purrr::is_null(roi_sf)) {
        # convert to geojson
        roi_geojson <- .sits_roi_sf_to_geojson(roi_sf)

        # # get bbox from roi_sf
        # bbox <- sf::st_bbox(roi_sf)
        # names(bbox) <- c("lon_min", "lat_min", "lon_max", "lat_max")
        # class(bbox) <- c("numeric", class(bbox))
    }

    # get the limit items to be returned in each page
    if (is.null(limit))
        limit <- .config_rstac_limit()

    # creating an query object to be search
    rstac_query <-  rstac::stac_search(q = rstac::stac(url),
                                       collections = collection,
                                       bbox        = NULL,
                                       intersects  = roi_geojson,
                                       datetime    = datetime,
                                       limit       = limit)

    return(rstac_query)
}
