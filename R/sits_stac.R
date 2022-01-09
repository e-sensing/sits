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

#' @title Get bbox and intersects parameters
#' @name .stac_get_roi
#' @keywords internal
#'
#' @param roi  the "roi" parameter defines a region of interest. It can be
#'  an \code{sfc} or \code{sf} object from sf package, a \code{character} with
#'  GeoJSON following the rules from RFC 7946, or a \code{vector}
#'  bounding box \code{vector} with named XY values
#'  ("xmin", "xmax", "ymin", "ymax").
#'
#' @return A named \code{list} with the values of the intersection and bbox
#'         parameters. If bbox is supplied, the intersection parameter gets
#'         NULL, otherwise bbox gets NULL if intersects is specified.
.stac_get_roi <- function(roi) {

    # set caller to show in errors
    .check_set_caller(".stac_get_roi")

    UseMethod(".stac_get_roi", roi)
}

#' @keywords internal
#' @export
.stac_get_roi.default <- function(roi) {

    stop("Invalid roi parameter. Please see the documentation on ?sits_cube")
}

#' @keywords internal
#' @export
.stac_get_roi.NULL <- function(roi) {

    return(list(bbox = NULL, intersects = NULL))
}

#' @keywords internal
#' @export
.stac_get_roi.sfc <- function(roi) {

    return(.stac_get_roi.sf(roi))
}

#' @keywords internal
#' @export
.stac_get_roi.sf <- function(roi) {

    roi_crs <- sf::st_crs(roi, parameters = TRUE)
    .check_lst(roi_crs, min_len = 1, msg = "invalid crs in provided roi.")

    if (roi_crs[["epsg"]] != 4326) {
        message("The supplied roi will be transformed to the WGS 84.")
        roi <- sf::st_transform(roi, crs = 4326)
    }

    # it does not remove the names from vector
    sf_bbox <- c(sf::st_bbox(roi))

    sits_bbox <- sf_bbox[c("xmin", "ymin", "xmax", "ymax")]
    names(sits_bbox) <- c("lon_min", "lat_min", "lon_max", "lat_max")

    return(list(bbox = sits_bbox, intersects = NULL))
}

#' @keywords internal
#' @export
.stac_get_roi.numeric <- function(roi) {

    .check_chr_within(x = names(roi),
                      within = c("lon_min", "lat_min", "lon_max", "lat_max"),
                      msg = "invalid roi parameter")

    return(
        list(bbox = roi[c("lon_min", "lat_min", "lon_max", "lat_max")],
         intersects = NULL)
    )
}

#' @keywords internal
#' @export
.stac_get_roi.character <- function(roi) {

    return(list(bbox = NULL, intersects = roi))
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
#' @param roi        Region of interest.
#' @param start_date Initial date for the cube (optional).
#' @param end_date   Final date for the cube  (optional).
#' @param limit      limit items to be returned in requisition.
#'
#' @return an \code{RSTACQuery} object.
.stac_create_items_query <- function(source,
                                     collection, ...,
                                     roi = NULL,
                                     start_date = NULL,
                                     end_date = NULL,
                                     limit = NULL) {

    # get collection original name
    collection <- .source_collection_name(source = source,
                                          collection = collection)

    url <- .source_url(source = source)


    # obtain the datetime parameter for STAC like parameter
    datetime <- .stac_format_datetime(start_date, end_date)

    # obtain the bounding box and intersects parameters
    roi_stac <- .stac_get_roi(roi)

    # get the limit items to be returned in each page
    if (is.null(limit))
        limit <- .config_rstac_limit()

    # creating an query object to be search
    rstac_query <-  rstac::stac_search(q = rstac::stac(url),
                                       collections = collection,
                                       bbox        = roi_stac$bbox,
                                       intersects  = roi_stac$intersects,
                                       datetime    = datetime,
                                       limit       = limit)

    return(rstac_query)
}
