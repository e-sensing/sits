
#' @title Select stac items by sits bands.
#'
#' @description Select bands in stac items by sits bands.
#'
#' @param items        a \code{STACItemcollection} object from rstac package.
#' @param bands_source a \code{character} with source bands (provider bands).
#' @param bands_sits   a \code{character} with sits bands.
#'
#' @return a \code{STACItemcollection} object with selected items by bands.
.stac_bands_select <- function(items, bands_source, bands_sits) {

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
#' @name .stac_roi
#' @keywords internal
#'
#' @param roi  the "roi" parameter defines a region of interest. It can be
#'  an \code{sfc} or \code{sf} object from sf package, a \code{character} with
#'  GeoJSON following the rules from RFC 7946, or a \code{vector}
#'  bounding box \code{vector} with named XY values
#'  ("xmin", "xmax", "ymin", "ymax").
#'
#' @return     A named \code{list} with the values of the intersection and bbox
#'             parameters. If bbox is supplied, the intersection parameter gets
#'             NULL, otherwise bbox gets NULL if intersects is specified.
.stac_roi <- function(roi) {

    # set caller to show in errors
    .check_set_caller(".stac_roi")

    # list to store parameters values
    roi_list <- list()

    # verify the provided parameters
    if (!inherits(roi, "sf")) {
        if (all(c("lon_min", "lat_min", "lon_max", "lat_max") %in% names(roi)))
            roi_list[c("bbox", "intersects")] <-
                list(roi[c("lon_min", "lat_min", "lon_max", "lat_max")], NULL)

        else if (typeof(roi) == "character")
            roi_list[c("bbox", "intersects")] <- list(NULL, roi)
    } else {
        roi_list[c("bbox", "intersects")] <- list(as.vector(sf::st_bbox(roi)),
                                                  NULL)
    }

    # checks if the specified parameters names is contained in the list
    .check_chr(x = names(roi_list),
               allow_empty = FALSE,
               msg = "invalid definition of ROI")

    return(roi_list)
}
#' @title Datetime format
#' @name .stac_datetime
#' @keywords internal
#'
#' @param start_date a \code{character} corresponds to the initial date when the
#'  cube will be created.
#' @param end_date   a \code{character} corresponds to the final date when the
#'  cube will be created.
#'
#' @return      a \code{character} formatted as parameter to STAC requisition.
.stac_datetime <- function(start_date, end_date) {

    datetime <- NULL
    # adding the dates according to RFC 3339
    if (!purrr::is_null(start_date) && !purrr::is_null(end_date))
        datetime <- paste(start_date, end_date, sep = "/")

    return(datetime)
}

#' @title ...
#' @name .stac_add_gdal_vsi
#' @keywords internal
#'
#' @param href a \code{character} ...
#'
#' @return ...
.stac_add_gdal_vsi <- function(href) {

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

#' @title Get the STAC information corresponding to a bbox extent
#' @name .stac_get_bbox
#' @keywords internal
#'
#' @param items a \code{STACItemCollection} object returned by rstac.
#' @param crs   a \code{character} with proj code.
#'
#' @return  a \code{bbox} object from the sf package representing the tile bbox.
.stac_get_bbox <- function(items, crs) {

    # get the extent points
    extent_points <- items$features[[1]]$geometry$coordinates[[1]]

    # create a polygon and transform the proj
    polygon_ext <- sf::st_polygon(list(do.call(rbind, extent_points)))
    polygon_ext <- sf::st_transform(sf::st_sfc(polygon_ext, crs = 4326), crs)

    bbox_ext <- sf::st_bbox(polygon_ext)

    return(bbox_ext)
}

#' @title  Creates a query to send to STAC api
#' @name .stac_items_query
#' @description  Creates a query using rstac package to send to STAC API.
#'
#' @param source     Name of the STAC provider
#' @param collection Collection to be searched in the data source
#' @param bbox       Area of interest.
#' @param start_date Initial date for the cube (optional).
#' @param end_date   Final date for the cube  (optional).
#' @param limit      limit items to be returned in requisition.
#'
#' @return an \code{RSTACQuery} object.
.stac_items_query <- function(source,
                              collection,
                              bbox = NULL,
                              start_date = NULL,
                              end_date = NULL,
                              limit = NULL) {

    # get collection original name
    collection <- .source_collection_name(source = source,
                                          collection = collection)

    url <- .source_url(source = source)
    roi <- list(bbox = NULL, intersects = NULL)

    # obtain the datetime parameter for STAC like parameter
    datetime <- .stac_datetime(start_date, end_date)

    # obtain the bounding box and intersects parameters
    if (!is.null(bbox))
        roi <- .stac_roi(bbox)

    # get the limit items to be returned in each page
    if (is.null(limit))
        limit <- .config_rstac_limit()

    # creating an query object to be search
    rstac_query <-  rstac::stac_search(q = rstac::stac(url),
                                       collections = collection,
                                       bbox        = roi$bbox,
                                       intersects  = roi$intersects,
                                       datetime    = datetime,
                                       limit       = limit)

    return(rstac_query)
}
