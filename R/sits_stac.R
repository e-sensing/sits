
#' TODO: document
.stac_bands_select <- function(items, bands_source, bands_sits) {

    assertthat::assert_that(
        all(bands_source %in% rstac::items_fields(items, "assets")),
        msg = paste("The bands contained in this product",
                    "are not mapped in the SITS package,",
                    "if you want to include them please",
                    "provide it in configuration file."))

    bands_converter <- bands_sits
    names(bands_converter) <- bands_source

    items$features <- purrr::map(items$features, function(item){

        item$assets <- item$assets[bands_source]
        names(item$assets) <- unname(bands_converter[names(item$assets)])

        item
    })

    items
}

#' @title Checks if the crs provided is valid
#' @name .stac_format_crs
#' @keywords internal
#'
#' @param stac_crs a \code{numeric} or \code{character} with CRS provided by
#'  STAC.
#'
#' @return  a \code{character} with the formatted CRS.
.stac_format_crs <- function(stac_crs) {

    if (is.null(stac_crs))
        stop(paste("sits_cube: The CRS in this catalog is null, please",
                   "enter a valid CRS."))
    return(sf::st_crs(stac_crs)[["input"]])
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

    # list to store parameters values
    roi_list <- list()

    # verify the provided parameters
    if (!inherits(roi, "sf")) {
        if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(roi)))
            roi_list[c("bbox", "intersects")] <-
                list(roi[c("xmin", "ymin", "xmax", "ymax")], NULL)

        else if (typeof(roi) == "character")
            roi_list[c("bbox", "intersects")] <- list(NULL, roi)
    } else {
        roi_list[c("bbox", "intersects")] <- list(as.vector(sf::st_bbox(roi)),
                                                  NULL)
    }

    # checks if the specified parameters names is contained in the list
    assertthat::assert_that(
        !purrr::is_null(names(roi_list)),
        msg = ".sits_stac_roi: invalid definition of ROI"
    )

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

#' @title  ...
#' @name .stac_items_query
#' @description  ...
#' @param source     Name of the STAC provider
#' @param collection ...
#' @param name ...
#' @param bands ...
#' @param tiles ...
#' @param bbox ...
#' @param start_date ...
#' @param end_date ...
#' @param ... ...
#'
#' @return ...
.stac_items_query <- function(source,
                              collection,
                              name,
                              bands,
                              tiles,
                              bbox = NULL,
                              start_date = NULL,
                              end_date = NULL,
                              limit = NULL, ...) {

    url <- .config_source_url(source = source)
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
