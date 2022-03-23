#' @keywords internal
#' @export
.source_collection_access_test.satveg_cube <- function(source,
                                                       collection, ...) {
    url <- .config_get(key = c("sources", source, "url_test"))
    access <- httr::GET(url)
    # did we get the data?
    if (httr::http_error(access)) {
        stop(paste(".source_collection_access_test.satveg_cube: service is unreachable"))
    }
    return(invisible(NULL))
}

#' @keywords internal
#' @export
.source_cube.satveg_cube <- function(source, collection, ...) {

    # get the bands
    bands <- .source_bands(source = source, collection = collection)
    # get the bounding box of the cube
    bbox <- .satveg_get_bbox(source = source, collection = collection)
    # get the projection of the SATVEG data
    crs <- .satveg_get_crs(source = source, collection = collection)
    # get the start date of SATVEG date
    date <- .satveg_get_date(source = source)
    # get the URL of SATVEG cube
    url <- .satveg_get_url(source = source)
    # set the file information
    file_info <- purrr::map_dfr(bands, function(band) {
        t <- tibble::tibble(
            band = band,
            res = .satveg_get_resolution(
                source = source,
                collection = collection,
                band = band
            ),
            date = date,
            path = url
        )
        return(t)
    })
    # create a tibble to store the metadata
    cube_satveg <- .cube_create(
        source = source,
        collection = collection,
        satellite = .source_collection_satellite(source, collection),
        sensor = .source_collection_sensor(source, collection),
        xmin = bbox[["xmin"]],
        xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]],
        ymax = bbox[["ymax"]],
        crs = crs,
        file_info = file_info
    )
    class(cube_satveg) <- .cube_s3class(cube_satveg)
    return(cube_satveg)
}

#' @title Helper satveg functions
#' @description ...
#' @name helper_satveg_function
#' @keywords internal
#'
#' @param source ...
#' @param collection ...
#' @param band ...
#'
#' @return functions that return values from config file
NULL


#' @rdname helper_satveg_function
.satveg_get_bbox <- function(source, collection) {

    # set caller to show in errors
    .check_set_caller(".satveg_get_bbox")
    # get the size of the cube
    bbox <- .config_get(key = c(
        "sources", source, "collections",
        collection, "bbox"
    ))
    # simplify
    bbox <- unlist(bbox)
    # post-condition
    .check_chr_within(
        x = names(bbox),
        within = c("xmin", "ymin", "xmax", "ymax"),
        msg = "invalid bbox."
    )
    return(bbox)
}

#' @rdname helper_satveg_function
.satveg_get_crs <- function(source, collection) {

    # get cube crs
    crs <- .config_get(key = c(
        "sources", source, "collections",
        collection, "crs"
    ))
    # simplify
    crs <- unlist(crs)
    return(.sits_proj_format_crs(crs))
}

#' @rdname helper_satveg_function
.satveg_get_resolution <- function(source, collection, band) {

    # get cube crs
    resolution <- .config_get(key = c(
        "sources", source, "collections",
        collection, "bands", band, "resolution"
    ))
    # simplify
    resolution <- unlist(resolution)
    return(resolution)
}

#' @rdname helper_satveg_function
.satveg_get_url <- function(source) {

    # get cube crs
    url <- .config_get(key = c("sources", source, "url"))
    return(url)
}

#' @rdname helper_satveg_function
.satveg_get_date <- function(source) {

    # get cube crs
    date <- .config_get(key = c("sources", source, "start_date"))
    # simplify
    date <- as.Date(unlist(date))
    return(date)
}
