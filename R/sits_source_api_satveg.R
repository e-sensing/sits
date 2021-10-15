#' @keywords internal
#' @export
.source_collection_access_test.satveg_cube <- function(source, ..., collection) {

    url <- .config_get(key = c("sources", source, "url_test"))

    access <- httr::GET(url)

    # did we get the data?
    if (httr::http_error(access))
        stop(paste(".source_collection_access_test.satveg_cube: service is unreachable"))

    return(invisible(NULL))
}

#' @keywords internal
#' @export
.source_cube.satveg_cube <- function(source, ..., collection) {

    # get the bands
    bands <- .source_bands(source = source, collection = collection)

    # get the bounding box of the cube
    bbox <- .satveg_get_bbox(source = source, collection = collection)

    # get the projection of the SATVEG data
    crs <- .satveg_get_crs(source = source, collection = collection)

    # create a tibble to store the metadata
    cube_satveg <- .cube_create(
        source = source,
        collection = collection,
        satellite = .source_collection_satellite(source, collection),
        sensor = .source_collection_sensor(source, collection),
        bands = bands,
        xmin = bbox[["xmin"]],
        xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]],
        ymax = bbox[["ymax"]],
        crs = crs
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
#'
#' @return functions that return values from config file
NULL


#' @rdname helper_satveg_function
.satveg_get_bbox <- function(source, collection) {

    # set caller to show in errors
    .check_set_caller(".satveg_get_bbox")

    # get the size of the cube
    bbox <- .config_get(key = c("sources", source, "collections",
                                collection, "bbox"))

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
    crs <- .config_get(key = c("sources", source, "collections",
                               collection, "crs"))

    # simplify
    crs <- unlist(crs)

    return(.sits_proj_format_crs(crs))
}
