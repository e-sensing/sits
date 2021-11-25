#' @keywords internal
#' @export
.source_collection_access_test.wtss_cube <- function(source, collection, ...) {

    # require package
    if (!requireNamespace("Rwtss", quietly = TRUE)) {
        stop("Please install package Rwtss", call. = FALSE)
    }

    url <- .source_url(source = source)
    coverages <- Rwtss::list_coverages(url)

    # is the WTSS service working?
    .check_null(x = coverages,
                msg = "WTSS is unreachable"
    )

    # is the cube in the list of cubes?
    .check_chr_within(
        x = collection,
        within = coverages,
        discriminator = "any_of",
        msg = paste(collection, "not available in the WTSS server")
    )

    return(invisible(NULL))
}
#' @keywords internal
#' @export
.source_cube.wtss_cube <- function(source, collection, name, ...) {

    cov <- .source_items_new(source = source,
                             collection = collection, ...)

    file_info <- .source_items_fileinfo(source = source,
                                        collection = collection,
                                        wtss_cov = cov, ...)

    cube <- .source_items_cube(source = source,
                               collection = collection,
                               items = cov,
                               file_info = file_info, ...)

    return(cube)
}

#' @keywords internal
#' @export
.source_items_new.wtss_cube <- function(source = source, ...,
                                        collection = NULL) {

    # set caller to show in errors
    .check_set_caller(".source_items_new.wtss_cube")

    url <- .source_url(source = source)

    # describe the cube based on the WTSS API
    cov <- Rwtss::describe_coverage(url, collection, .print = FALSE)
    .check_null(x = cov,
                msg = paste("failed to get cube description in WTSS.")
    )

    return(cov)
}

#' @keywords internal
#' @export
.source_items_fileinfo.wtss_cube <- function(source,
                                             collection,
                                             wtss_cov, ...) {

    bands <- .source_bands(source = source, collection = collection)

    url <- .source_url(source = source)
    file_info <- tibble::tibble(
        date = wtss_cov$timeline, path = url, band = bands, res = wtss_cov$xres)

    return(file_info)
}

#' @keywords internal
#' @export
.source_items_cube.wtss_cube <- function(source,
                                         collection,
                                         items,
                                         file_info, ...) {


    # create a tibble to store the metadata
    cube_wtss <- .cube_create(
        source = source,
        collection = collection,
        satellite = .source_collection_satellite(source, collection),
        sensor = .source_collection_sensor(source, collection),
        xmin = items$xmin,
        xmax = items$xmax,
        ymin = items$ymin,
        ymax = items$ymax,
        crs  = items$crs,
        file_info = file_info
    )

    class(cube_wtss) <- .cube_s3class(cube_wtss)

    return(cube_wtss)
}
