#' @keywords internal
#' @export
.source_access_test.wtss_cube <- function(source, collection, ...) {

    # require package
    if (!requireNamespace("Rwtss", quietly = TRUE)) {
        stop(paste("Please install package Rwtss from CRAN:",
                   "install.packages('Rwtss')"), call. = FALSE
        )
    }

    url <- .config_source_url(source = source)
    coverages <- Rwtss::list_coverages(url)

    # is the WTSS service working?
    assertthat::assert_that(!purrr::is_null(coverages),
                            msg = paste(".source_access_test.wtss_cube: WTSS",
                                        "is unreachable.")
    )

    # is the cube in the list of cubes?
    assertthat::assert_that(
        collection %in% coverages,
        msg = paste(".source_access_test.wtss_cube:",
                    collection, "not available in the WTSS server")
    )

    return(invisible(NULL))
}
#' @keywords internal
#' @export
.source_cube.wtss_cube <- function(source, ...,
                                   collection,
                                   name) {

    cov <- .source_items_new(source = source,
                             collection = collection, ...)

    file_info <- .source_items_fileinfo(source = source, ...,
                                        collection = collection,
                                        wtss_cov = cov)

    cube <- .source_items_cube(source = source,
                               collection = collection,
                               name = name,
                               items = cov,
                               file_info = file_info, ...)

    return(cube)
}

#' @keywords internal
#' @export
.source_items_new.wtss_cube <- function(source = source,
                                        collection = collection, ...) {

    url <- .config_source_url(source = source)

    # describe the cube based on the WTSS API
    cov <- Rwtss::describe_coverage(url, collection, .print = FALSE)
    assertthat::assert_that(!purrr::is_null(cov),
                            msg = paste(".source_items_new.wtss_cube: failed",
                                        "to get cube description in WTSS.")
    )

    return(cov)
}

#' @keywords internal
#' @export
.source_items_fileinfo.wtss_cube <- function(source, ...,
                                             collection,
                                             wtss_cov) {

    url <- .config_source_url(source = source)
    file_info <- tibble::tibble(date = wtss_cov$timeline, path = url)

    return(file_info)
}

#' @keywords internal
#' @export
.source_items_cube.wtss_cube <- function(source,
                                         collection,
                                         name,
                                         items,
                                         file_info, ...) {


    bands <- .config_bands(source = source, collection = collection)

    # create a tibble to store the metadata
    cube_wtss <- .sits_cube_create(
        name = name,
        source = source,
        collection = collection,
        satellite = items$satellite,
        sensor = items$sensor,
        bands = bands,
        nrows = items$nrows,
        ncols = items$ncols,
        xmin = items$xmin,
        xmax = items$xmax,
        ymin = items$ymin,
        ymax = items$ymax,
        xres = items$xres,
        yres = items$yres,
        crs  = items$crs,
        file_info = file_info
    )

    class(cube_wtss) <- c("wtss_cube", class(cube_wtss))

    return(cube_wtss)
}
