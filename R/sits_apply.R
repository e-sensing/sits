#' @title Apply a function on a set of time series
#'
#' @name sits_apply
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Apply a named expression to a sits cube or a sits tibble
#' to be evaluated and generate new bands (indices). In the case of sits
#' cubes, it materializes a new band in `output_dir` using `gdalcubes`.
#'
#' @param data          Valid sits tibble or cube
#' @param output_dir    Directory where files will be saved.
#' @param ...           Named expressions to be evaluated.
#'
#' @return A sits tibble or a sits cube with new bands.
#'
#' @examples
#' # Get a time series
#' # apply a normalization function
#'
#' point2 <-
#'   sits_select(point_mt_6bands, "NDVI") %>%
#'   sits_apply(NDVI_norm = (NDVI - min(NDVI)) / (max(NDVI) - min(NDVI))
#' )
#'
#' @export
sits_apply <- function(data, ...) {

    UseMethod("sits_apply", data)
}
#' @export
sits_apply.sits <- function(data, ...) {

    .check_set_caller("sits_apply.sits")

    .sits_fast_apply(data, col = "time_series", fn = dplyr::mutate, ...)
}
#'
#' @export
sits_apply.raster_cube <- function(data, ..., output_dir = getwd()) {

    .check_set_caller("sits_apply.raster_cube")

    toi <- .gc_get_valid_interval(data)

    ic <- .gc_create_database(data, path_db = tempfile(fileext = ".db"))

    # capture dots as a list of quoted expressions
    list_expr <- lapply(substitute(list(...), env = environment()),
                        unlist, recursive = F)[-1]

    bands <- names(list_expr)

    .check_that(length(bands) == length(list_expr),
                local_msg = "not all expressions have names",
                msg = "invalid expressions parameters")

    result <- slider::slide_dfr(data, function(tile) {

        cv <- .gc_create_cube_view(
            tile = tile,
            period = tile[["period"]],
            res = .cube_resolution(tile),
            roi = NULL,
            toi = toi,
            agg_method = "first",
            resampling = "bilinear"
        )

        rc <- gdalcubes::raster_cube(ic, view = cv)

        output_files <- purrr::map(bands, function(band) {

            cc <- gdalcubes::apply_pixel(rc,
                                         expr = deparse(list_expr[[band]]),
                                         names = band)

            gdalcubes::write_tif(
                cc,
                dir = output_dir,
                prefix = paste("cube", tile[["tile"]], band, "", sep = "_"),
                creation_options = list("COMPRESS" = "LZW", "BIGTIFF" = "YES"),
                pack = list(type = "int16", nodata = -9999, scale = 1,
                            offset = 0)
            )
        })

        # retrieve dates
        dates <- purrr::map(output_files, function(x) {
            dplyr::tibble(date = .gc_get_date(x))
        })

        output_files <- purrr::map(output_files, function(x) {
            dplyr::tibble(path = x)
        })

        res <- .cube_resolution(tile)

        file_info <- tidyr::unnest(tibble::tibble(
            date = dates,
            band = bands,
            xres = res[["xres"]],
            yres = res[["yres"]],
            path = output_files
        ), cols = c("date", "path"))

        tile[["file_info"]][[1]] <-
            dplyr::bind_rows(tile[["file_info"]][[1]],
                             file_info) %>%
            dplyr::arrange(date, band)

        tile
    })

    return(result)
}

.apply_across <- function(data, fn, ...) {

    .check_set_caller(".apply_across")

    fn_across <- fn
    .sits_fast_apply(data, col = "time_series", fn = function(x, ...) {
        dplyr::mutate(x, dplyr::across(dplyr::matches(sits_bands(data)),
                                       fn_across, ...))
    }, ...)
}
