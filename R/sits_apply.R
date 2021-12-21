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

#' @rdname sits_apply
#' @export
sits_apply.sits <- function(data, ...) {

    .check_set_caller("sits_apply.sits")

    .sits_fast_apply(data, col = "time_series", fn = dplyr::mutate, ...)
}

#' @rdname sits_apply
#' @export
sits_apply.raster_cube <- function(data, ..., output_dir = getwd()) {

    .check_set_caller("sits_apply.raster_cube")

    # capture dots as a list of quoted expressions
    list_expr <- lapply(substitute(list(...), env = environment()),
                        unlist, recursive = F)[-1]

    # suppress gdalcubes progress bar
    gdalcubes::gdalcubes_options(show_progress = FALSE)

    # slide tiles
    result <- slider::slide_dfr(data, function(tile) {

        fids <- unique(.cube_file_info(tile)[["fid"]])
        tile[["file_info"]][[1]] <- purrr::map_dfr(fids, function(fid) {

            tile_fid <- tile

            tile_fid[["file_info"]][[1]] <-
                dplyr::filter(.cube_file_info(tile),
                              .data[["fid"]] == !!fid)

            toi <- .gc_get_valid_interval(tile_fid, period = "P1D")

            ic <- .gc_create_database(tile_fid,
                                      path_db = tempfile(fileext = ".db"))

            bands <- names(list_expr)

            .check_that(length(bands) == length(list_expr),
                        local_msg = "not all expressions have names",
                        msg = "invalid expressions parameters")


            cv <- .gc_create_cube_view(
                tile = tile_fid,
                period = "P1D",
                res = .cube_resolution(tile_fid),
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

                # file prefix
                prefix <- paste("cube", tile_fid[["tile"]], band, "", sep = "_")

                gdalcubes::write_tif(
                    cc,
                    dir = output_dir,
                    prefix = prefix,
                    creation_options = list("COMPRESS" = "LZW",
                                            "BIGTIFF" = "YES"),
                    pack = list(type = "int16", nodata = -9999,
                                scale = 1, offset = 0)
                )

                file_name <- paste0(output_dir, "/", prefix,
                                    .cube_file_info(tile_fid)[["date"]][[1]],
                                    ".tif")
                return(file_name)
            })

            file_info <- .cube_file_info(tile_fid)

            file_info <- tidyr::unnest(tibble::tibble(
                fid = file_info[["fid"]][[1]],
                date = file_info[["date"]][[1]],
                band = bands,
                xmin = file_info[["xmin"]][[1]],
                xmax = file_info[["xmax"]][[1]],
                ymin = file_info[["ymin"]][[1]],
                ymax = file_info[["ymax"]][[1]],
                xres = file_info[["xres"]][[1]],
                yres = file_info[["yres"]][[1]],
                nrows = file_info[["nrows"]][[1]],
                ncols = file_info[["ncols"]][[1]],
                path = output_files
            ), cols = c("date", "path"))

            file_info_fid <- dplyr::bind_rows(tile_fid[["file_info"]][[1]],
                                              file_info) %>%
                dplyr::arrange(date, band)

            return(file_info_fid)
        })

        return(tile)
    })

    return(result)
}

#' @title Apply a function to a set of time series
#' @name .apply_across
#' @keywords internal
.apply_across <- function(data, fn, ...) {

    .check_set_caller(".apply_across")

    fn_across <- fn
    .sits_fast_apply(data, col = "time_series", fn = function(x, ...) {
        dplyr::mutate(x, dplyr::across(dplyr::matches(sits_bands(data)),
                                       fn_across, ...))
    }, ...)
}
