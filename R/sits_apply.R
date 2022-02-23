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
#' @param memsize           memory available for classification (in GB).
#' @param multicores        number of cores to be used for classification.
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
NULL

#' @rdname sits_apply
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
sits_apply.raster_cube <- function(data, ...,
                                   memsize = 1,
                                   multicores = 2,
                                   output_dir = getwd(),
                                   progress = TRUE) {

    .check_set_caller("sits_apply.raster_cube")
    progress <- .check_documentation(progress)

    # capture dots as a list of quoted expressions
    list_expr <- lapply(substitute(list(...), env = environment()),
                        unlist, recursive = FALSE)[-1]

    # check bands names from expression
    .check_lst(list_expr, min_len = 1, max_len = 1,
               msg = "invalid expression value")

    # get out band
    out_band <- toupper(names(list_expr))
    names(list_expr) <- out_band

    # define function to give a name to output new file
    out_file_name <- function(tile_name, band, date, output_dir) {
        # prepare file name
        file_prefix <- paste("cube", tile_name, band, date, sep = "_")
        file_name <- paste(file_prefix, "tif", sep = ".")
        file_path <- paste(output_dir, file_name, sep = "/")
        return(file_path)
    }

    # define function to return a file_info of created band
    out_file_info <- function(in_fi_fid, band, output_file) {

        tibble::tibble(
            fid = in_fi_fid[["fid"]][[1]],
            date = in_fi_fid[["date"]][[1]],
            band = band,
            xmin = in_fi_fid[["xmin"]][[1]],
            xmax = in_fi_fid[["xmax"]][[1]],
            ymin = in_fi_fid[["ymin"]][[1]],
            ymax = in_fi_fid[["ymax"]][[1]],
            xres = in_fi_fid[["xres"]][[1]],
            yres = in_fi_fid[["yres"]][[1]],
            nrows = in_fi_fid[["nrows"]][[1]],
            ncols = in_fi_fid[["ncols"]][[1]],
            path = output_file)
    }

    # TODO: dry run expression

    # prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # traverse each tile
    result <- slider::slide_dfr(data, function(tile) {


        # get all input bands in cube data
        in_bands <- .cube_bands(tile)

        # find which bands are in input expressions
        char_expr <- toupper(deparse(list_expr[[out_band]]))
        used_bands <- purrr::map_lgl(in_bands, grepl, char_expr)

        # pre-condition
        .check_that(any(used_bands),
                    local_msg = "no valid band was informed",
                    msg = "invalid expression value")

        # get input bands
        in_bands <- in_bands[used_bands]

        # get all fids for this tile
        fids <- .file_info_fids(tile)

        # traverse each fid
        file_info <- purrr::map_dfr(fids, function(fid) {

            # get file_info filtered by bands
            in_fi_fid <- .file_info(tile, bands = in_bands,
                                    fid = fid)

            # prepare output file name
            out_file_path <- out_file_name(
                tile_name = .cube_tiles(tile),
                band = out_band,
                date = in_fi_fid[["date"]][[1]],
                output_dir = output_dir)

            # check if output file exists and returns it
            if (file.exists(out_file_path)) {
                return(out_file_info(in_fi_fid = in_fi_fid,
                                     band = out_band,
                                     output_file = out_file_path))
            }

            # divide the input data in blocks
            blocks <- .sits_raster_blocks_apply(
                tile = tile,
                sub_image = .sits_raster_sub_image_default(tile),
                memsize = memsize,
                multicores = multicores)

            # save each output value
            blocks_path <- .sits_parallel_map(blocks, function(b) {


                # define the file name of the raster file to be written
                filename_block <- paste0(
                    tools::file_path_sans_ext(out_file_path),
                    "_block_", b[["first_row"]], "_", b[["nrows"]], ".tif")

                # resume feature
                if (file.exists(filename_block))
                    return(filename_block)

                # load bands data
                in_values <- tryCatch({
                    purrr::map(in_bands, function(band) {

                        # transform file_info columns as bands and values
                        # as paths
                        in_files <- in_fi_fid %>%
                            dplyr::select(dplyr::all_of(c("band", "path"))) %>%
                            tidyr::pivot_wider(names_from = "band",
                                               values_from = "path")

                        # get the missing values, minimum values and scale
                        # factors
                        missing_value <-
                            .cube_band_missing_value(tile, band = band)
                        minimum_value <-
                            .cube_band_minimum_value(tile, band = band)
                        maximum_value <-
                            .cube_band_maximum_value(tile, band = band)

                        # scale the data set
                        scale_factor <-
                            .cube_band_scale_factor(tile, band = band)
                        offset_value <-
                            .cube_band_offset_value(tile, band = band)

                        # read the values
                        values <- .raster_read_stack(in_files[[band]],
                                                     block = b)

                        # correct NA, minimum, maximum, and missing values
                        values[values == missing_value] <- NA
                        values[values < minimum_value] <- NA
                        values[values > maximum_value] <- NA

                        # compute scale and offset
                        values <- scale_factor * values + offset_value

                        return(values)
                    })
                }, error = function(e) NULL)

                # check if an error occurred
                if (is.null(in_values)) {
                    return(NULL)
                }

                # set band names
                names(in_values) <- in_bands

                # evaluate expressions, scale and offset values
                out_values <- eval(list_expr[[out_band]], in_values)

                # apply scale and offset
                out_values <- out_values /
                    .config_get("raster_cube_scale_factor") -
                    .config_get("raster_cube_offset_value")

                # compute block spatial parameters
                params <- .cube_params_block(tile, block = b)

                # new raster
                r_obj <- .raster_new_rast(
                    nrows = params[["nrows"]],
                    ncols = params[["ncols"]],
                    xmin = params[["xmin"]],
                    xmax = params[["xmax"]],
                    ymin = params[["ymin"]],
                    ymax = params[["ymax"]],
                    nlayers = 1,
                    crs = params[["crs"]])

                # set values
                r_obj <- .raster_set_values(r_obj, out_values)

                # write values
                .raster_write_rast(
                    r_obj = r_obj,
                    file = filename_block,
                    format = "GTiff",
                    data_type = .config_get("raster_cube_data_type"),
                    gdal_options = .config_gtiff_default_options(),
                    overwrite = FALSE,
                    missing_value = .config_get("raster_cube_missing_value"))

                # clean memory
                gc()

                return(filename_block)
            }, progress = progress)

            # merge result
            blocks_path <- unlist(blocks_path)

            # join predictions
            if (!is.null(blocks_path)) {
                .raster_merge(
                    in_files = blocks_path,
                    out_file = out_file_path,
                    format = "GTiff",
                    gdal_datatype = .raster_gdal_datatype(
                        .config_get("raster_cube_data_type")),
                    gdal_options = .config_gtiff_default_options(),
                    overwrite = TRUE,
                    progress = progress
                )
            }

            # prepare output file_info
            out_file_info_fid <- out_file_info(
                in_fi_fid = in_fi_fid,
                band = out_band,
                output_file = out_file_path)

            return(out_file_info_fid)
        })


        file_info <-  dplyr::bind_rows(file_info, .file_info(tile)) %>%
            dplyr::arrange(dplyr::across(
                dplyr::all_of(c("date", "band", "fid"))))

        out_tile <- .cube_create(
            source = .cube_source(tile),
            collection = .cube_collection(tile),
            satellite = .source_collection_satellite(.cube_source(tile),
                                                     .cube_collection(tile)),
            sensor = .source_collection_sensor(.cube_source(tile),
                                               .cube_collection(tile)),
            tile = .cube_tiles(tile),
            xmin = tile[["xmin"]],
            xmax = tile[["xmax"]],
            ymin = tile[["ymin"]],
            ymax = tile[["ymax"]],
            crs = tile[["crs"]],
            labels = tile[["labels"]],
            file_info = file_info)

        return(out_tile)
    })

    class(result) <- c("raster_cube", class(result))

    return(result)
}

#' @rdname sits_apply
#' @keywords internal
.apply_across <- function(data, fn, ...) {

    .check_set_caller(".apply_across")

    fn_across <- fn
    .sits_fast_apply(data, col = "time_series", fn = function(x, ...) {
        dplyr::mutate(x, dplyr::across(dplyr::matches(sits_bands(data)),
                                       fn_across, ...))
    }, ...)
}
