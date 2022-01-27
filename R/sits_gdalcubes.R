#' @title Images arrangement in sits cube
#' @name .gc_arrange_images
#'
#' @keywords internal
#'
#' @param cube       A sits data cube
#'
#' @param agg_method A \code{character} with method that will be applied by
#'  \code{gdalcubes} for aggregation. Options: \code{min}, \code{max},
#'  \code{mean}, \code{median} and \code{first}. Default is \code{median}.
#'
#' @param duration   A \code{Duration} object from lubridate package.
#'
#' @param ...        Additional parameters.
#'
#' @return  A sits cube with the images arranged according to some criteria.
.gc_arrange_images <- function(cube, agg_method, duration, ...) {

    class(agg_method) <- agg_method

    UseMethod(".gc_arrange_images", agg_method)
}

#' @keywords internal
#' @export
.gc_arrange_images.default <- function(cube, agg_method, duration, ...) {

    return(cube)
}

#' @keywords internal
#' @export
.gc_arrange_images.first <- function(cube, agg_method, duration, ...) {

    .sits_fast_apply(data = cube, col = "file_info", fn = function(x) {

        tl_length <- max(2,
                         lubridate::interval(start = min(x[["date"]]),
                                             end = max(x[["date"]]))  / duration
        )

        dplyr::group_by(x, date_interval = cut(.data[["date"]], tl_length),
                        .add = TRUE) %>%
            dplyr::arrange(.data[["cloud_cover"]], .by_group = TRUE) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data[["date_interval"]])
    })
}

#' @title Save the images based on an aggregation method.
#' @name .gc_new_cube
#' @keywords internal
#'
#' @param tile          A data cube tile
#'
#' @param res           ...
#'
#' @param resampling    ...
#'
#' @param roi    ...
#'
#' @param output_dir    Directory where the aggregated images will be written.
#'
#' @param multithreads  A \code{numeric} with the number of cores will be used in
#'  the regularize. By default is used 1 core.
#'
#' @param ...         ...
#'
#' @return  A data cube tile with information used in its creation.
.gc_new_cube <- function(tile,
                         res,
                         start_date,
                         resampling,
                         roi,
                         output_dir,
                         multithreads, ...) {

    # set caller to show in errors
    .check_set_caller(".gc_new_cube")

    #bbox <- .cube_tile_bbox(cube = tile)

    tile <- .reg_apply_resample(
        tile = tile,
        resolution = res,
        resampling = resampling,
        output_dir = output_dir
    )

    if (.source_cloud() %in% .cube_bands(tile))
        tile <- .reg_apply_mask(tile = tile)

    tile <- .reg_merge_images(tile = tile,
                              output_dir = output_dir,
                              start_date = start_date)

    return(tile)
}

.reg_apply_resample <- function(tile, resolution, resampling, output_dir) {

    t_bands <- .cube_bands(tile)
    t_file_info <- .file_info(tile)
    t_bbox <- .cube_tile_bbox(tile)

    b_file_info <- dplyr::filter(t_file_info, .data[["xres"]] != resolution &
                                     .data[["yres"]] != resolution) %>%
        slider::slide_dfr(function(fl) {

            band_in <- fl[["band"]]
            img_in <- terra::rast(fl[["path"]])

            img_out_name <- tempfile(
                pattern = band_in,
                tmpdir = output_dir,
                fileext = ".tif"
            )

            # output template
            new_rast <- terra::rast(
                resolution = resolution,
                xmin  = t_bbox[["xmin"]],
                xmax  = t_bbox[["xmax"]],
                ymin  = t_bbox[["ymin"]],
                ymax  = t_bbox[["ymax"]],
                nrows = terra::nrow(img_in),
                ncols = terra::ncol(img_in),
                crs   = terra::crs(img_in)
            )

            terra_datatype <- .source_collection_gdalcubes_type(
                .cube_source(cube = tile),
                collection = .cube_collection(cube = tile)
            )

            img_out <- terra::resample(
                x = img_in,
                y = new_rast,
                method = resampling,
                datatype = terra_datatype,
                gdal = .config_gtiff_default_options(),
                filename = img_out_name,
                memmax = 3
            )

            fl[["path"]] <- img_out_name
            fl[["xres"]] <- terra::xres(img_out)
            fl[["yres"]] <- terra::yres(img_out)
            fl[["xmin"]] <- terra::xmin(img_out)
            fl[["xmax"]] <- terra::xmax(img_out)
            fl[["ymin"]] <- terra::ymin(img_out)
            fl[["ymax"]] <- terra::ymax(img_out)
            fl[["nrows"]] <- terra::ncol(img_out)
            fl[["ncols"]] <- terra::nrow(img_out)

            return(fl)
        })

    if (nrow(b_file_info) == 0)
        return(tile)

    native_bands <- setdiff(t_bands, unique(b_file_info[["band"]]))
    if (length(native_bands) > 0)
        b_file_info <- dplyr::filter(t_file_info,
                                     .data[["band"]] %in% native_bands) %>%
        dplyr::bind_rows(b_file_info) %>%
        dplyr::arrange(.data[["date"]], .data[["fid"]], .data[["band"]])

    tile[["file_info"]][[1]] <- b_file_info

    return(tile)
}


.reg_apply_mask <- function(tile) {

    t_bands <- .cube_bands(tile, add_cloud = FALSE)

    purrr::map(t_bands, function(t_band) {

        t_band <- .file_info(tile, bands = t_band)
        c_band <- .file_info(tile, bands = .source_cloud())

        fl <- slider::slide2_dfr(t_band, c_band, function(t_b, t_c) {

            t_cloud <- terra::rast(t_c[["path"]])
            b_cloud <- terra::rast(t_b[["path"]])

            interp_values <- .source_cloud_interp_values(
                source = .cube_source(cube = tile),
                collection = .cube_collection(cube = tile)
            )

            img_r <- terra::mask(b_cloud,
                                 mask = t_cloud,
                                 maskvalues = interp_values,
                                 filename = t_b[["path"]],
                                 overwrite = TRUE)
        })
    })

    return(tile)
}

.reg_merge_images <- function(tile, output_dir, start_date) {

    t_file_info <- .file_info(tile)
    t_band <- .cube_bands(tile, add_cloud = FALSE)

    file_ext <- tools::file_ext(
        gsub(".*/([^?]*)\\??.*$", "\\1", t_file_info[["path"]][[1]])
    )

    r_filename <- paste0(
        output_dir, "/",
        paste("cube", .cube_tiles(tile), start_date, t_band, sep = "_"),
        ".", file_ext
    )

    t_datatype <- .source_collection_gdalcubes_type(
        .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)
    )

    t_rast_list <- purrr::map(t_file_info[["path"]], terra::rast)

    r_merged <- terra::merge(
        x =  terra::src(t_rast_list),
        filename = r_filename,
        datatype = t_datatype,
        gdal = .config_gtiff_default_options(),
        steps = 10,
        overwrite = TRUE
    )

    unlink(t_file_info[["path"]])

    # set file info values
    n_file_info <- tibble::tibble(
        fid = paste("cube", .cube_tiles(tile), start_date, sep = "_"),
        date = start_date,
        band = t_band,
        xres  = terra::xres(r_merged),
        yres  = terra::yres(r_merged),
        xmin  = terra::xmin(r_merged),
        xmax  = terra::xmax(r_merged),
        ymin  = terra::ymin(r_merged),
        ymax  = terra::ymax(r_merged),
        nrows = terra::nrow(r_merged),
        ncols = terra::ncol(r_merged),
        path = r_filename
    )

    tile[["file_info"]][[1]] <- n_file_info

    return(tile)
}

#' @title Create an object image_mask with information about mask band
#' @name .gc_cloud_mask
#' @keywords internal
#'
#' @param tile Data cube tile from where data is to be retrieved.
#'
#' @return A \code{object} 'image_mask' from gdalcubes containing information
#'  about the mask band.
.gc_cloud_mask <- function(tile) {

    bands <- .cube_bands(tile)
    cloud_band <- .source_cloud()

    # checks if the cube has a cloud band
    .check_chr_within(
        x = cloud_band,
        within = unique(bands),
        discriminator = "any_of",
        msg = paste("It was not possible to use the cloud",
                    "mask, please include the cloud band in your cube")
    )

    # create a image mask object
    mask_values <- gdalcubes::image_mask(
        cloud_band,

    )

    # is this a bit mask cloud?
    if (.source_cloud_bit_mask(
        source = .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)))

        mask_values <- list(
            band = cloud_band,
            min = 1,
            max = 2^16,
            bits = mask_values$values,
            values = NULL,
            invert = FALSE
        )

    class(mask_values) <- "image_mask"

    return(mask_values)
}

#' @title Get the timeline of intersection in all tiles
#' @name .gc_get_valid_timeline
#'
#' @keywords internal
#'
#' @param cube       Data cube from where data is to be retrieved.
#' @param period     A \code{character} with ISO8601 time period for regular
#'  data cubes produced by \code{gdalcubes}, with number and unit, e.g., "P16D"
#'  for 16 days. Use "D", "M" and "Y" for days, month and year.
#'
#' @return a \code{vector} with all timeline values.
.gc_get_valid_timeline <- function(cube, period) {

    # pre-condition
    .check_chr(period, allow_empty = FALSE,
               len_min = 1, len_max = 1,
               msg = "invalid 'period' parameter")

    # start date - maximum of all minimums
    max_min_date <- do.call(
        what = max,
        args = purrr::map(cube[["file_info"]], function(file_info){
            return(min(file_info[["date"]]))
        })
    )

    # end date - minimum of all maximums
    min_max_date <- do.call(
        what = min,
        args = purrr::map(cube[["file_info"]], function(file_info){
            return(max(file_info[["date"]]))
        }))

    # check if all timeline of tiles intersects
    .check_that(
        x = max_min_date <= min_max_date,
        msg = "the timeline of the cube tiles do not intersect."
    )

    if (substr(period, 3, 3) == "M") {
        max_min_date <- lubridate::date(paste(
            lubridate::year(max_min_date),
            lubridate::month(max_min_date),
            "01", sep = "-"))
    } else if (substr(period, 3, 3) == "Y") {
        max_min_date <- lubridate::date(paste(
            lubridate::year(max_min_date),
            "01", "01", sep = "-"))
    }

    # generate timeline
    date <- lubridate::ymd(max_min_date)
    min_max_date <- lubridate::ymd(min_max_date)
    tl <- date
    while (TRUE) {
        date <- lubridate::ymd(date) %m+% lubridate::period(period)
        if (date > min_max_date) break
        tl <- c(tl, date)
    }

    # timeline cube
    tiles_tl <- suppressWarnings(sits_timeline(cube))

    if (!is.list(tiles_tl))
        tiles_tl <- list(tiles_tl)

    return(tl)
}
