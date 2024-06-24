#' @title Images arrangement in sits cube
#' @name .gc_arrange_images
#'
#' @keywords internal
#' @noRd
#' @param cube       Data cube.
#' @param timeline   Timeline of regularized cube
#' @param period     Period of interval to aggregate images
#' @param roi        Optional. Used only for Sentinel-1 cube.
#' @param ...        Additional parameters.
#'
#' @return           Data cube with the images arranged by cloud.
.gc_arrange_images <- function(cube, timeline, period, ...) {
    UseMethod(".gc_arrange_images", cube)
}

#' @keywords internal
#' @noRd
#' @export
.gc_arrange_images.raster_cube <- function(cube, timeline, period, ...) {
    # include the end of last interval
    timeline <- c(
        timeline,
        timeline[[length(timeline)]] %m+% lubridate::period(period)
    )

    # filter and change image order according to cloud coverage
    cube <- .apply(cube, "file_info", function(x) {
        x <- dplyr::filter(
            x, .data[["date"]] >= timeline[[1]],
            .data[["date"]] < timeline[[length(timeline)]]
        )

        x <- dplyr::group_by(
            x,
            interval = cut(.data[["date"]], timeline, labels = FALSE),
            .add = TRUE
        )

        if ("cloud_cover" %in% names(x)) {
            x <- dplyr::arrange(
                x, .data[["cloud_cover"]],
                .by_group = TRUE
            )
        }
        x <- dplyr::select(dplyr::ungroup(x), -"interval")

        return(x)
    })

    return(cube)
}

#' @title Create a cube_view object
#' @name .gc_create_cube_view
#' @keywords internal
#' @noRd
#'
#' @param tile       Data cube tile
#' @param period     Period of time in which it is desired to apply in the cube,
#'                   must be provided based on ISO8601, where 1 number and a
#'                   unit are provided, for example "P16D".
#' @param res        Spatial resolution of the image that
#'                   will be aggregated.
#' @param roi        Region of interest.
#' @param toi        Timeline of intersection.
#' @param agg_method Aggregation method.
#' @param resampling Resampling method.
#'                   Options: \code{near}, \code{bilinear}, \code{bicubic} or
#'                   others supported by gdalwarp
#'                   (see https://gdal.org/programs/gdalwarp.html).
#'                   Default is "bilinear".
#'
#' @return           \code{Cube_view} object from gdalcubes.
.gc_create_cube_view <- function(tile,
                                 period,
                                 res,
                                 roi,
                                 date,
                                 agg_method,
                                 resampling) {
    # set caller to show in errors
    .check_set_caller(".gc_create_cube_view")

    # pre-conditions
    .check_that(nrow(tile) == 1)


    # get bbox roi
    if (!is.null(roi)) {
        bbox_roi <- .bbox(roi, as_crs = .tile_crs(tile))
    } else {
        bbox_roi <- .tile_bbox(tile)
    }

    # create a gdalcubes extent
    extent <- list(
        left   = bbox_roi[["xmin"]],
        right  = bbox_roi[["xmax"]],
        bottom = bbox_roi[["ymin"]],
        top    = bbox_roi[["ymax"]],
        t0     = format(date, "%Y-%m-%d"),
        t1     = format(date, "%Y-%m-%d")
    )

    # create a list of cube view
    cv <- suppressMessages(
        gdalcubes::cube_view(
            extent = extent,
            srs = .cube_crs(tile),
            dt = period,
            dx = res,
            dy = res,
            aggregation = agg_method,
            resampling = resampling
        )
    )

    return(cv)
}

#' @title Create an gdalcubes::image_mask object
#' @name .gc_create_cloud_mask
#' @keywords internal
#' @noRd
#'
#' @param tile  A cube tile.
#'
#' @return      \code{gdalcubes::image_mask} with information about mask band.
.gc_create_cloud_mask <- function(tile) {
    # set caller to show in errors
    .check_set_caller(".gc_create_cloud_mask")

    if (!.tile_contains_cloud(tile)) {
        return(NULL)
    }
    # create a image mask object
    mask_values <- suppressMessages(
        gdalcubes::image_mask(
            band = .source_cloud(),
            values = .source_cloud_interp_values(
                source = .cube_source(cube = tile),
                collection = .cube_collection(cube = tile)
            )
        )
    )
    # is this a bit mask cloud?
    if (.source_cloud_bit_mask(
        source = .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)
    )) {
        mask_values <- list(
            band = .source_cloud(),
            min = 1,
            max = 2^16,
            bits = mask_values[["values"]],
            values = NULL,
            invert = FALSE
        )
    }

    class(mask_values) <- "image_mask"

    return(mask_values)
}

#' @title Create an image_collection object
#' @name .gc_create_database_stac
#'
#' @keywords internal
#' @noRd
#' @param cube      Data cube from where data is to be retrieved.
#' @param path_db   Path and name for gdalcubes database.
#' @return          Image_collection containing information on the
#'                  images metadata.
.gc_create_database_stac <- function(cube, path_db) {
    # deleting the existing database to avoid errors in the stac database
    if (file.exists(path_db)) {
        unlink(path_db)
    }

    # use crs from tile if there is no crs in file_info
    if ("crs" %in% names(.fi(cube))) {
        file_info <- dplyr::select(cube, "file_info") |>
            tidyr::unnest(cols = "file_info")
    } else {
        file_info <- dplyr::select(cube, "file_info", "crs") |>
            tidyr::unnest(cols = "file_info")
    }

    # can be "proj:epsg" or "proj:wkt2"
    crs_type <- .gc_detect_crs_type(file_info[["crs"]][[1]])

    file_info <- file_info |>
        dplyr::transmute(
            fid  = .data[["fid"]],
            xmin = .data[["xmin"]],
            ymin = .data[["ymin"]],
            xmax = .data[["xmax"]],
            ymax = .data[["ymax"]],
            crs  = .data[["crs"]],
            href = .data[["path"]],
            datetime = as.character(.data[["date"]]),
            band = .data[["band"]],
            !!crs_type := gsub("^EPSG:", "", .data[["crs"]])
        )

    features <- dplyr::mutate(file_info, id = .data[["fid"]]) |>
        tidyr::nest(features = -"fid")

    features <- slider::slide_dfr(features, function(feat) {
        bbox <- .bbox(feat[["features"]][[1]][1, ], as_crs = "EPSG:4326")

        feat[["features"]][[1]] <- dplyr::mutate(feat[["features"]][[1]],
            xmin = bbox[["xmin"]],
            xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]],
            ymax = bbox[["ymax"]]
        )

        feat
    })

    gc_data <- purrr::map(features[["features"]], function(feature) {
        feature <- feature |>
            dplyr::select(-"crs") |>
            tidyr::nest(assets = c("href", "band")) |>
            tidyr::nest(properties = c(
                "datetime",
                !!crs_type
            )) |>
            tidyr::nest(bbox = c(
                "xmin", "ymin",
                "xmax", "ymax"
            ))

        feature[["assets"]] <- purrr::map(feature[["assets"]], function(asset) {
            asset |>
                tidyr::pivot_wider(
                    names_from = "band",
                    values_from = "href"
                ) |>
                purrr::map(
                    function(x) list(href = x, `eo:bands` = list(NULL))
                )
        })

        feature <- unlist(feature, recursive = FALSE)
        feature[["properties"]] <- c(feature[["properties"]])
        feature[["bbox"]] <- unlist(feature[["bbox"]])
        feature
    })

    ic_cube <- suppressMessages(
        gdalcubes::stac_image_collection(
            s = gc_data,
            out_file = path_db,
            url_fun = identity
        )
    )

    return(ic_cube)
}

#' @title Create a gdalcubes::pack object
#' @name .gc_create_pack
#' @keywords internal
#' @noRd
#' @param cube   a sits cube object
#' @param band   a \code{character} band name
#'
#' @return an \code{gdalcube::pack} object.
.gc_create_pack <- function(cube, band) {
    # set caller to show in errors
    .check_set_caller(".gc_create_pack")

    conf <- .tile_band_conf(cube, band)

    pack <- list(
        type = .conf("gdalcubes_type_format"),
        nodata = .miss_value(conf),
        scale = 1,
        offset = 0
    )

    return(pack)
}

#' @title Create an gdalcubes::raster_cube object
#' @name .gc_create_raster_cube
#' @keywords internal
#' @noRd
#' @param cube_view    \code{gdalcubes::cube_view} object.
#' @param path_db      Path to a gdalcubes database.
#' @param band         Band name to be generated
#' @param mask_band    \code{gdalcubes::image_mask} object with metadata
#'                      about the band to be used to mask clouds.
#' @return             \code{gdalcubes::image_mask} with info on mask band.
#'
.gc_create_raster_cube <- function(cube_view, path_db, band, mask_band) {
    # set caller to show in errors
    .check_set_caller(".gc_create_raster_cube")

    # open db in each process
    img_col <- suppressMessages(
        gdalcubes::image_collection(path = path_db)
    )

    # create a gdalcubes::raster_cube object
    raster_cube <- suppressMessages(
        gdalcubes::raster_cube(
            image_collection = img_col,
            view = cube_view,
            mask = mask_band,
            chunking = .conf("gdalcubes_chunk_size")
        )
    )

    # filter band of raster_cube
    raster_cube <- suppressMessages(
        gdalcubes::select_bands(
            cube = raster_cube,
            bands = band
        )
    )

    return(raster_cube)
}

#' @title Get the timeline of intersection in all tiles
#' @name .gc_get_valid_timeline
#'
#' @keywords internal
#' @noRd
#' @param cube       Data cube.
#' @param period     ISO8601 time period.
#' @param extra_date_step Add an extra date in the end of timeline?
#'
#' @return a \code{vector} with all timeline values.
.gc_get_valid_timeline <- function(cube, period, extra_date_step = FALSE) {
    # set caller to show in errors
    .check_set_caller(".gc_get_valid_timeline")

    # start date - maximum of all minimums
    max_min_date <- do.call(
        what = max,
        args = purrr::map(cube[["file_info"]], function(file_info) {
            return(min(file_info[["date"]]))
        })
    )
    # end date - minimum of all maximums
    min_max_date <- do.call(
        what = min,
        args = purrr::map(cube[["file_info"]], function(file_info) {
            return(max(file_info[["date"]]))
        })
    )
    # check if all timeline of tiles intersects
    .check_that(max_min_date <= min_max_date)
    # define dates for period
    if (substr(period, 3, 3) == "M") {
        max_min_date <- lubridate::date(paste(
            lubridate::year(max_min_date),
            lubridate::month(max_min_date),
            "01",
            sep = "-"
        ))
    } else if (substr(period, 3, 3) == "Y") {
        max_min_date <- lubridate::date(paste(
            lubridate::year(max_min_date),
            "01", "01",
            sep = "-"
        ))
    }
    # generate timeline
    date <- lubridate::ymd(max_min_date)
    min_max_date <- lubridate::ymd(min_max_date)
    tl <- date
    repeat {
        date <- lubridate::ymd(date) %m+% lubridate::period(period)
        if (date > min_max_date) break
        tl <- c(tl, date)
    }
    # Add extra time step
    if (extra_date_step) {
        tl <- c(tl, tl[[length(tl)]] %m+% lubridate::period(period))
    }
    return(tl)
}

#' @title Saves the images of a raster cube.
#' @name .gc_save_raster_cube
#' @keywords internal
#' @noRd
#' @param raster_cube  \code{gdalcubes::raster_cube} object.
#' @param pack         \code{gdalcubes::pack} object.
#' @param output_dir   Directory where the aggregated images will be written.
#' @param files_prefix File names prefix.
#' @param ...          Additional parameters that can be included. See
#'                     '?gdalcubes::write_tif'.
#'
#' @return  A list of generated images.
#'
.gc_save_raster_cube <- function(raster_cube,
                                 pack,
                                 output_dir,
                                 files_prefix, ...) {
    # set caller to show in errors
    .check_set_caller(".gc_save_raster_cube")

    # convert sits gtiff options to gdalcubes format
    gtiff_options <- strsplit(.conf("gdalcubes_options"),
                              split = "=", fixed = TRUE)
    gdalcubes_co <- purrr::map(gtiff_options, `[[`, 2)
    names(gdalcubes_co) <- purrr::map_chr(gtiff_options, `[[`, 1)

    # get cog config parameters
    generate_cog <- .conf("gdalcubes_cog_generate")
    cog_overview <- .conf("gdalcubes_cog_resample_overview")

    # write the aggregated cubes
    img_paths <- suppressMessages(
        gdalcubes::write_tif(
            x = raster_cube,
            dir = output_dir,
            prefix = files_prefix,
            creation_options = gdalcubes_co,
            pack = pack,
            COG = generate_cog,
            rsmpl_overview = cog_overview, ...
        )
    )
    # post-condition
    .check_that(length(img_paths) >= 1)

    return(img_paths)
}

#' @title Build a regular data cube from an irregular one
#'
#' @name .gc_regularize
#' @keywords internal
#' @noRd
#' @description Creates cubes with regular time intervals
#'  using the gdalcubes package.
#'
#' @references Appel, Marius; Pebesma, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092.
#'
#'
#' @param cube       Data cube whose spacing of observation
#'                   times is not constant and will be regularized
#'                   by the \code{gdalcubes} package.
#' @param output_dir Valid directory where the
#'                   regularized images will be written.
#' @param period     ISO8601 time period for regular data cubes
#'                   with number and unit, e.g., "P16D" for 16 days.
#'                   Use "D", "M" and "Y" for days, month and year.
#' @param res        Spatial resolution of the regularized images.
#' @param roi        A named \code{numeric} vector with a region of interest.
#' @param multicores Number of cores used for regularization.
#' @param progress   Show progress bar?
#' @param ...        Additional parameters for httr package.
#'
#' @return             Data cube with aggregated images.
.gc_regularize <- function(cube,
                           period,
                           res,
                           roi,
                           output_dir,
                           multicores = 1,
                           progress = progress) {
    # set caller to show in errors
    .check_set_caller(".gc_regularize")
    # require gdalcubes package
    .check_require_packages("gdalcubes")

    # prepare temp_output_dir
    temp_output_dir <- file.path(output_dir, ".sits")
    if (!dir.exists(temp_output_dir)) {
        dir.create(temp_output_dir, recursive = TRUE)
    }

    # filter only intersecting tiles
    if (.has(roi)) {
        cube <- .cube_filter_spatial(cube, roi = roi)
    }

    # timeline of intersection
    timeline <- .gc_get_valid_timeline(cube, period = period)

    # least_cc_first requires images ordered based on cloud cover
    cube <- .gc_arrange_images(
        cube = cube,
        timeline = timeline,
        period = period,
        roi = roi
    )
    # start processes
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop())

    # does a local cube exist
    local_cube <- tryCatch(
        {
            sits_cube(
                source = .cube_source(cube),
                collection = .cube_collection(cube),
                data_dir = temp_output_dir,
                multicores = multicores,
                progress = progress
            )
        },
        error = function(e) {
            return(NULL)
        }
    )
    # find the tiles that have not been processed yet
    jobs <- .gc_missing_tiles(
        cube = cube,
        local_cube = local_cube,
        timeline = timeline
    )
    # recovery mode
    finished <- length(jobs) == 0

    while (!finished) {
        # for cubes that have a time limit to expire - mpc cubes only
        cube <- .cube_token_generator(cube)

        # process bands and tiles in parallel
        .parallel_map(jobs, function(job) {
            # get parameters from each job
            tile_name <- job[[1]]
            band <- job[[2]]
            date <- job[[3]]

            # we consider token is expired when the remaining time is
            # less than 5 minutes
            if (.cube_is_token_expired(cube))
                return(NULL)

            # filter tile
            tile <- dplyr::filter(cube, .data[["tile"]] == !!tile_name)
            # post-condition
            .check_that(nrow(tile) == 1)

            # append gdalcubes path
            path_db <- tempfile(pattern = "gc", fileext = ".db")
            # create an image collection
            .gc_create_database_stac(cube = tile, path_db = path_db)
            # create a gdalcubes::cube_view
            cube_view <- .gc_create_cube_view(
                tile = tile,
                period = period,
                roi = roi,
                res = res,
                date = date,
                agg_method = "first",
                resampling = "bilinear"
            )

            # create a gdalcubes::raster_cube object
            raster_cube <- .gc_create_raster_cube(
                cube_view = cube_view,
                path_db = path_db,
                band = band,
                mask_band = .gc_create_cloud_mask(tile)
            )

            # files prefix
            prefix <- paste(tile[["satellite"]],
                tile[["sensor"]],
                .cube_tiles(tile),
                band, "",
                sep = "_"
            )

            # check documentation mode
            progress <- .check_documentation(progress)

            # gdalcubes log file
            gdalcubes_log_file <- file.path(tempdir(), "/gdalcubes.log")
            # setting threads to process
            gdalcubes::gdalcubes_options(
                parallel = 2,
                debug = FALSE,
                log_file = gdalcubes_log_file,
                show_progress = progress
            )

            # create of the aggregate cubes
            tryCatch(
                {
                    .gc_save_raster_cube(
                        raster_cube = raster_cube,
                        pack = .gc_create_pack(cube = tile, band = band),
                        output_dir = temp_output_dir,
                        files_prefix = prefix
                    )
                },
                error = function(e) {
                    return(NULL)
                }
            )
        }, progress = progress)

        # create local cube from files in output directory
        local_cube <- tryCatch(
            {
                sits_cube(
                    source = .cube_source(cube),
                    collection = .cube_collection(cube),
                    data_dir = temp_output_dir,
                    multicores = multicores,
                    progress = FALSE
                )
            },
            error = function(e) {
                return(NULL)
            }
        )

        # find if there are missing tiles
        jobs <- .gc_missing_tiles(
            cube = cube,
            local_cube = local_cube,
            timeline = timeline
        )

        # have we finished?
        finished <- length(jobs) == 0

        # inform the user
        if (!finished) {
            # convert list of missing tiles and bands to a list of vectors
            tiles_bands <- purrr::transpose(jobs)
            tiles_bands <- purrr::map(tiles_bands, unlist)

            # get missing tiles
            bad_tiles <- unique(tiles_bands[[1]])

            # get missing bands per missing tile
            msg <- paste(
                bad_tiles,
                purrr::map_chr(bad_tiles, function(tile) {
                    paste0(
                        "(",
                        toString(
                            unique(
                                tiles_bands[[2]][tiles_bands[[1]] == tile]
                            )
                        ),
                        ")"
                    )
                }),
                collapse = ", "
            )

            # show message
            message("tiles", msg, "are missing or malformed", "
                    and will be reprocessed."
            )

            # remove cache
            .parallel_stop()
            .parallel_start(workers = multicores)
        }
    }
    # Crop files
    if (.has(roi)) {
        # Crop files to output_dir
    } else {
        # Move files to output_dir
    }
    return(local_cube)
}

#' @title Detect the type of cube crs
#'
#' @name .gc_detect_crs_type
#' @keywords internal
#' @noRd
#' @param cube_crs A vector of characters with cube crs.
#'
#' @return A character with the type of crs: "proj:wkt2" or "proj:epsg"
.gc_detect_crs_type <- function(cube_crs) {
    if (all(is.numeric(cube_crs)) ||
            all(startsWith(cube_crs, prefix = "EPSG"))
        ) {
        return("proj:epsg")
    }
    return("proj:wkt2")
}

#' @title Finds the missing tiles in a regularized cube
#'
#' @name .gc_missing_tiles
#' @keywords internal
#' @noRd
#' @param cube     Original cube to be regularized.
#' @param gc_cube  Regularized cube (may be missing tiles).
#' @param timeline Timeline used by gdalcubes for regularized cube
#' @param period   Period of timeline regularization.
#'
#' @return         Tiles that are missing from the regularized cube.
.gc_missing_tiles <- function(cube, local_cube, timeline) {
    # do a cross product on tiles and bands
    tiles_bands_times <- unlist(slider::slide(cube, function(tile) {
        bands <- .cube_bands(tile, add_cloud = FALSE)
        tidyr::expand_grid(
            tile = .cube_tiles(tile), band = bands,
            time = timeline
        ) |>
            purrr::pmap(function(tile, band, time) {
                return(list(tile, band, time))
            })
    }), recursive = FALSE)

    # if regularized cube does not exist, return all tiles from original cube
    if (is.null(local_cube)) {
        return(tiles_bands_times)
    }

    # do a cross product on tiles and bands
    gc_tiles_bands_times <- unlist(slider::slide(local_cube, function(tile) {
        bands <- .cube_bands(tile, add_cloud = FALSE)
        tidyr::expand_grid(
            tile = .cube_tiles(tile), band = bands,
            time = timeline
        ) |>
            purrr::pmap(function(tile, band, time) {
                return(list(tile, band, time))
            })
    }), recursive = FALSE)

    # first, include tiles and bands that have not been processed
    miss_tiles_bands_times <-
        tiles_bands_times[!tiles_bands_times %in% gc_tiles_bands_times]

    # second, include tiles and bands that have been processed
    proc_tiles_bands_times <-
        tiles_bands_times[tiles_bands_times %in% gc_tiles_bands_times]

    # do all tiles and bands in local_cube have the same timeline as
    # the original cube?
    bad_timeline <- purrr::pmap_lgl(
        purrr::transpose(proc_tiles_bands_times),
        function(tile, band, date) {
            tile <- local_cube[local_cube[["tile"]] == tile, ]
            tile <- sits_select(tile, bands = band)
            return(!date %in% sits_timeline(tile))
        }
    )

    # update malformed processed tiles and bands
    proc_tiles_bands_times <- proc_tiles_bands_times[bad_timeline]

    # return all tiles from the original cube
    # that have not been processed or regularized correctly
    miss_tiles_bands_times <-
        unique(c(miss_tiles_bands_times, proc_tiles_bands_times))

    return(miss_tiles_bands_times)
}
