#' @title Build a regular data cube from an irregular one
#'
#' @name sits_regularize
#'
#' @description Creates cubes with regular time intervals
#'  using the gdalcubes package. Cubes can be composed using "median" or
#'  "least_cc_first" functions. Users need to provide an time
#'  interval which is used by the composition function.
#'
#' @references APPEL, Marius; PEBESMA, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092.
#'
#' @examples{
#' \dontrun{
#'
#' # --- Access to the AWS STAC
#'
#' # define an AWS data cube
#'   s2_cube <- sits_cube(source = "AWS",
#'                       collection = "sentinel-s2-l2a-cogs",
#'                       bands = c("B08", "SCL"),
#'                       tiles = c("20LKP"),
#'                       start_date = as.Date("2018-07-18"),
#'                       end_date = as.Date("2018-08-18")
#'   )
#'
#' # create a directory to store the resulting images
#' dir.create(paste0(tempdir(),"/images/"))
#'
#' # Build a data cube of equal intervals using the "gdalcubes" package
#' gc_cube <- sits_regularize(cube       = s2_cube,
#'                            output_dir = paste0(tempdir(),"/images/"),
#'                            period     = "P1M",
#'                            res        = 320)
#' }
#' }
#'
#' @param cube         A \code{sits_cube} object whose spacing of observation
#'  times is not constant and will be regularized by the \code{gdalcubes}
#'  package.
#'
#' @param output_dir   A \code{character} with a valid directory where the
#'  regularized images will be written by \code{gdalcubes}.
#'
#' @param period       A \code{character} with ISO8601 time period for regular
#'  data cubes produced by \code{gdalcubes}, with number and unit, e.g., "P16D"
#'  for 16 days. Use "D", "M" and "Y" for days, month and year.
#'
#' @param res          A \code{numeric} with spatial resolution of the image
#'  that will be aggregated.
#'
#' @param roi          A named \code{numeric} vector with a region of interest.
#'  See more above
#'
#' @param multicores   A \code{numeric} with the number of cores used for
#'  regularization. This parameter specifies how many bands from different tiles
#'  should be processed in parallel. By default, 1 core is used.
#'
#' @param multithreads A \code{numeric} value that specifies the number of
#'  threads used in the gdalcubes package. This parameter determines how many
#'  chunks are executed in parallel. The gdalcubes package divides data cubes
#'  into smaller chunks, where the generated chunk creates a 3-dimensional array
#'  of band, latitude, and longitude information. By default 2 threads are used.
#'
#' @param agg_method   A \code{character} with method that will be applied by
#'  \code{gdalcubes} for aggregation. Options: \code{median} and
#'  \code{least_cc_first}. The default aggregation method is
#'  \code{least_cc_first}. See more above.
#'
#' @param resampling   A \code{character} with method to be used by
#'  \code{gdalcubes} for resampling in mosaic operation.
#'  Options: \code{near}, \code{bilinear}, \code{bicubic} or others supported by
#'  gdalwarp (see https://gdal.org/programs/gdalwarp.html).
#'  Default is bilinear.
#'
#' @param cloud_mask   A \code{logical} to use cloud band for aggregation by
#'  \code{gdalcubes}. Deprecated as of SITS version 0.16.0. Default
#'  is \code{FALSE}.
#'
#' @note
#'    If malformed images with the same required tiles and bands are found in
#'    the current directory, these images are deleted and recreated.
#'
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lat_min", "lat_max", "long_min", "long_max")
#'
#' @note
#'    The "least_cc_first" aggregation method sorts the images based on cloud
#'    cover, where images with the fewest clouds at the top of the stack. Once
#'    the stack of images is sorted, the method uses the first valid value to
#'    create the temporal aggregation.
#'
#' @note
#'    If the supplied data cube contains cloud band, the values indicated as
#'    clouds or cloud shadow will be removed.
#'
#' @return A \code{sits_cube} object with aggregated images.
#'
#' @export
sits_regularize <- function(cube,
                            output_dir,
                            period,
                            res,
                            roi = NULL,
                            agg_method = "least_cc_first",
                            resampling = "bilinear",
                            cloud_mask = FALSE,
                            multicores = 1,
                            multithreads = 2) {

    # set caller to show in errors
    .check_set_caller("sits_regularize")

    # require gdalcubes package
    if (!requireNamespace("gdalcubes", quietly = TRUE))
        stop("Please install package gdalcubes", call. = FALSE)

    # collections
    .check_null(.source_collection_gdalcubes_support(.cube_source(cube),
                                                     .cube_collection(cube)),
                msg = "sits_regularize not available for collection ",
                cube$collection, " from ", cube$source
    )

    # precondition - test if provided object is a raster cube
    .check_that(
        x = inherits(cube, "raster_cube"),
        msg = paste("provided cube is invalid,",
                    "please provide a 'raster_cube' object.",
                    "see '?sits_cube' for more information.")
    )

    # precondition - check output dir fix
    output_dir <- normalizePath(output_dir)

    # verifies the path to save the images
    .check_that(
        x = dir.exists(output_dir),
        msg = "invalid 'output_dir' parameter."
    )

    # append gdalcubes path
    path_db <- paste0(output_dir, "/gdalcubes.db")

    # precondition - is the period valid?
    duration <- lubridate::duration(period)
    .check_na(duration, msg = "invalid period specified")

    # precondition - is the resolution valid?
    .check_num(x = res,
               allow_zero = FALSE,
               min = 1,
               len_min = 1,
               len_max = 1,
               msg = "a valid resolution needs to be provided"
    )

    # precondition - is the aggregation valid?
    agg_methods <- .config_get("gdalcubes_aggreg_methods")
    .check_chr_within(
        x = agg_method,
        within = names(agg_methods),
        discriminator = "any_of",
        msg = "invalid aggregation method"
    )

    # get the valid name for gdalcubes aggregation method
    agg_method <- agg_methods[[agg_method]]

    # precondition - is the resampling valid?
    .check_chr_within(
        x = resampling,
        within = .config_get("gdalcubes_resampling_methods"),
        discriminator = "any_of",
        msg = "invalid resampling method"
    )

    # as of SITS 0.16.0, parameter "cloud_mask" is deprecated
    if (!missing(cloud_mask))
        warning("cloud_mask parameter is deprecated and no longer required")

    # is there a cloud band?
    cloud_mask <- FALSE
    if ("CLOUD" %in% sits_bands(cube))
        cloud_mask <- TRUE

    # precondition - is the multithreads valid?
    .check_num(
        x = multithreads,
        min = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'multithreads' parameter."
    )

    # precondition - is the multicores valid?
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'multicores' parameter."
    )

    if (!is.null(roi)) {

        # filter only intersecting tiles
        intersects <- slider::slide_lgl(
            cube, .sits_raster_sub_image_intersects, roi
        )

        # retrieve only intersecting tiles
        cube <- cube[intersects, ]
    }

    # timeline of intersection
    timeline <- .gc_get_valid_timeline(cube, period = period)
    toi <- c(timeline[[1]], timeline[[length(timeline)]])

    # matches the start dates of different tiles
    cube[["file_info"]] <- purrr::map(cube[["file_info"]], function(file_info) {
        idx <- which(file_info[["date"]] == min(file_info[["date"]]))
        file_info[["date"]][idx] <- toi[[1]]
        file_info
    })

    # least_cc_first requires images ordered based on cloud cover
    cube <- .gc_arrange_images(
        cube = cube,
        agg_method = agg_method,
        duration = duration
    )

    # create an image collection
    img_col <- .gc_create_database_stac(cube = cube, path_db = path_db)

    # get all cube bands
    bands <- .cube_bands(cube = cube, add_cloud = FALSE)

    # does a local cube exist
    gc_cube <- tryCatch({
        sits_cube(
            source = .cube_source(cube),
            collection = .cube_collection(cube),
            data_dir = output_dir,
            parse_info = c("x1", "tile", "band", "date")
        )
    },
    error = function(e){
        return(NULL)
    })

    # find the tiles that have not been processed yet
    missing_tiles <- .reg_missing_tiles(cube, gc_cube, timeline)

    # combination of tiles and bands
    tiles_bands <- purrr::cross2(missing_tiles, bands)

    # start process
    multicores <- min(multicores, length(tiles_bands))
    .sits_parallel_start(multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # recovery mode
    finished <- length(missing_tiles) == 0

    while (!finished) {
        # process bands and tiles in parallel
        tiles_bands <- purrr::cross2(missing_tiles, bands)

        .sits_parallel_map(tiles_bands, function(tile_band) {

            tile <- tile_band[[1]]
            band <- tile_band[[2]]
            cube <- dplyr::filter(cube, tile == !!tile)

            if (.source_cloud() %in% .cube_bands(cube))
                band <- c(band, .source_cloud())

            cube <- sits_select(data = cube, bands = band)

            # open db in each process
            img_col <- gdalcubes::image_collection(path_db)

            # create a list of cube view object
            cv <- .gc_create_cube_view(
                tile = cube,
                period = period,
                roi = roi,
                res = res,
                toi = toi,
                agg_method = agg_method,
                resampling = resampling
            )

            # create of the aggregate cubes
            gc_tile <- .gc_new_cube(
                tile = cube,
                cv = cv,
                img_col = img_col,
                path_db = path_db,
                output_dir = output_dir,
                cloud_mask = cloud_mask,
                multithreads = multithreads
            )

            # prepare class result
            class(gc_tile) <- .cube_s3class(gc_tile)

            return(gc_tile)

        }, progress = multicores > 1)

        # create local cube from files in output directory
        gc_cube <- sits_cube(
            source = .cube_source(cube),
            collection = .cube_collection(cube),
            data_dir = output_dir,
            parse_info = c("x1", "tile", "band", "date")
        )

        # find if there are missing tiles
        missing_tiles <- .reg_missing_tiles(cube, gc_cube, timeline)

        # have we finished?
        finished <- length(missing_tiles) == 0

        # inform the user
        if (!finished)
            message(paste("Tiles", paste0(missing_tiles, collapse = ", "),
                          "have errors and will be reprocessed."))
    }

    # post-condition
    if (!.cube_is_regular(gc_cube))
        warning(paste0("please, run sits_regularize() again",
                       "generated cube is not regular"))

    return(gc_cube)
}

#' @title Finds the missing tiles in a regularized cube
#'
#' @name .reg_missing_tiles
#' @keywords internal
#'
#' @param   cube        original cube to be regularized
#' @param   gc_cube     regularized cube (may be missing tile)
#' @param   timeline    timeline used by gdalcube for regularized cube
#'
#' @return              tiles that are missing from the regularized cube
.reg_missing_tiles <- function(cube, gc_cube = NULL, timeline) {

    # if regularized cube does not exist, return all tiles from original cube
    if (purrr::is_null(gc_cube))
        return(cube[["tile"]])

    # first, include tiles that have not been processed
    missing_tiles <- setdiff(cube[["tile"]], gc_cube[["tile"]])

    # these are the tiles that have been processed
    proc_tiles <- gc_cube[["tile"]]

    # original bands in the non-regularized cube
    orig_bands <- .cube_bands(cube, add_cloud = FALSE)

    # do all tiles in gc_cube have the same bands as the original cube?
    tiles_miss_bands <- slider::slide_lgl(gc_cube, function(tile){
        tl_bands <- sits_bands(tile)
        return(!(all(orig_bands %in% tl_bands)))
    })
    # do all tiles in gc_cube have the same timeline as the original cube?
    tiles_miss_time <- slider::slide_lgl(gc_cube, function(tile){
        bands_miss_time <- purrr::map_lgl(.cube_bands(tile), function(band) {
            tile_band <- sits_select(tile, bands = band)
            return(!(all(timeline %in% sits_timeline(tile_band))))
        })
        return(any(bands_miss_time))
    })

    # return all tiles from the original cube
    # that have not been regularized correctly
    missing_tiles <- unique(c(missing_tiles,
                              proc_tiles[tiles_miss_bands],
                              proc_tiles[tiles_miss_time]))

    # find the malformed tiles
    bad_tiles <- unique(c(proc_tiles[tiles_miss_bands], proc_tiles[tiles_miss_time]))
    if (length(bad_tiles) > 0 ) {
        # clean all files from bad tiles
        gc_cube_bad_tiles <- dplyr::filter(gc_cube, tile %in% bad_tiles)
        bad_files <- dplyr::bind_rows(gc_cube_bad_tiles[["file_info"]])[["path"]]
        unlink(bad_files)
    }
    return(missing_tiles)
}
