#' @title Build a regular data cube from an irregular one
#'
#' @name sits_regularize
#'
#' @description Creates cubes with regular time intervals
#'  using the gdalcubes package. Cubes can be composed using "min", "max",
#'  "mean", "median" or "first" functions. Users need to provide an time
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
#'                            agg_method = "median",
#'                            resampling = "bilinear",
#'                            res        = 60,
#'                            cloud_mask = TRUE)
#' }
#' }
#'
#' @param cube       A \code{sits_cube} object whose spacing of observation
#'  times is not constant and will be regularized by the \code{gdalcubes}
#'  package.
#' @param output_dir A \code{character} with a valid directory where the
#'  regularized images will be written by \code{gdalcubes}.
#' @param period     A \code{character} with ISO8601 time period for regular
#'  data cubes produced by \code{gdalcubes}, with number and unit, e.g., "P16D"
#'  for 16 days. Use "D", "M" and "Y" for days, month and year.
#' @param res        A \code{numeric} with spatial resolution of the image that
#'  will be aggregated.
#' @param roi        A named \code{numeric} vector with a region of interest.
#'  See above
#' @param multicores A \code{numeric} with the number of cores will be used in
#'  the regularize. By default is used 1 core.
#' @param agg_method A \code{character} with method that will be applied by
#'  \code{gdalcubes} for aggregation. Options: \code{min}, \code{max},
#'  \code{mean}, \code{median} and \code{first}. Default is \code{median}.
#' @param resampling A \code{character} with method to be used by
#'  \code{gdalcubes} for resampling in mosaic operation.
#'  Options: \code{near}, \code{bilinear}, \code{bicubic} or others supported by
#'  gdalwarp (see https://gdal.org/programs/gdalwarp.html).
#'  By default is bilinear.
#' @param cloud_mask A \code{logical} to use cloud band for aggregation by
#' \code{gdalcubes}. Default is \code{TRUE}.
#'
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lat_min", "lat_max", "long_min", "long_max")
#'
#' @return A \code{sits_cube} object with aggregated images.
#'
#' @export
sits_regularize <- function(cube,
                            output_dir,
                            period,
                            res,
                            roi = NULL,
                            agg_method = "median",
                            resampling = "bilinear",
                            cloud_mask = FALSE,
                            multicores = 2,
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
    .check_na(lubridate::duration(period), msg = "invalid period specified")

    # precondition - is the resolution valid?
    .check_num(x = res,
               allow_zero = FALSE,
               min = 1,
               len_min = 1,
               len_max = 1,
               msg = "a valid resolution needs to be provided")

    # precondition - is the aggregation valid?
    .check_chr_within(
        x = agg_method,
        within = .config_get("gdalcubes_aggreg_methods"),
        discriminator = "any_of",
        msg = "invalid aggregation method"
    )

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

    if (!is.null(roi)) {

        # filter only intersecting tiles
        intersects <- slider::slide_lgl(cube,
                                        .sits_raster_sub_image_intersects,
                                        roi)

        # retrieve only intersecting tiles
        cube <- cube[intersects, ]
    }

    # timeline of intersection
    tl <- .gc_get_valid_timeline(cube, period = period)
    toi <- c(tl[[1]], tl[[length(tl)]])

    # matches the start dates of different tiles
    cube[["file_info"]] <- purrr::map(cube[["file_info"]], function(file_info) {
        idx <- which(file_info[["date"]] == min(file_info[["date"]]))
        file_info[["date"]][idx] <- toi[[1]]
        file_info
    })

    # create an image collection
    img_col <- .gc_create_database(cube = cube, path_db = path_db)

    # get all cube bands
    bands <- .cube_bands(cube, add_cloud = FALSE)

    # start process
    multicores <- min(multicores, length(bands))
    .sits_parallel_start(multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # process bands in parallel
    gc_cube_lst <- .sits_parallel_map(bands, function(band) {

        # open db in each process
        img_col <- gdalcubes::image_collection(path_db)

        # filter cube band
        process_bands <- band
        if (.source_cloud() %in% .cube_bands(cube))
            process_bands <- c(process_bands, .source_cloud())
        cube <- sits_select(cube, bands = process_bands)


        # resume feature:
        # filter remaining tiles of current band
        fields <- do.call(
            rbind,
            strsplit(list.files(output_dir, pattern = "\\.tif$"),
                     split = "_"))

        # initialize processed cubes
        processed_cube <- NULL

        if (!is.null(fields)) {

            # canonical output name from sits_regularize()
            colnames(fields) <- c("cube", "tile", "band", "date")

            # get all remaining tile for this band
            processed_tiles <-
                dplyr::select(tibble::as_tibble(fields),
                              dplyr::all_of(c("tile", "band"))) %>%
                dplyr::distinct() %>%
                dplyr::filter(band == !!band) %>%
                dplyr::pull("tile")

            if (length(processed_tiles) > 0) {

                # get processed files for the current band of processed tiles
                processed_cube <-
                    sits_cube(source = .cube_source(cube),
                              collection = .cube_collection(cube),
                              data_dir = output_dir,
                              bands = band,
                              parse_info = c("X1", "tile", "band", "date"))
            }

            # filter remaining tiles
            cube <- dplyr::filter(cube, !tile %in% !!processed_tiles)
        }

        # slide cube to process each remaining tile
        gc_cube <- slider::slide_dfr(cube, function(tile) {

            # create a list of cube view object
            cv <- .gc_create_cube_view(tile = tile,
                                       period = period,
                                       roi = roi,
                                       res = res,
                                       toi = toi,
                                       agg_method = agg_method,
                                       resampling = resampling)

            # create of the aggregate cubes
            gc_tile <- .gc_new_cube(tile = tile,
                                    cv = cv,
                                    img_col = img_col,
                                    path_db = path_db,
                                    output_dir = output_dir,
                                    cloud_mask = cloud_mask,
                                    multithreads = multithreads)

            return(gc_tile)
        })

        # bind results
        gc_cube <- dplyr::bind_rows(processed_cube, gc_cube)

        # prepare class result
        class(gc_cube) <- .cube_s3class(gc_cube)

        return(gc_cube)

    }, progress = multicores > 1)

    # merge bands
    gc_cube <- Reduce(f = sits_merge, x = gc_cube_lst[-1],
                      init = gc_cube_lst[[1]])

    return(gc_cube)
}

