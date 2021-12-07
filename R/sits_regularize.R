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
#' @references Ferreira, Karine R., et al. "Earth observation data cubes for
#' Brazil: Requirements, methodology and products." Remote Sensing 12.24 (2020):
#'  4033. DOI: 10.3390/rs12244033.
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
#'                            res        = 60)
#' }
#' }
#'
#' @param cube        A \code{sits_cube} object whose spacing of observation
#'  times is not constant and will be regularized by the \code{gdalcubes}
#'  package.
#' @param output_dir  A \code{character} with a valid directory where the
#'  regularized images will be written by \code{gdalcubes}.
#' @param period      A \code{character} with ISO8601 time period for regular
#'  data cubes produced by \code{gdalcubes}, with number and unit, e.g., "P16D"
#'  for 16 days. Use "D", "M" and "Y" for days, month and year.
#' @param res         A \code{numeric} with spatial resolution of the image that
#'  will be aggregated.
#' @param roi         A named \code{numeric} vector with a region of interest.
#'  See above
#' @param multicores  A \code{numeric} with the number of cores will be used in
#'  the regularize. By default is used 1 core.
#' @param agg_method  A \code{character} with method that will be applied by
#'  \code{gdalcubes} for aggregation. Options: \code{median} and
#'  \code{least_cc_first}.
#'  The default aggregation method is \code{least_cc_first}. See more above.
#' @param fill_method A \code{character} indicating which interpolation method
#'  will be applied. Options: \code{near} for nearest neighbor; \code{linear}
#'  for linear interpolation; \code{locf} for ast observation carried forward,
#'  or \code{nocb} for next observation carried backward.
#'  Default is \code{near}.
#' @param resampling  A \code{character} with method to be used by
#'  \code{gdalcubes} for resampling in mosaic operation.
#'  Options: \code{near}, \code{bilinear}, \code{bicubic} or others supported by
#'  gdalwarp (see https://gdal.org/programs/gdalwarp.html).
#'  By default is bilinear.
#'
#' @note
#'    The "roi" parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values ("xmin", "xmax", "ymin", "ymax") or
#'    named lat/long values ("lat_min", "lat_max", "long_min", "long_max")
#'
#' @note
#'    The \code{least_cc_first} aggregation method sorts the images based on
#'    cloud coverage, images with the least clouds are at the top of the stack.
#'    Once the stack of images is sorted the method uses the first valid value
#'    to generate the temporal aggregation.
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
                            fill_method = "near",
                            resampling = "bilinear",
                            multicores = 2) {

    # set caller to show in errors
    .check_set_caller("sits_regularize")

    # require gdalcubes package
    if (!requireNamespace("gdalcubes", quietly = TRUE))
        stop("Please install package gdalcubes", call. = FALSE)

    # precondition - test if provided object is a raster cube
    .check_that(
        x = inherits(cube, "raster_cube"),
        msg = paste("provided cube is invalid,",
                    "please provide a 'raster_cube' object.",
                    "see '?sits_cube' for more information.")
    )

    # precondition - check if this cube be regularized
    .source_collection_gdalcubes_support(
        source = .cube_source(cube), collection = .cube_collection(cube)
    )

    # in order for the path to be accepted on different operating systems
    output_dir <- normalizePath(output_dir)

    # precondition - is the path valid?
    .check_that(
        x = dir.exists(output_dir),
        msg = "invalid 'output_dir' parameter."
    )

    # append gdalcubes path
    path_db <- paste0(output_dir, "/gdalcubes.db")

    # precondition - is the period valid?
    duration <- lubridate::duration(period)
    .check_na(duration, msg = "Invalid period. Please see ISO 8601 format.")

    # precondition - is the resolution valid?
    .check_num(x = res,
               allow_zero = FALSE,
               min = 1,
               len_min = 1,
               len_max = 1,
               msg = "a valid resolution needs to be provided")

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

    # precondition - is the filling valid?
    .check_chr_within(
        x = fill_method,
        within = .config_get("gdalcubes_filling_methods"),
        discriminator = "any_of",
        msg = "invalid filling methods"
    )

    # precondition - is the resampling valid?
    .check_chr_within(
        x = resampling,
        within = .config_get("gdalcubes_resampling_methods"),
        discriminator = "any_of",
        msg = "invalid resampling method"
    )

    # is there a cloud band?
    cloud_mask <- FALSE
    if ("CLOUD" %in% sits_bands(cube))
        cloud_mask <- TRUE

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
        intersects <- slider::slide_lgl(cube,
                                        .sits_raster_sub_image_intersects,
                                        roi)

        # retrieve only intersecting tiles
        cube <- cube[intersects, ]
    }

    # get the interval of intersection in all tiles
    .get_valid_interval <- function(cube) {

        # start date - maximum of all minimums
        max_min_date <- do.call(
            what = max,
            args = purrr::map(cube$file_info, function(file_info){
                return(min(file_info$date))
            })
        )

        # end date - minimum of all maximums
        min_max_date <- do.call(
            what = min,
            args = purrr::map(cube$file_info, function(file_info){
                return(max(file_info$date))
            }))

        # check if all timeline of tiles intersects
        .check_that(
            x = max_min_date < min_max_date,
            msg = "the timeline of the cube tiles do not intersect."
        )

        # finds the length of the timeline
        tl_length <- max(2, ceiling(
            lubridate::interval(start = max_min_date,
                                end = min_max_date) / duration
            )
        )

        # timeline dates
        tl <- duration * (seq_len(tl_length) - 1) + as.Date(max_min_date)

        # the count starts from the second valid day
        # it is necessary to return one day
        if (tl[tl_length] >= min_max_date)
            tl[tl_length] <- tl[tl_length] - 1

        # timeline cube
        tiles_tl <- suppressWarnings(sits_timeline(cube))

        if (!is.list(tiles_tl))
            tiles_tl <- list(tiles_tl)

        # checks if the timelines intersect
        tl_check <- vapply(tiles_tl, function(tile_tl) {

            # at least one image must be in begin and end in timeline interval
            begin <- any(tile_tl >= tl[1] & tile_tl < tl[2])
            end <- any(tile_tl >= tl[tl_length - 1] & tile_tl < tl[tl_length])

            return(begin && end)
        }, logical(1))

        .check_that(x = all(tl_check), msg = "invalid images interval")

        return(list(max_min_date = tl[1], min_max_date = tl[length(tl)]))
    }

    # adds the bbox for each image in the file_info
    .add_bbox_fileinfo <- function(cube) {

        # number of requests in parallel
        n_workers <- .config_gdalcubes_open_connections()

        # progress bar
        progress <- TRUE

        # adds crs to the file_info that is used in the bbox transformation
        cube$file_info <- lapply(seq_along(cube$file_info), function(i) {
            cube$file_info[[i]] <- dplyr::mutate(cube$file_info[[i]],
                                                 crs = cube[i, ]$crs)

            cube$file_info[[i]]
        })

        if (sum(lengths(cube)) < .config_gdalcubes_min_files_for_parallel()) {
            n_workers <- 1
            progress <- FALSE
        }

        cube <- .sits_fast_apply(cube, col = "file_info", fn = function(x) {

            # prepare parallelization
            .sits_parallel_start(workers = 1, log = FALSE)
            on.exit(.sits_parallel_stop(), add = TRUE)

            x$bbox <- .sits_parallel_map(seq_len(nrow(x)), function(i) {

                r_obj <- tryCatch({
                    .raster_open_rast(x$path[[i]])
                }, error = function(e) {
                    return(NULL)
                })

                if (is.null(r_obj))
                    return(NULL)

                bbox <- .raster_extent(r_obj)

                bbox <- c(
                    .sits_proj_to_latlong(x = bbox[["xmin"]],
                                          y = bbox[["ymin"]],
                                          crs = x$crs[[i]]),
                    .sits_proj_to_latlong(x = bbox[["xmax"]],
                                          y = bbox[["ymax"]],
                                          crs = x$crs[[i]])
                )

                names(bbox) <- c("left", "bottom", "right", "top")
                tibble::as_tibble(lapply(bbox, identity))
            }, progress = progress, n_retries = 0)

            x
        })

        cube$file_info <- lapply(cube$file_info, function(fi) {

            # removing invalid bbox
            dplyr::group_by(fi, date) %>%
                dplyr::mutate(valid_image = all(
                    vapply("bbox", Negate(is.null), logical(1)))) %>%
                dplyr::filter(valid_image) %>%
                dplyr::ungroup() %>%
                dplyr::select(-"valid_image") %>%
                tidyr::unnest(cols = "bbox")
        })

        cube
    }

    cube <- .add_bbox_fileinfo(cube)

    # timeline of intersection
    toi <- .get_valid_interval(cube)

    # matches the start dates of different tiles
    cube$file_info <- purrr::map(cube$file_info, function(file_info) {
        idx <- which(file_info$date == min(file_info$date))
        file_info$date[idx] <- toi[[1]]
        file_info
    })

    # least_cc_first requires images ordered based on cloud cover
    cube <- .gc_arrange_images(cube, agg_method, duration)

    # create an image collection using stac
    img_col <- .gc_create_database(cube = cube, path_db = path_db)

    gc_cube <- slider::slide_dfr(cube, function(tile){

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
                                fill_method = fill_method,
                                img_col = img_col,
                                path_db = path_db,
                                output_dir = output_dir,
                                cloud_mask = cloud_mask,
                                multicores = multicores)
        return(gc_tile)

    })

    # reset global option
    gdalcubes::gdalcubes_options(threads = 1)

    # the database is not used to generate new cubes
    unlink(path_db)

    class(gc_cube) <- .cube_s3class(gc_cube)

    return(gc_cube)
}
