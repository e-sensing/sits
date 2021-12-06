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
                            cloud_mask = TRUE,
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
    .check_na(duration, msg = "invalid period. Please see ISO 8601 formats.")

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

    # precondition - is the cloud mask valid?
    .check_lgl_type(
        cloud_mask, msg = "cloud mask parameter should be TRUE/FALSE"
    )

    # if the cloud mask is true, is there a cloud band?
    if (cloud_mask) {
        .check_chr_contains(
            x = sits_bands(cube),
            contains = "CLOUD",
            msg = "no cloud band available in the cube"
        )
    }

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
        ))

        # timeline dates
        tl <- duration * (seq_len(tl_length) - 1) + as.Date(max_min_date)

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

        list(max_min_date = tl[1], min_max_date = tl[length(tl)])
    }

    # adds the bbox for each image in the file_info
    .add_bbox_fileinfo <- function(cube) {

        # number of requests in parallel
        n_workers <- .config_parallel_requests()

        # progress bar
        progress <- TRUE

        data <- cube

        # make sure that nesting operation (bellow) will be done correctly
        data[["..row_id"]] <- seq_len(nrow(data))

        # unnest bands
        data <- tidyr::unnest(data, cols = "file_info")

        if (nrow(data) < .config_parallel_minimum_requests()) {
            n_workers <- 1
            progress <- FALSE
        }

        # prepare parallelization
        .sits_parallel_start(workers = n_workers, log = FALSE)
        on.exit(.sits_parallel_stop(), add = TRUE)

        data$bbox <- .sits_parallel_map(seq_len(nrow(data)), function(i) {

            r_obj <- tryCatch({
                .raster_open_rast(data$path[[i]])
            }, error = function(e) {
                return(NULL)
            })

            if (is.null(r_obj))
                return(NULL)

            bbox <- .raster_extent(r_obj)

            bbox <- c(
                .sits_proj_to_latlong(x = bbox[["xmin"]],
                                      y = bbox[["ymin"]],
                                      crs = data$crs[[i]]),
                .sits_proj_to_latlong(x = bbox[["xmax"]],
                                      y = bbox[["ymax"]],
                                      crs = data$crs[[i]])
            )

            names(bbox) <- c("left", "bottom", "right", "top")
            tibble::as_tibble(lapply(bbox, identity))
        }, progress = progress, n_retries = 0)

        # nest again
        data <- tidyr::nest(data, file_info = c("date", "band", "res",
                                                "path", "bbox"))

        # remove ..row_id
        data <- dplyr::select(data, -"..row_id")

        data$file_info <- lapply(data$file_info, function(fi) {

            # removing invalid bbox
            dplyr::group_by(fi, date) %>%
                dplyr::mutate(valid_image = all(
                    vapply(bbox, Negate(is.null), logical(1)))) %>%
                dplyr::filter(valid_image) %>%
                dplyr::ungroup() %>%
                dplyr::select(-valid_image) %>%
                tidyr::unnest(cols = bbox)
        })

        # set sits tibble class
        class(data) <- class(cube)

        data
    }

    cube <- .add_bbox_fileinfo(cube)

    # timeline of intersection
    toi <- .get_valid_interval(cube = cube)

    cube$file_info <- purrr::map(cube$file_info, function(file_info) {
        idx <- which(file_info$date == min(file_info$date))
        file_info$date[idx] <- toi[[1]]
        file_info
    })

    # create an image collection
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
                                img_col = img_col,
                                path_db = path_db,
                                output_dir = output_dir,
                                cloud_mask = cloud_mask,
                                multicores = multicores)
        return(gc_tile)

    })

    # reset global option
    gdalcubes::gdalcubes_options(threads = 1)

    class(gc_cube) <- .cube_s3class(gc_cube)

    return(gc_cube)
}
