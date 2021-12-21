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
#' @param cloud_mask A \code{logical} to use cloud band for aggregation by
#' \code{gdalcubes}. Deprecated as of SITS version 0.16.0.
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
                            cloud_mask = FALSE,
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

    # precondition - check if this cube could be regularized
    .source_collection_gdalcubes_support(
        source = .cube_source(cube), collection = .cube_collection(cube)
    )

    # ensuring the path is accepted in all OS
    output_dir <- normalizePath(output_dir)

    # precondition - is the path valid?
    .check_that(
        x = dir.exists(output_dir),
        msg = "invalid 'output_dir' parameter."
    )

    # append gdalcubes path
    path_db <- tempfile("database_", tmpdir = output_dir, fileext = ".db")

    # precondition - is the period valid?
    duration <- lubridate::duration(period)
    .check_na(duration, msg = "invalid period, please see ISO 8601 format")

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
    # as of SITS 0.16.0, parameter "cloud_mask" is deprecated
    if (!missing(cloud_mask))
        warning("cloud_mask parameter is deprecated and no longer required")

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
        msg = "invalid 'multicores' parameter"
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
    toi <- .gc_get_valid_interval(cube, period = period)

    # matches the start dates of different tiles
    cube[["file_info"]] <- purrr::map(cube[["file_info"]], function(file_info) {
        idx <- which(file_info[["date"]] == min(file_info[["date"]]))
        file_info[["date"]][idx] <- toi[[1]]
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
                                roi = roi,
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
