#' @title Creates a regularized data cube from an irregular one
#' @name sits_regularize
#' @description Creates cubes with regular time intervals
#'  using the gdalcubes package. Cubes are composed using "min", "max", "mean",
#' "median" or "first" functions. Users need to provide an
#'  time interval which is used by the composition function.
#'
#' @references APPEL, Marius; PEBESMA, Edzer. On-demand processing of data cubes
#'  from satellite image collections with the gdalcubes library. Data, v. 4,
#'  n. 3, p. 92, 2019. DOI: 10.3390/data4030092.
#'
#' @examples{
#' \dontrun{
#'
#' # --- Access to the AWS STAC
#' # Provide your AWS credentials as environment variables
#' Sys.setenv(
#'     "AWS_ACCESS_KEY_ID" = <your_aws_access_key>,
#'     "AWS_SECRET_ACCESS_KEY" = <your_aws_secret_access_key>
#' )
#'
#' # define an AWS data cube
#'   s2_cube <- sits_cube(source = "AWS",
#'                       name = "T20LKP_2018_2019",
#'                       collection = "sentinel-s2-l2a",
#'                       bands = c("B08", "SCL"),
#'                       tiles = c("20LKP"),
#'                       start_date = as.Date("2018-07-18"),
#'                       end_date = as.Date("2018-08-18"),
#'                       s2_resolution = 60
#' )
#'
#' # create a directory to store the resulting images
#' dir.create(paste0(tempdir(),"/images/"))
#'
#'  # Build a data cube of equal intervals using the "gdalcubes" package
#' gc_cube <- sits_regularize(cube   = s2_cube,
#'                      name          = "T20LKP_2018_2019_1M",
#'                      dir_images   = paste0(tempdir(),"/images/"),
#'                      period        = "P1M",
#'                      agg_method    = "median",
#'                      resampling    = "bilinear",
#'                      cloud_mask    = TRUE)
#' }
#' }
#'
#' @param cube       A \code{sits_cube} object whose spacing of observation
#'  times is not constant and will be regularized by the \code{gdalcubes}
#'  package.
#' @param output_dir A \code{character} with a directory where the regularized
#'  images will be written by \code{gdalcubes}.
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
#'  \code{gdalcubes} for aggregation.
#'  Options: \code{min}, \code{max}, \code{mean}, \code{median} and
#'  \code{first}.
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
                            period  = NULL,
                            res     = NULL,
                            roi     = NULL,
                            agg_method = "median",
                            resampling = "bilinear",
                            cloud_mask = TRUE,
                            multicores = 1) {

    # set caller to show in errors
    .check_set_caller("sits_regularize")

    # require gdalcubes package
    if (!requireNamespace("gdalcubes", quietly = TRUE))
        stop("Please install package gdalcubes", call. = FALSE)

    # collections
    .check_null(.source_collection_gdal_config(.cube_source(cube),
                                               .cube_collection(cube)),
                msg = "sits_regularize not available for collection ",
                cube$collection, " from ", cube$source
    )

    .check_num(
        x = multicores,
        allow_zero = FALSE,
        min = 1,
        msg = "invalid 'multicores' parameter."
    )

    # setting in global env multicores options
    gdalcubes::gdalcubes_options(threads = multicores)

    # test if provided object its a sits cube
    .check_that(
        x = inherits(cube, "raster_cube"),
        msg = paste("provided cube is invalid,",
                    "please provide a 'raster_cube' object.",
                    "see '?sits_cube' for more information.")
    )

    # fix slashes for windows
    output_dir <- normalizePath(output_dir)

    # verifies the path to save the images
    .check_that(
        x = dir.exists(output_dir),
        msg = "invalid 'output_dir' parameter."
    )

    path_db <- paste0(output_dir, "/gdalcubes.db")

    if (!is.null(roi)) {

        # filter only intersecting tiles
        intersects <- slider::slide_lgl(cube,
                                        .sits_raster_sub_image_intersects,
                                        roi)

        # retrieve only intersecting tiles
        cube <- cube[intersects, ]
    }

    # get the interval of intersection in all tiles
    interval_intersection <- function(cube) {

        max_min_date <- do.call(
            what = max,
            args = purrr::map(cube$file_info, function(file_info){
                return(min(file_info$date))
            })
        )

        min_max_date <- do.call(
            what = min,
            args = purrr::map(cube$file_info, function(file_info){
                return(max(file_info$date))
            }))

        # check if all tiles intersects
        .check_that(
            x = max_min_date < min_max_date,
            msg = "the cube tiles' timelines do not intersect."
        )

        list(max_min_date = max_min_date, min_max_date = min_max_date)
    }

    # timeline of intersection
    toi <- interval_intersection(cube)

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
                                cloud_mask = cloud_mask)
        return(gc_tile)

    })

    # reset global option
    gdalcubes::gdalcubes_options(threads = 1)

    class(gc_cube) <- .cube_s3class(gc_cube)

    return(gc_cube)
}
