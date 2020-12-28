#' @title Create a list of a gdal_cubes raster object.
#' @name sits_gdalcubes_raster
#'
#' @references `gdalcubes` package (https://github.com/appelmar/gdalcubes_R)
#'
#' @description Creates a list of cubes based on the object provided by the
#'  package 'gdalcubes' to be later aggregated or applied any operations.
#'
#' @param cube       Data cube from where data is to be retrieved.
#' @param path_db    a \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#' @param period     a \code{character} with the The period of time in which it
#'  is desired to apply in the cube, must be provided based on ISO8601, where 1
#'  number and a unit are provided, for example "P16D".
#' @param method     a \code{character} with the method that will be applied in
#'  the aggregation, the following are available: "min", "max", "mean",
#'  "median" or "first".
#' @param resampling a \code{character} with the method that will be applied
#'  in the resampling in mosaic operation. The following are available: "near",
#'  "bilinear", "bicubic" or others supported by gdalwarp
#'  (see https://gdal.org/programs/gdalwarp.html).
#'
#' @param ...        Aditional parameters that can be included. See
#'  '?gdalcubes::cube_view'.
#'
#' @return a \code{gdalcubes_list} object where each index has a 'raster_cube'
#'  for each tile.
#'
#' @examples
#' \dontrun{
#' # this example requires access to an external service, so should not be run
#' # by CRAN
#'
#' # s3://sentinel-cogs/sentinel-s2-l2a-cogs/2017/S2A_35MNR_20171025_0_L2A/
#'
#' # Provide your AWS credentials here
#' # Sys.setenv(
#' # "AWS_ACCESS_KEY_ID"     = <your_access_key>,
#' # "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,
#' # "AWS_DEFAULT_REGION"    = <your AWS region>,
#' # "AWS_ENDPOINT" = "sentinel-s2-l2a.s3.amazonaws.com",
#' # "AWS_REQUEST_PAYER"     = "requester"
#' # )
#'
#' s2_cube <- sits_cube(
#'     type = "S2_L2A_AWS",
#'     name = "T20LKP_2018_2019",
#'     satellite = "SENTINEL-2",
#'     sensor = "MSI",
#'     tiles = "20LKP",
#'     s2_aws_resolution = "20m",
#'     start_date = as.Date("2018-07-18"),
#'     end_date = as.Date("2018-09-18")
#' )
#'
#' r_cube <- sits_gdalcubes_raster(s2_cube, path_db = "my/path/cube.db",
#'                                 period     = "P1M",
#'                                 method     = "median",
#'                                 resampling = "bilinear")
#'
#' }
#'
#' @export
sits_gdalcubes_raster <- function(cube, path_db,
                                  period     = NULL,
                                  method     = NULL,
                                  resampling = "bilinear", ...) {

    # require gdalcubes package
    if (!requireNamespace("gdalcubes", quietly = TRUE)) {
        stop(paste("Please install package gdalcubes from CRAN:",
                   "install.packages('gdalcubes')"), call. = FALSE
        )
    }

    # test if provided object if a stack_cube
    assertthat::assert_that("stack_cube" %in% class(cube),
                            msg = paste("The provided object is invalid,",
                                        "please provide a 'stack_cube' object.")
    )

    # create an image collection
    img_col <- .sits_gdalcubes_image_collection(cube, path_db)

    # create a cube view
    cube_view <- .sits_gdalcubes_cube_view(cube,
                                           period,
                                           method,
                                           resampling, ...)
    # create a list of raster cube
    rc_list <- purrr::map(cube_view, function(cv) {
        gdalcubes::raster_cube(img_col, cv)
    })

    # defines the object class
    class(rc_list) <- c("gdalcubes_list", class(rc_list))

    return(rc_list)
}
#' @title Save the images based on an aggregation method.
#' @name sits_gdalcubes_aggregation
#'
#' @description Based on the defined parameters in 'sits_gdalcubes_raster'
#'  function, this function materializes the cubes created in the .tif format.
#'
#' @param gdalcubes_list a \code{gdalcubes_list} object returned by
#'  sits_gdalcubes_raster function.
#' @param path_images    a \code{character} with the path where the aggregated
#'  images will be writed.
#' @param ...            Additional parameters that can be included. See
#'  '?gdalcubes::write_tif'.
#'
#' @return  an invisible return of "gdalcubes_list" object.
#'
#' @examples
#' \dontrun{
#' # this example requires access to an external service, so should not be run
#' # by CRAN
#'
#' # s3://sentinel-cogs/sentinel-s2-l2a-cogs/2017/S2A_35MNR_20171025_0_L2A/
#'
#' # Provide your AWS credentials here
#' # Sys.setenv(
#' # "AWS_ACCESS_KEY_ID"     = <your_access_key>,
#' # "AWS_SECRET_ACCESS_KEY" = <your_secret_access_key>,
#' # "AWS_DEFAULT_REGION"    = <your AWS region>,
#' # "AWS_ENDPOINT" = "sentinel-s2-l2a.s3.amazonaws.com",
#' # "AWS_REQUEST_PAYER"     = "requester"
#' # )
#'
#' s2_cube <- sits_cube(
#'     type = "S2_L2A_AWS",
#'     name = "T20LKP_2018_2019",
#'     satellite = "SENTINEL-2",
#'     sensor = "MSI",
#'     tiles = "20LKP",
#'     s2_aws_resolution = "20m",
#'     start_date = as.Date("2018-07-18"),
#'     end_date = as.Date("2018-09-18")
#' )
#'
#' r_cube <- sits_gdalcubes_raster(s2_cube, path_db = "my/path/cube.db",
#'                                 period     = "P1M",
#'                                 method     = "median",
#'                                 resampling = "bilinear")
#'
#' sits_gdalcubes_aggregation(r_cube, "my/path/images/")
#' }
#'
#' @export
sits_gdalcubes_aggregation <- function(gdalcubes_list, path_images, ...) {

    # verifies the provided object
    assertthat::assert_that("gdalcubes_list" %in% class(gdalcubes_list),
                            msg = paste("The provided object must be a ",
                                        "'gdalcubes_list' object. Please see",
                                        "'sits_gdalcubes_raster' function.")
    )

    # verifies the path to save the images
    assertthat::assert_that(dir.exists(path_images),
                            msg = paste("The provided dir does not exist.",
                                        "Please provided a valid path.")

    )

    # write the aggregated cubes
    purrr::map(gdalcubes_list, gdalcubes::write_tif, path_images,
               write_json_descr = TRUE, ...)

    return(invisible(gdalcubes_list))
}
#' @title Create an image_collection object
#' @name .sits_gdalcubes_image_collection
#' @keywords internal
#'
#' @param cube      Data cube from where data is to be retrieved.
#' @param path_db   a \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#'
#' @return a \code{object} 'image_collection' containing information about the
#'  images metadata.
.sits_gdalcubes_image_collection <- function(cube, path_db) {

    # error if a cube other than S2_L2A_AWS is provided
    assertthat::assert_that(!(cube[1,]$type != "S2_L2A_AWS"),
                            msg = paste("For now, only 'S2_L2A_AWS' cubes",
                                        "can be aggregated.")
    )

    # joining the bands of all tiles
    full_images <- dplyr::bind_rows(cube$file_info)

    # retrieving the s2_la_aws format
    format_cube <- system.file("extdata/gdalcubes/s2la_aws.json",
                             package = "sits")

    # create image collection cube
    ic_cube <- gdalcubes::create_image_collection(files    = full_images$path,
                                                  format   = format_cube,
                                                  out_file = path_db)
    return(ic_cube)
}
#' @title Create a cube_view object
#' @name .sits_gdalcubes_cube_view
#' @keywords internal
#'
#' @param path_db    a \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#' @param period     a \code{character} with the The period of time in which it
#'  is desired to apply in the cube, must be provided based on ISO8601, where 1
#'  number and a unit are provided, for example "P16D".
#' @param method     a \code{character} with the method that will be applied in
#'  the aggregation, the following are available: "min", "max", "mean",
#'  "median" or "first".
#' @param resampling a \code{character} with the method that will be applied
#'  in the resampling in mosaic operation. The following are available: "near",
#'  "bilinear", "bicubic" or others supported by gdalwarp
#'  (see https://gdal.org/programs/gdalwarp.html).
#'
#' @param ...        Aditional parameters that can be included. See
#'  '?gdalcubes::cube_view'.
#'
#' @return a \code{list} with a cube_view objects.
.sits_gdalcubes_cube_view <- function(cube,
                                      period,
                                      method,
                                      resampling, ...) {

    assertthat::assert_that(!purrr::is_null(period),
                            msg = paste("sits_gdalcubes: for ",
                                        "sits_gdalcubes_raster 'period' must",
                                        "be provided"))

    assertthat::assert_that(!purrr::is_null(method),
                            msg = paste("sits_gdalcubes: for ",
                                        "sits_gdalcubes_raster 'method' must",
                                        "be provided"))

    # create a list of cube view
    cv_list <- slider::slide(cube, function(c_tile) {
        gdalcubes::cube_view(
            extent = list(left   = c_tile$xmin,
                          right  = c_tile$xmax,
                          bottom = c_tile$ymin,
                          top    = c_tile$ymax,
                          t0 = format(min(cube$file_info[[1]]$date), "%Y-%m"),
                          t1 = format(max(cube$file_info[[1]]$date), "%Y-%m")),
            srs = c_tile$crs[[1]],
            dt  = period,
            nx  = c_tile$ncols[[1]],
            ny  = c_tile$nrows[[1]],
            aggregation = method,
            resampling  = resampling,
            ...)})


    return(cv_list)
}
