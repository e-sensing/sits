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
#' \donttest{
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
#'  #' # Build a data cube of equal intervals using the "gdalcubes" package
#' gc_cube <- sits_regularize(cube   = s2_cube,
#'                      name          = "T20LKP_2018_2019_1M",
#'                      path_db       = paste0(tempdir(),"/cube.db"),
#'                      path_images   = paste0(tempdir(),"/images/"),
#'                      period        = "P1M",
#'                      agg_method    = "median",
#'                      resampling    = "bilinear",
#'                      cloud_mask    = TRUE)
#' }
#'
#' @param cube              A cube whose spacing of observation times is not constant
#'                          and will be regularized by the "gdalcubes" packges
#' @param name              Name of the output data cube
#' @param path_images       Directory where the regularized images will be
#'                          written by \code{gdalcubes}.
#' @param path_db           Path and name where the \code{gdalcubes}
#'                          database will be create. E.g. "my/path/gdalcubes.db"
#' @param period            ISO8601 time period for regular data cubes
#'                          produced by \code{gdalcubes},
#'                          with number and unit, e.g., "P16D" for 16 days.
#'                          Use "D", "M" and "Y" for days, month and year..
#' @param agg_method        Method that will be applied by \code{gdalcubes}
#'                          for aggregation. Options: "min", "max", "mean",
#'                          "median" and "first".
#' @param resampling        Method to be used by \code{gdalcubes}
#'                          for resampling in mosaic operation.
#'                          Options: "near", "bilinear", "bicubic"
#'                          or others supported by gdalwarp
#'                          (see https://gdal.org/programs/gdalwarp.html).
#' @param cloud_mask        Use cloud band for aggregation by \code{gdalcubes}? (TRUE/FALSE)
#' @export
sits_regularize <- function(cube,
                            name,
                            path_images,
                            path_db = NULL,
                            period  = NULL,
                            agg_method = NULL,
                            resampling = "bilinear",
                            cloud_mask = TRUE) {
    # require gdalcubes package
    if (!requireNamespace("gdalcubes", quietly = TRUE)) {
        stop(paste("Please install package gdalcubes from CRAN:",
                   "install.packages('gdalcubes')"), call. = FALSE
        )
    }

    # test if provided object its a sits cube
    assertthat::assert_that("raster_cube" %in% class(cube),
                            msg = paste("The provided cube is invalid,",
                                        "please provide a 'raster_cube' object.",
                                        "See '?sits_cube' for more information.")
    )

    # in case of null path a temporary directory is generated
    if (is.null(path_db))
        path_db <- file.path(tempdir(), "cube.db")

    # create an image collection
    img_col <- .sits_gc_database(cube, path_db)

    # create a list of cube view object
    cv_list <- .sits_gc_cube(cube, period, agg_method, resampling)

    # create of the aggregate cubes
    gc_cube <- .sits_gc_compose(cube, name, cv_list, img_col,
                                path_db, path_images, cloud_mask)

    class(gc_cube) <- c("raster_cube", class(gc_cube))

    return(gc_cube)
}
