#' @title Post-process a classified data raster probs using bayesian smoothing
#'
#' @name  sits_smooth_bayes
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and apply optional bayesian smoothing process.
#'
#' @param  cube              Classified image data cube.
#' @param  window            A matrix with the neighborhood window
#'                           to compute bayesian smooth.
#'                           The central element index (i, j) is given by
#'                           i = floor(nrows(window)/2)+1 and
#'                           j = floor(ncols(window)/2)+1.
#'                           Elements '0' are excluded from window.
#' @param  variance          Estimated variance of logit of class_probs
#'                           (Bayesian smoothing parameter).
#' @param  output_dir        Output directory where to out the file
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#' @return A tibble with metadata about the output raster objects.
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select band "ndvi"
#'
#' samples_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # Classify a raster file with 23 instances for one year
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'     package = "sits"
#' ))
#'
#' # create a data cube based on the information about the files
#' sinop <- sits_cube(
#'     type = "BRICK", satellite = "TERRA",
#'     sensor = "MODIS", name = "Sinop-crop",
#'     timeline = timeline_modis_392,
#'     output_dir = tempdir(),
#'     bands = c("NDVI"), files = files
#' )
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # label the classification and smooth the result with a bayesian filter
#' sinop_bayes <- sits_bayes_smoothing(sinop_probs,
#'     output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_smooth_bayes <- function(cube,
                              window = matrix(1,
                                              nrow = 3,
                                              ncol = 3,
                                              byrow = TRUE
                              ),
                              variance = 20,
                              output_dir = "./",
                              version = "v1") {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that("probs_cube" %in% class(cube[1,]),
            msg = "sits_smooth_bayes: input is not probability cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(nrow(window) == ncol(window),
            msg = "sits_smooth_bayes: window must have equal sizes"
    )

    # prediction 3 - test variance
    assertthat::assert_that(variance > 1,
            msg = "sits_smooth_bayes: variance must be more than 1"
    )

    # find out how many labels exist
    n_labels <- length(.sits_cube_labels(cube[1,]))

    # allocate matrix of probabilities
    cube_size <- cube[1,]$nrows * cube[1,]$ncols
    smooth_values <- matrix(NA, nrow = cube_size, ncol = n_labels, byrow = TRUE)

    # create metadata for labeled raster cube
    cube_bayes <- .sits_cube_clone(
        cube = cube,
        ext = "_bayes",
        output_dir = output_dir,
        version = version
    )
    # retrieve the files to be read and written
    in_files <- .sits_cube_files(cube)
    out_files <- .sits_cube_files(cube_bayes)

    # retrieve the scale factor
    scale_factor <- cube[1,]$scale_factors[[1]][1]
    mult_factor <- 1/scale_factor

    purrr::map2(in_files, out_files,
                function(in_file, out_file) {
                    values <- .sits_raster_api_read_extent(in_file)
                    # avoid extreme values
                    values[values < 1] <- 1
                    values[values > 9999] <- 9999
                    for (b in 1:n_labels) {
                        # create a matrix with the values of each label
                        band <- matrix(
                            as.matrix(values[ ,b]),
                            nrow = cube[1,]$nrows,
                            ncol = cube[1,]$ncols,
                            byrow = TRUE
                        )
                        # calculate the bayes smoothing
                        values[, b] <- bayes_estimator(band,
                                                       window,
                                                       variance,
                                                       mult_factor)
                    }
                    # write values into a file
                    cube_bayes <- .sits_raster_api_write(
                        params = .sits_raster_api_params_cube(cube_bayes[1, ]),
                        num_layers = n_labels,
                        values = values,
                        filename = out_file,
                        datatype = "INT2U",

                    )
                })
    return(cube_bayes)
}

