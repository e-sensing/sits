#' @title Post-process a classified data raster probs using smoothing
#'
#' @name  sits_smooth
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and applies a smoothing function
#'
#' @param  cube              Probability data cube
#' @param  type              Type of smoothing
#' @param  ...               Parameters for specific functions
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
#' sinop_bayes <- sits_smooth(sinop_probs,
#'     output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_smooth <- function(cube,
                        type = "bayes",
                        ...) {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that("probs_cube" %in% class(cube[1,]),
                            msg = "sits_smooth: input is not probability cube"
    )
    class(type) <- c(type, class(type))
    UseMethod("sits_smooth", type)
}

#' @title Post-process a classified data raster probs using bayesian smoothing
#'
#' @name  sits_smooth.bayes
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of raster bricks with probabilities,
#'              whose metadata is created by \code{\link[sits]{sits_cube}},
#'              and apply a bayesian smoothing process.
#'              If \code{covar} is \code{FALSE} only main diagonal of
#'              covariance prior matrix will be computed.
#'
#' @param  cube              Probability data cube
#' @param  type              Type of smoothing
#' @param  ...               Parameters for specific functions
#' @param  window_size       Size of the neighbourhood.
#' @param  smoothness        Estimated variance of logit of class_probs
#'                           (Bayesian smoothing parameter). It can be either
#'                           a matrix or a scalar.
#' @param  covar             a logical argument indicating if a covariance
#'                           matrix must be computed as the prior covariance.
#' @param  multicores        Number of process to run the Bayesian smoothing in
#'                           snow subprocess.
#' @param  memory            Maximul overall memory (in GB) to run the Bayesian
#'                           smoothing.
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
#' sinop_bayes <- sits_smooth(sinop_probs, output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_smooth.bayes <- function(cube,
                              type = "bayes",
                              ...,
                              window_size = 5,
                              smoothness = 20,
                              covar = FALSE,
                              multicores = 1,
                              memory = 1,
                              output_dir = getwd(),
                              version = "v1") {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that(inherits(cube, "probs_cube"),
            msg = "sits_smooth: input is not probability cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(window_size %% 2 != 0,
            msg = "sits_smooth: window_size must be an odd number"
    )

    # find out how many labels exist
    n_labels <- length(.sits_cube_labels(cube[1,]))

    # precondition 3 - test variance
    if (is.matrix(smoothness)) {
        assertthat::assert_that((nrow(smoothness) == ncol(smoothness)) &&
                                    (ncol(smoothness) == n_labels),
           msg = paste("sits_smooth: smoothness must be square matrix of",
                       "the same length as the number of labels")
        )
    } else {
        assertthat::assert_that(smoothness > 1,
                                msg = "sits_smooth: smoothness must be more than 1"
        )
        smoothness <- diag(smoothness, nrow = n_labels, ncol = n_labels)
    }

    # precondition 4 - multicores
    assertthat::assert_that(multicores >= 1,
                            msg = "sits_smooth: multicores must be at least 1"
    )

    # precondition 5 - memory
    assertthat::assert_that(memory > 0,
                            msg = "sits_smooth: memory must be positive"
    )

    # create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)

    # create metadata for labeled raster cube
    cube_bayes <- .sits_cube_clone(
        cube = cube,
        ext = "_bayes",
        output_dir = output_dir,
        version = version
    )

    # retrieve the scale factor
    scale_factor <- cube[1,]$scale_factors[[1]][1]
    mult_factor <- 1 / scale_factor

    # bayesian inference to be executed  by workers cluster
    .do_bayes <- function(chunk, window, smoothness, covar) {

        data <- unname(raster::values(chunk))

        # fix probs
        maxprob <- mult_factor - ncol(data) + 1
        data[data == 0] <- 1
        data[data > maxprob] <- maxprob

        # compute logit
        logit <- log(data / (rowSums(data) - data))

        # process bayesian
        data <- bayes_multiv_smooth(m = logit,
                                    m_nrow = raster::nrow(chunk),
                                    m_ncol = raster::ncol(chunk),
                                    w = window,
                                    sigma = smoothness,
                                    covar = covar)

        # calculate the bayesian probability for the pixel
        data <- exp(data) * mult_factor / (exp(data) + 1)

        # create cube smooth
        res <- raster::brick(chunk, nl = raster::nlayers(chunk))
        res[] <- data

        return(res)
    }

    # process each brick layer (each time step) individually
    .sits_map_layer_cluster(cube = cube,
                            cube_out = cube_bayes,
                            overlapping_y_size =
                                ceiling(window_size / 2) - 1,
                            func = .do_bayes,
                            func_args = list(
                                window = window,
                                smoothness = smoothness,
                                covar = covar
                            ),
                            multicores = multicores,
                            memory = memory,
                            datatype = "INT2U",
                            options = c("COMPRESS=LZW",
                                        "BIGTIFF=YES"))

    return(cube_bayes)
}

#' @title Post-process a classified data raster probs using gaussian smoothing
#'
#' @name  sits_smooth.gaussian
#' @author @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and apply gaussian smoothing process.
#'
#' @references K. Schindler, "An Overview and Comparison of Smooth Labeling
#'             Methods for Land-Cover Classification",
#'             IEEE Transactions on Geoscience and Remote Sensing,
#'             50 (11), 4534-4545, 2012.
#'
#' @param  cube              Probability data cube
#' @param  type              Type of smoothing
#' @param  ...               Parameters for specific functions
#' @param  window_size       Size of the neighbourhood.
#' @param  sigma             Standard deviation of the spatial gaussian kernel
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
#' # smooth the result with a gaussian filter
#' sinop_gauss <- sits_smooth(sinop_probs,
#'     type = "gaussian",
#'     output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_smooth.gaussian <- function(cube,
                                 type = "gaussian",
                                 ...,
                                 window_size = 5,
                                 sigma = 0.85,
                                 output_dir = "./",
                                 version = "v1") {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that("probs_cube" %in% class(cube[1,]),
                            msg = "sits_smooth: input is not probability cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(window_size %% 2 != 0,
                            msg = "sits_smooth: window_size must be an odd number"
    )

    # prediction 3 - test variance
    assertthat::assert_that(sigma > 0,
                            msg = "sits_smooth: smoothness must be positive"
    )
    # create output window
    gauss_kernel <- matrix(1, nrow = window_size, ncol = window_size)
    center_i <- floor(window_size/2 + 1)
    center_j <- floor(window_size/2 + 1)
    for (i in 1:window_size) {
        for (j in 1:window_size) {
            h <- (i - center_i)^2 + (j - center_j)^2
            gauss_kernel[i,j] <- exp(-h/(2*sigma^2))
        }
    }
    # find out how many labels exist
    n_labels <- length(.sits_cube_labels(cube[1,]))

    # create metadata for labeled raster cube
    cube_gauss <- .sits_cube_clone(
        cube = cube,
        ext = "_gauss",
        output_dir = output_dir,
        version = version
    )
    # retrieve the files to be read and written
    in_files <- .sits_cube_files(cube)
    out_files <- .sits_cube_files(cube_gauss)

    purrr::map2(in_files, out_files,
                function(in_file, out_file) {
                    values <- .sits_raster_api_read_file(in_file)
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
                        # calculate the smoothing
                        values[, b] <- kernel_estimator(band,
                                                        gauss_kernel)
                    }
                    # write values into a file
                    cube_gauss <- .sits_raster_api_write(
                        params = .sits_raster_api_params_cube(cube_gauss[1, ]),
                        num_layers = n_labels,
                        values = values,
                        filename = out_file,
                        datatype = "INT2U",

                    )
                })
    return(cube_gauss)
}
#' @title Post-process a classified data raster probs using bilinear smoothing
#'
#' @name  sits_smooth.bilinear
#' @author @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and apply bilinear smoothing process.
#'
#' @references K. Schindler, "An Overview and Comparison of Smooth Labeling
#'             Methods for Land-Cover Classification",
#'             IEEE Transactions on Geoscience and Remote Sensing,
#'             50 (11), 4534-4545, 2012.
#'
#' @param  cube              Probability data cube
#' @param  type              Type of smoothing
#' @param  ...               Parameters for specific functions
#' @param  window_size       Size of the neighbourhood.
#' @param  sigma             Standard deviation of the spatial gaussian kernel
#' @param  tau               Standard deviation of the class probs value
#' @param  output_dir        Output directory
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
#' # smooth the result with a bilinear filter
#' sinop_bil <- sits_smooth(sinop_probs,
#'     type = "bilinear",
#'     output_dir = tempdir()
#'     )
#' }
#'
#' @export
sits_smooth.bilinear <- function(cube,
                                 type = "bilinear",
                                 ...,
                                 window_size = 5,
                                 sigma = 0.85,
                                 tau = 0.5,
                                 output_dir = "./",
                                 version = "v1") {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that("probs_cube" %in% class(cube[1,]),
                            msg = "sits_smooth: input is not probability cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(window_size %% 2 != 0,
                            msg = "sits_smooth: window_size must be an odd"
    )

    # prediction 3 - test variance
    assertthat::assert_that(sigma > 0,
                            msg = "sits_smooth: sigma must be positive"
    )
    # prediction 3 - test variance
    assertthat::assert_that(tau > 0,
                            msg = "sits_smooth: tau must be positive"
    )
    # create output window
    gauss_kernel <- matrix(1, nrow = window_size, ncol = window_size)
    center_i <- floor(window_size/2 + 1)
    center_j <- floor(window_size/2 + 1)
    for (i in 1:window_size) {
        for (j in 1:window_size) {
            h <- (i - center_i)^2 + (j - center_i)^2
            gauss_kernel[i,j] <- exp(-h/(2*sigma^2))
        }
    }
    # find out how many labels exist
    n_labels <- length(.sits_cube_labels(cube[1,]))

    # create metadata for labeled raster cube
    cube_bilinear <- .sits_cube_clone(
        cube = cube,
        ext = "_bilinear",
        output_dir = output_dir,
        version = version
    )
    # retrieve the files to be read and written
    in_files <- .sits_cube_files(cube)
    out_files <- .sits_cube_files(cube_bilinear)

    # retrieve the scale factor
    scale_factor <- cube[1,]$scale_factors[[1]][1]

    purrr::map2(in_files, out_files,
                function(in_file, out_file) {
                    values <- .sits_raster_api_read_file(in_file)
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
                        # calculate the smoothing
                        val <- kernel_estimator_non_linear(band,
                                                           gauss_kernel,
                                                           tau,
                                                           scale_factor)
                        values[, b] <- val
                    }
                    # write values into a file
                    cube_bilinear <- .sits_raster_api_write(
                        params = .sits_raster_api_params_cube(cube_bilinear[1, ]),
                        num_layers = n_labels,
                        values = values,
                        filename = out_file,
                        datatype = "INT2U",

                    )
                })
    return(cube_bilinear)
}
