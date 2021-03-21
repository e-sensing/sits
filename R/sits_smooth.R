#' @title Post-process a classified data raster probs using smoothing
#'
#' @name  sits_smooth
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and applies a smoothing function. There are three options,
#'              defined by the "type" parameter:
#' \itemize{
#'    \item{"bayes": }{Use a bayesian smoother}
#'    \item{"gaussian": }{Use a gaussian smoother}
#'    \item{"bilinear: }{Use a bilinear smoother}
#'
#' }
#' @param  cube              Probability data cube
#' @param  type              Type of smoothing
#' @param  ...               Parameters for specific functions
#' @param  window_size       Size of the neighbourhood.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a matrix or a scalar.
#' @param  covar             a logical argument indicating if a covariance
#'                           matrix must be computed as the prior covariance
#'                           for bayesian smoothing.
#' @param  sigma             Standard deviation of the spatial Gaussian kernel
#'                           (for gaussian and bilinear smoothing)
#' @param  tau               Standard deviation of the class probs value
#'                           (for bilinear smoothing)
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the smoothing.
#' @param  output_dir        Output directory where to out the file
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#'
#' @return A tibble with metadata about the output raster objects.
#'
#' @references K. Schindler, "An Overview and Comparison of Smooth Labeling
#'             Methods for Land-Cover Classification",
#'             IEEE Transactions on Geoscience and Remote Sensing,
#'             50 (11), 4534-4545, 2012 (for gaussian and bilinear smoothing)
#'
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
#' # create a data cube based on the information about the files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "LOCAL",
#'     name = "sinop-2014",
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "band", "date")
#' )
#'
#' # classify the raster image
#' probs_cube <- sits_classify(cube,
#'     ml_model = rfor_model,
#'     output_dir = tempdir(),
#'     memsize = 4, multicores = 2
#' )
#'
#' # smooth the result with a bayesian filter
#' bayes_cube <- sits_smooth(probs_cube,
#'      type = "bayes", output_dir = tempdir()
#' )
#'
#' # smooth the result with a gaussian filter
#' gauss_cube <- sits_smooth(probs_cube,
#'     type = "gaussian", output_dir = tempdir()
#' )
#'
#' # smooth the result with a bilinear filter
#' bil_cube <- sits_smooth(probs_cube,
#'     type = "bilinear", output_dir = tempdir()
#' )
#' }
#'
#' @export
sits_smooth <- function(cube, type = "bayes", ...) {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that(
        inherits(cube, "probs_cube"),
        msg = "sits_smooth: input is not probability cube"
    )

    class(type) <- c(type, class(type))

    UseMethod("sits_smooth", type)
}

#' @title Post-process a classified data raster probs using Bayesian smoothing
#'
#' @rdname  sits_smooth
#'
sits_smooth.bayes <- function(cube, type = "bayes", ...,
                              window_size = 5,
                              smoothness = 20,
                              covar = FALSE,
                              multicores = 2,
                              memsize = 4,
                              output_dir = getwd(),
                              version = "v1") {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that(
        inherits(cube, "probs_cube"),
        msg = "sits_smooth: input is not probability cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(
        window_size %% 2 != 0,
        msg = "sits_smooth: window_size must be an odd number"
    )

    # find out how many labels exist
    n_labels <- length(.sits_cube_labels(cube[1,]))

    # precondition 3 - test variance
    if (is.matrix(smoothness)) {
        assertthat::assert_that(
            (nrow(smoothness) == ncol(smoothness)) &&
                (ncol(smoothness) == n_labels),
            msg = paste("sits_smooth: smoothness must be square matrix of",
                        "the same length as the number of labels")
        )
    } else {
        assertthat::assert_that(
            smoothness > 1,
            msg = "sits_smooth: smoothness must be more than 1"
        )
        smoothness <- diag(smoothness, nrow = n_labels, ncol = n_labels)
    }

    # precondition 4 - multicores
    assertthat::assert_that(
        multicores >= 1,
        msg = "sits_smooth: multicores must be at least 1"
    )

    # precondition 5 - memory
    assertthat::assert_that(
        memsize > 0,
        msg = "sits_smooth: memsize must be positive"
    )

    # create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)

    # create metadata for labelled raster cube
    cube_bayes <- .sits_cube_clone(
        cube = cube,
        name = paste0(cube$name, "_bayes"),
        ext = "_bayes",
        output_dir = output_dir,
        version = version
    )

    # retrieve the scale factor
    scale_factor <- .sits_config_probs_scale_factor()
    mult_factor <- 1 / scale_factor

    # Bayesian smoother to be executed by workers cluster
    .do_bayes <- function(chunk) {

        data <- unname(raster::values(chunk))

        # fix probabilities
        maxprob <- mult_factor - ncol(data) + 1
        data[data == 0] <- 1
        data[data > maxprob] <- maxprob

        # compute logit
        logit <- log(data / (rowSums(data) - data))

        # process Bayesian
        data <- bayes_smoother(m = logit,
                               m_nrow = raster::nrow(chunk),
                               m_ncol = raster::ncol(chunk),
                               w = window,
                               sigma = smoothness,
                               covar_sigma0 = covar)

        # calculate the Bayesian probability for the pixel
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
                            multicores = multicores,
                            memsize = memsize,
                            datatype = "INT2U",
                            options = c("COMPRESS=LZW",
                                        "BIGTIFF=YES"))

    return(cube_bayes)
}

#' @title Post-process a probability cube using Gaussian smoothing
#' @rdname  sits_smooth
#'
sits_smooth.gaussian <- function(cube, type = "gaussian", ...,
                                 window_size = 5,
                                 sigma = 1,
                                 multicores = 2,
                                 memsize = 4,
                                 output_dir = getwd(),
                                 version = "v1") {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that(
        inherits(cube, "probs_cube"),
        msg = "sits_smooth: input is not probability cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(
        window_size %% 2 != 0,
        msg = "sits_smooth: window_size must be an odd number"
    )

    # prediction 3 - test variance
    assertthat::assert_that(
        sigma > 0,
        msg = "sits_smooth: smoothness must be positive"
    )

    # precondition 4 - multicores
    assertthat::assert_that(
        multicores >= 1,
        msg = "sits_smooth: multicores must be at least 1"
    )

    # precondition 5 - memsize
    assertthat::assert_that(
        memsize > 0,
        msg = "sits_smooth: memsize must be positive"
    )

    # create output window
    gauss_kernel <- .sits_gauss_kernel(window_size = window_size,
                                       sigma = sigma)

    # create metadata for Gauss smoothed raster cube
    cube_gauss <- .sits_cube_clone(
        cube = cube,
        name = paste0(cube$name, "_gauss"),
        ext = "_gauss",
        output_dir = output_dir,
        version = version
    )

    # retrieve the scale factor
    scale_factor <- .sits_config_probs_scale_factor()
    mult_factor <- 1 / scale_factor

    # Gaussian smoother to be executed by workers cluster
    .do_gauss <- function(chunk) {

        # scale probabilities
        data <- unname(raster::values(chunk) * scale_factor)

        # process Gaussian smoother
        data <- kernel_smoother(m = data,
                                m_nrow = raster::nrow(chunk),
                                m_ncol = raster::ncol(chunk),
                                w = gauss_kernel,
                                normalised = TRUE)

        # create cube smooth
        res <- raster::brick(chunk, nl = raster::nlayers(chunk))
        res[] <- data * mult_factor

        return(res)
    }

    # process each brick layer (each time step) individually
    .sits_map_layer_cluster(cube = cube,
                            cube_out = cube_gauss,
                            overlapping_y_size =
                                ceiling(window_size / 2) - 1,
                            func = .do_gauss,
                            multicores = multicores,
                            memsize = memsize,
                            datatype = "INT2U",
                            options = c("COMPRESS=LZW",
                                        "BIGTIFF=YES"))

    return(cube_gauss)
}
#' @title Post-process a probability cube using using bilinear smoothing
#' @rdname  sits_smooth
sits_smooth.bilinear <- function(cube,
                                 type = "bilinear",
                                 ...,
                                 window_size = 5,
                                 sigma = 1,
                                 tau = 0.25,
                                 multicores = 2,
                                 memsize = 4,
                                 output_dir = getwd(),
                                 version = "v1") {

    # precondition 1 - check if cube has probability data
    assertthat::assert_that(
        inherits(cube, "probs_cube"),
        msg = "sits_smooth: input is not probability cube"
    )

    # precondition 2 - test window size
    assertthat::assert_that(
        window_size %% 2 != 0,
        msg = "sits_smooth: window_size must be an odd number"
    )

    # prediction 3 - test variance
    assertthat::assert_that(
        sigma > 0,
        msg = "sits_smooth: smoothness must be positive"
    )

    # prediction 4 - test variance
    assertthat::assert_that(
        sigma > 0,
        msg = "sits_smooth: smoothness must be positive"
    )

    # precondition 5 - multicores
    assertthat::assert_that(
        multicores >= 1,
        msg = "sits_smooth: multicores must be at least 1"
    )

    # precondition 6 - memsize
    assertthat::assert_that(
        memsize > 0,
        msg = "sits_smooth: memsize must be positive"
    )

    # create output window
    gauss_kernel <- .sits_gauss_kernel(window_size = window_size,
                                       sigma = sigma)

    # create metadata for bilinear smoothed raster cube
    cube_bilinear <- .sits_cube_clone(
        cube = cube,
        name = paste0(cube$name, "_bilin"),
        ext = "_bilin",
        output_dir = output_dir,
        version = version
    )

    # retrieve the scale factor
    scale_factor <- .sits_config_probs_scale_factor()
    mult_factor <- 1 / scale_factor

    # Gaussian smoother to be executed by workers cluster
    .do_bilinear <- function(chunk) {

        # scale probabilities
        data <- unname(raster::values(chunk) * scale_factor)

        # process bilinear smoother
        data <- bilinear_smoother(m = data,
                                  m_nrow = raster::nrow(chunk),
                                  m_ncol = raster::ncol(chunk),
                                  w = gauss_kernel,
                                  tau = tau)

        # create cube smooth
        res <- raster::brick(chunk, nl = raster::nlayers(chunk))
        res[] <- data * mult_factor

        return(res)
    }

    # process each brick layer (each time step) individually
    .sits_map_layer_cluster(cube = cube,
                            cube_out = cube_bilinear,
                            overlapping_y_size =
                                ceiling(window_size / 2) - 1,
                            func = .do_bilinear,
                            multicores = multicores,
                            memsize = memsize,
                            datatype = "INT2U",
                            options = c("COMPRESS=LZW",
                                        "BIGTIFF=YES"))

    return(cube_bilinear)
}
