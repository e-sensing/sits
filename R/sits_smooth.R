#' @title Post-process a classified data raster probs using smoothing
#'
#' @name  sits_smooth
#'
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
#'    \item{"bilateral: }{Use a bilateral smoother}
#'
#' }
#'
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
#'                           (for gaussian and bilateral smoothing)
#' @param  tau               Standard deviation of the class probs value
#'                           (for bilateral smoothing)
#' @param  multicores        Number of cores to run the smoothing function
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           smoothing.
#' @param  output_dir        Output directory for image files
#' @param  version           Version of resulting image
#'                           (in the case of multiple tests)
#'
#' @return A tibble with metadata about the output raster objects.
#'
#' @references K. Schindler, "An Overview and Comparison of Smooth Labeling
#'             Methods for Land-Cover Classification",
#'             IEEE Transactions on Geoscience and Remote Sensing,
#'             50 (11), 4534-4545, 2012 (for gaussian and bilateral smoothing)
#'
#' @examples
#' \dontrun{
#' # Retrieve the samples for Mato Grosso
#' # select band "ndvi"
#'
#' samples_ndvi <- sits_select(samples_modis_4bands, bands = "NDVI")
#'
#' # select a random forest model
#' rfor_model <- sits_train(samples_ndvi, sits_rfor(num_trees = 500))
#'
#' # create a data cube based on the information about the files
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' cube <- sits_cube(
#'     source = "BDC",
#'     collection = "MOD13Q1-6",
#'     data_dir = data_dir,
#'     delim = "_",
#'     parse_info = c("X1", "X2", "tile", "band", "date")
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
#' # smooth the result with a bilateral filter
#' bil_cube <- sits_smooth(probs_cube,
#'     type = "bilateral", output_dir = tempdir()
#' )
#' }
#'
#' @export
#'
sits_smooth <- function(cube, type = "bayes", ...) {

    # set caller to show in errors
    .check_set_caller("sits_smooth")

    if (!requireNamespace("parallel", quietly = TRUE)) {
        stop("Please install package parallel.", call. = FALSE)
    }

    # check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # define the class of the smoothing
    class(type) <- c(type, class(type))

    UseMethod("sits_smooth", type)
}

#' @rdname sits_smooth
#'
#' @export
#'
sits_smooth.bayes <- function(cube, type = "bayes", ...,
                              window_size = 5,
                              smoothness = 20,
                              covar = FALSE,
                              multicores = 2,
                              memsize = 4,
                              output_dir = tempdir(),
                              version = "v1") {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition 2 - test window size
    .check_that(
        x = window_size %% 2 != 0,
        msg = "window_size must be an odd number"
    )

    # find out how many labels exist
    n_labels <- length(sits_labels(cube[1, ]))

    # precondition 3 - test variance
    if (is.matrix(smoothness)) {
        .check_that(
            x = (nrow(smoothness) == ncol(smoothness)) &&
                (ncol(smoothness) == n_labels),
            msg = paste("smoothness must be square matrix of",
                        "the same length as the number of labels")
        )
    } else {
        .check_num(x = smoothness,
                   min = 1,
                   len_max = 1,
                   allow_zero = FALSE,
                   msg = "smoothness must be greater than 1")
        smoothness <- diag(smoothness, nrow = n_labels, ncol = n_labels)
    }

    # precondition 4 - multicores
    .check_num(x = multicores,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "multicores must be at least 1")

    # precondition 5 - memory
    .check_num(x = memsize,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "memsize must be positive")

    # create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)

    # create metadata for raster cube
    cube_bayes <- .cube_probs_label(
        cube       = cube,
        ext        = "probs_bayes",
        output_dir = output_dir,
        version    = version
    )

    # retrieve the scale factor
    scale_factor <- round(1 / .config_get("probs_cube_scale_factor"))
    mult_factor <- 1 / scale_factor

    # Bayesian smoother to be executed by workers cluster
    .do_bayes <- function(chunk) {

        data <- .raster_get_values(r_obj = chunk)

        # fix probabilities
        maxprob <- mult_factor - ncol(data) + 1
        data[data == 0] <- 1
        data[data > maxprob] <- maxprob

        # compute logit
        logit <- log(data / (rowSums(data) - data))

        # process Bayesian
        data <- bayes_smoother(m = logit,
                               m_nrow = .raster_nrows(chunk),
                               m_ncol = .raster_ncols(chunk),
                               w = window,
                               sigma = smoothness,
                               covar_sigma0 = covar)

        # calculate the Bayesian probability for the pixel
        data <- exp(data) * mult_factor / (exp(data) + 1)

        # create cube smooth
        res <- .raster_rast(r_obj = chunk,
                            nlayers = .raster_nlayers(chunk))

        # copy values
        res <- .raster_set_values(r_obj = res,
                                  values = data)

        return(res)
    }

    # process each brick layer (each time step) individually
    .sits_smooth_map_layer(
        cube = cube,
        cube_out = cube_bayes,
        overlapping_y_size =
            ceiling(window_size / 2) - 1,
        func = .do_bayes,
        multicores = multicores,
        memsize = memsize,
        gdal_datatype = .config_get("probs_cube_data_type"),
        gdal_options = .config_gtiff_default_options()
    )

    return(cube_bayes)
}

#' @rdname sits_smooth
#'
#' @export
#'
sits_smooth.gaussian <- function(cube, type = "gaussian", ...,
                                 window_size = 5,
                                 sigma = 1,
                                 multicores = 2,
                                 memsize = 4,
                                 output_dir = tempdir(),
                                 version = "v1") {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition 2 - test window size
    .check_that(
        x = window_size %% 2 != 0,
        msg = "window_size must be an odd number"
    )

    # prediction 3 - test variance
    .check_num(x = sigma,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "smoothness must be positive")

    # precondition 4 - multicores
    .check_num(x = multicores,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "multicores must be at least 1")

    # precondition 5 - memsize
    .check_num(x = memsize,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "memsize must be positiv")

    # create output window
    gauss_kernel <- .sits_smooth_gauss_kernel(window_size = window_size,
                                              sigma = sigma)

    # create metadata for Gauss smoothed raster cube
    cube_gauss <- .cube_probs_label(
        cube       = cube,
        ext        = "probs_gauss",
        output_dir = output_dir,
        version    = version
    )

    # retrieve the scale factor
    scale_factor <- round(1 / .config_get("probs_cube_scale_factor"))
    mult_factor <- 1 / scale_factor

    # Gaussian smoother to be executed by workers cluster
    .do_gauss <- function(chunk) {

        # scale probabilities
        data <- .raster_get_values(r_obj = chunk) * scale_factor

        # process Gaussian smoother
        data <- kernel_smoother(m = data,
                                m_nrow = .raster_nrows(chunk),
                                m_ncol = .raster_ncols(chunk),
                                w = gauss_kernel,
                                normalised = TRUE)

        # create cube smooth
        res <- .raster_rast(r_obj = chunk,
                            nlayers = .raster_nlayers(chunk))

        # copy values
        res <- .raster_set_values(r_obj = res,
                                  values = data * mult_factor)
        return(res)
    }

    # process each brick layer (each time step) individually
    .sits_smooth_map_layer(
        cube = cube,
        cube_out = cube_gauss,
        overlapping_y_size =
            ceiling(window_size / 2) - 1,
        func = .do_gauss,
        multicores = multicores,
        memsize = memsize,
        gdal_datatype = .config_get("probs_cube_data_type"),
        gdal_options = .config_gtiff_default_options()
    )

    return(cube_gauss)
}

#' @rdname sits_smooth
#'
#' @export
#'
sits_smooth.bilateral <- function(cube,
                                  type = "bilateral",
                                  ...,
                                  window_size = 5,
                                  sigma = 8,
                                  tau = 0.1,
                                  multicores = 2,
                                  memsize = 4,
                                  output_dir = tempdir(),
                                  version = "v1") {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )

    # precondition 2 - test window size
    .check_that(
        x = window_size %% 2 != 0,
        msg = "window_size must be an odd number"
    )

    # prediction 3 - test variance
    .check_num(x = sigma,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "smoothness must be positive")

    # precondition 4 - multicores
    .check_num(x = multicores,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "multicores must be at least 1")

    # precondition 5 - memsize
    .check_num(x = memsize,
               len_max = 1,
               min = 1,
               allow_zero = FALSE,
               msg = "memsize must be positive")

    # create output window
    gauss_kernel <- .sits_smooth_gauss_kernel(window_size = window_size,
                                              sigma = sigma)

    # create metadata for bilateral smoothed raster cube
    cube_bilat <- .cube_probs_label(
        cube       = cube,
        ext        = "probs_bilat",
        output_dir = output_dir,
        version    = version
    )

    # retrieve the scale factor
    scale_factor <- round(1 / .config_get("probs_cube_scale_factor"))
    mult_factor <- 1 / scale_factor

    # Gaussian smoother to be executed by workers cluster
    .do_bilateral <- function(chunk) {

        # scale probabilities
        data <- .raster_get_values(r_obj = chunk) * scale_factor

        # process bilateral smoother
        data <- bilateral_smoother(m = data,
                                   m_nrow = .raster_nrows(chunk),
                                   m_ncol = .raster_ncols(chunk),
                                   w = gauss_kernel,
                                   tau = tau)

        # create cube smooth
        res <- .raster_rast(r_obj = chunk,
                            nlayers = .raster_nlayers(chunk))

        # copy values
        res <- .raster_set_values(r_obj = res,
                                  values = data * mult_factor)

        return(res)
    }

    # process each brick layer (each time step) individually
    .sits_smooth_map_layer(
        cube = cube,
        cube_out = cube_bilat,
        overlapping_y_size =
            ceiling(window_size / 2) - 1,
        func = .do_bilateral,
        multicores = multicores,
        memsize = memsize,
        gdal_datatype = .config_get("probs_cube_data_type"),
        gdal_options = .config_gtiff_default_options()
    )

    return(cube_bilat)
}
