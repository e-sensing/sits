#' @title Smooth probability cubes with spatial predictors
#'
#' @name  sits_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and applies a smoothing function. There are two options,
#'              defined by the "type" parameter:
#' \itemize{
#'    \item{"bayes": }{Use a bayesian smoother}
#'    \item{"bilateral: }{Use a bilateral smoother}
#'
#' }
#'
#' @param  cube              Probability data cube
#' @param  type              Type of smoothing
#' @param  ...               Parameters for specific functions
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with highest probability
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a matrix or a scalar.
#' @param  covar             a logical argument indicating if a covariance
#'                           matrix must be computed as the prior covariance
#'                           for bayesian smoothing.
#' @param  sigma             Standard deviation of the spatial Gaussian kernel
#'                           (for bilateral smoothing)
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
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # create a ResNet model
#'     torch_model <- sits_train(samples_modis_ndvi, sits_resnet(epochs = 20))
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = torch_model)
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(bayes_cube)
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
sits_smooth <- function(cube,
                        type = "bayes", ...,
                        window_size = 13,
                        memsize = 4,
                        multicores = 2,
                        output_dir = getwd(),
                        version = "v1") {

    # Check if cube has probability data
    .check_is_probs_cube(cube)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output dir
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)

    # Define the class of the smoothing
    class(type) <- c(type, class(type))
    UseMethod("sits_smooth", type)
}

#' @rdname sits_smooth
#' @export
sits_smooth.bayes <- function(cube, type = "bayes", ...,
                              window_size = 9,
                              neigh_fraction = 0.5,
                              smoothness = 20,
                              covar = FALSE,
                              multicores = 2,
                              memsize = 4,
                              output_dir = getwd(),
                              version = "v1") {
    # Check window size
    .check_window_size(window_size, min = 7)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, min = 0, max = 1)
    # Prepare smoothness parameter
    nlabels <- length(.tile_labels(cube))
    if (!is.matrix(smoothness)) {
        smoothness <- diag(smoothness, nrow = nlabels, ncol = nlabels)
    }
    # Check smoothness
    .check_smoothness_mat(smoothness, nlabels = nlabels)
    # Check covar
    .check_lgl_type(covar)
    # Create smooth function
    smooth_fn <- .smooth_fn_bayes(
        window_size = window_size,
        neigh_fraction = neigh_fraction,
        smoothness = smoothness,
        covar = covar,
        nlabels = nlabels
    )
    # Call smooth function
    probs_cube <- .smooth(
        cube = cube,
        smooth_fn = smooth_fn,
        band = "bayes",
        window_size = window_size,
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = FALSE, ...
    )
    return(probs_cube)
}
#' @rdname sits_smooth
#' @export
sits_smooth.bilateral <- function(cube,
                                  type = "bilateral", ...,
                                  window_size = 5,
                                  sigma = 8,
                                  tau = 0.1,
                                  multicores = 2,
                                  memsize = 4,
                                  output_dir = getwd(),
                                  version = "v1") {
    # Check window size
    .check_window_size(window_size, min = 3)
    # Check variance
    .check_num_parameter(sigma, exclusive_min = 0)
    # Check tau
    .check_num_parameter(tau, exclusive_min = 0)
    # Create smooth function
    smooth_fn <- .smooth_fn_bilat(
        window_size = window_size,
        sigma = sigma,
        tau = tau
    )
    # Call smooth function
    probs_cube <- .smooth(
        cube = cube,
        smooth_fn = smooth_fn,
        band = "bilat",
        window_size = window_size,
        memsize = memsize,
        multicores = multicores,
        output_dir = output_dir,
        version = version,
        progress = FALSE, ...
    )
    return(probs_cube)
}

sits_smooth.default <- function(cube,
                                type = "default", ...,
                                window_size = 13,
                                memsize = 4,
                                multicores = 2,
                                output_dir = getwd(),
                                version = "v1") {
    stop("Invalid `type` parameter ",
         "(value must be one of 'bayes', 'bilateral').")
}
