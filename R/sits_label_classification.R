#' @title Post-process a classified data raster probs to obtain a labelled image
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_cube}}),
#'              and label them, with an optional bayesian smoothing process.
#'
#' @param  cube              Classified image data cube.
#' @param  smoothing         (optional) smoothing method to be applied ("none", "bayesian", "majority")
#' @param  window            A matrix with the neighborhood window to compute bayesian smooth.
#'                           The central element index (i, j) is given by
#'                           i = floor(nrows(window)/2)+1 and j = floor(ncols(window)/2)+1.
#'                           Elements '0' are excluded from window.
#' @param  variance          Estimated variance of logit of class_probs (Bayesian smoothing parameter).
#' @param  output_dir        Output directory where to out the file
#' @return A tibble with metadata about the output RasterLayer objects.
#' @examples
#' \donttest{
#' # Retrieve the samples for Mato Grosso
#' # Install the inSitu library
#' # devtools::install_github("e-sensing/inSitu")
#' # library(inSitu)
#'
#' samples <- inSitu::br_mt_1_8K_9classes_6bands
#'
#' # select the bands "ndvi", "evi"
#'
#' samples_2bands <- sits_select_bands(samples, ndvi, evi)
#'
#' #select a random forest model
#' rfor_model <- sits_train(samples_2bands, ml_method = sits_rfor())
#'
#' # Classify a raster file with 23 instances for one year
#' # select the bands "ndvi", "evi" from the "inSitu" package
#' evi_file <- system.file("extdata/Sinop", "Sinop_evi_2014.tif", package = "inSitu")
#' ndvi_file <- system.file("extdata/Sinop", "Sinop_ndvi_2014.tif", package = "inSitu")
#'
#' files <- c(ndvi_file, evi_file)
#'
#' # define the timeline for the files
#' time_file <- system.file("extdata/Sinop", "timeline_2014.txt", package = "inSitu")
#' timeline_2013_2014 <- scan(time_file, character())
#'
#' # create a data cube based on the information about the files
#' raster.tb <- sits_cube(name = "Sinop", timeline = timeline_2013_2014,
#'                        bands = c("ndvi", "evi"), files = files)
#'
#' # classify the raster image and generate a probability file
#' raster_probs.tb <- sits_classify(raster.tb, ml_model = rfor_model, memsize = 4, multicores = 2)
#'
#' # label the probability (no smoothing applied by default)
#' raster_class.tb <- sits_label_classification(raster_probs.tb)
#'
#' # plot the raster image
#' sits_plot_raster(raster_class.tb, time = 1, title = "Sinop-2013-2014")
#'
#' # smooth the result with a bayesian filter
#' raster_class_bayes.tb <- sits_label_classification(raster_probs.tb, smoothing = "bayesian")
#'
#' # plot the smoothened image
#' sits_plot_raster(raster_class_bayes.tb, time = 1, title = "Sinop-smooth")
#' }
#' @export
sits_label_classification <- function(cube,
                              smoothing    = "none",
                              window       = matrix(1, nrow = 3, ncol = 3, byrow = TRUE),
                              variance     = 20,
                              output_dir   = "./") {

    # precondition 1 - check if cube has probability data
    file_name <- .sits_cube_file(cube)
    ensurer::ensure_that(file_name, as.logical(grep("probs",(.))),
                         err_desc = "sits_label_classification: input is not a cube with probability values")
    # precondition 2 - test smoothing parameters
    ensurer::ensure_that(smoothing, (.) %in% c("none", "bayesian", "majority", "bayesian+majority"),
                         err_desc = "sits_label_classification: unknown smoothing method")
    # precondition 3 - test window size
    ensurer::ensure_that(window, nrow(.) == ncol(.),
                         err_desc = "sits_label_classification: window must have equal sizes")
    # prediction 4 - test variance
    if (smoothing == "bayesian" || smoothing == "bayesian+majority")
        ensurer::ensure_that(variance, (.) > 1,
                         err_desc = "sits_label_classification: variance must be more than 1")


    # extract parameters
    in_files  <- .sits_cube_files(cube)
    r_obj     <- .sits_cube_robj(cube)
    cube_size <- raster::nrow(r_obj)*raster::ncol(r_obj)
    labels    <- .sits_cube_labels(cube)
    n_labels  <- length(labels)
    nrows     <- cube$nrows
    ncols     <- cube$ncols

    # allocate matrix of  probability image
    values <- matrix(NA, nrow = cube_size, ncol = n_labels)

    # create metadata for labelled raster cube
    cube_labels <-  .sits_cube_labelled(cube, smoothing, output_dir)
    # retrieve the files to be written
    out_files   <-  .sits_cube_files(cube_labels)

    for (i in 1:length(out_files)) {

        for (b in 1:n_labels) {
            # read band values from file using GDAL
            data <- matrix(as.matrix(rgdal::readGDAL(fname = in_files[i], band = b, silent = TRUE)@data),
                                            nrow = nrows, byrow = TRUE)

            # avoid extreme values
            data[data < 1] <- 1
            data[data > 9999] <- 9999

            # for each class, compute the smooth estimator (if required)
            if (smoothing == "bayesian" || smoothing == "bayesian+majority")
                # get smoothed values
                values[, b] <- bayes_estimator_class(data, window, variance)
            else
                values[, b] <- t(data)

        }
        # create a raster object to write
        layer <- raster::raster(r_obj)
        raster::dataType(layer) <- "INT1U"

        # select the best class by choosing the maximum value
        # copy classes to raster
        layer[] <- apply(values, 1, which.max)

        # # apply majority filter
        if (smoothing == "majority" || smoothing == "bayesian+majority") {
            # layer <- raster::focal(x = layer, w = window, pad = TRUE, fun = function (neigh) {
            #     majority_smooth(neigh, n_labels)
            layer <- raster::focal(x = layer, w = window, pad = TRUE, na.rm = TRUE, fun = raster::modal)
        }
        # save raster output to file
        layer <- raster::writeRaster(layer, filename = out_files[i], overwrite = TRUE)
    }
    return(cube_labels)
}



.sits_majority_smooth <-  function (data, window, nclasses) {

    smooth_values <- matrix(NA,
                            nrow = raster::nrow(data),
                            ncol = raster::ncol(data), byrow = TRUE)
    # go through the data
    for (i in 1:nrow(data)) {
        for (j in 1:ncol(data)) {
            # create a matrix of neighbours
            neigh <- matrix(0,
                           nrow = raster::nrow(window),
                           ncol = raster::ncol(window))
            # obtain the matrix of neighbourhood values
            for (k in 1:nrow(window)) {
                for (l in 1:ncol(window)) {
                    # find the neighbour
                    data_i <- i + k - (nrow(window) + 1)/2
                    data_j <- j + l - (ncol(window) + 1)/2
                    # check for boundary conditions
                    if (data_i < 1 || data_i > nrow(data))
                        data_i <- i
                    if (data_j < 1 || data_j > ncol(data))
                        data_j <- j
                    # update the neighbourhood matrix
                    neigh[k,l] <- as.integer(data[data_i, data_j] * window[k, l])
                }
            }
            # calculate the histogram
            hist_neigh <- vector(length = nclasses)
            for (k in 1:nrow(window)) {
                for (l in 1:ncol(window)) {
                    hist_neigh[neigh[k,l]] <-  hist_neigh[neigh[k,l]] + 1
                }
            }
            # verify which are the indices with the majority class
            indices <- which(hist_neigh == max(hist_neigh))
            # if there more than one majority class, choose randomly
            if (length(indices) > 1)
                ind <- sample(1:length(indices), 1)
            else
                ind <- 1
            # get the majority class in the smoothed image
            smooth_values[i,j] <- indices[ind]
        }
    }
    return(as.vector(t(smooth_values)))
}
