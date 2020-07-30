#' @title Classify time series or data cube using machine learning models
#' @name sits_classify
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series or data cube given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#'
#' Currenly, sits supports the following models:
#' 'svm' (see \code{\link[sits]{sits_svm}}),
#' random forests (see \code{\link[sits]{sits_rfor}}),
#' linear discriminant analysis (see \code{\link[sits]{sits_lda}}),
#' quadratic discriminant analysis (see \code{\link[sits]{sits_qda}}),
#' multinomial logit (see \code{\link[sits]{sits_mlr}}) and its variants
#' 'lasso' (see \code{\link[sits]{sits_mlr}}) and
#' 'ridge' (see \code{\link[sits]{sits_mlr}}),
#' extreme gradient boosting (see \code{\link[sits]{sits_xgboost}}),
#' and different deep learning functions, including multi-layer perceptrons
#' (see \code{\link[sits]{sits_deeplearning}}, 1D convolutional neural networks
#' \code{\link[sits]{sits_FCN}},
#' mixed 1D and MLP networks \code{\link[sits]{sits_TempCNN}}
#' a 1D version of ResNet \code{\link[sits]{sits_ResNet}}),
#' and a combined LSTM-FCN model \code{\link[sits]{sits_LSTM_FCN}}
#'
#' The model should be precomputed by the user
#' using the function \code{\link[sits]{sits_train}}
#' and then passed to the "sits_classify" function using parameter "ml_model".
#'
#' @param  data              Tibble with time series metadata and data.
#' @param  ml_model          Pre-built machine learning model
#'                           (see \code{\link[sits]{sits_train}}).
#' @param  interval          Interval used for classification (in months).
#' @param  filter            Smoothing filter to be applied (if desired).
#' @param  memsize           Memory available for classification (in GB).
#' @param  multicores        Number of cores to be used for classification.
#' @param  output_dir        Output directory
#' @param  version           Version of classification (for multiple tests)
#' @return A tibble with the predicted labels for each input segment.
#' @examples
#' \donttest{
#' # Retrieve the samples for Mato Grosso
#'
#' # select the bands "ndvi", "evi"
#' samples_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
#'
#' #select a random forest model
#'
#' rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'
#' # classify the point
#'
#' class.tb <- sits_classify(point_ndvi, rfor_model)
#'
#' # plot the classification
#'
#' plot(class.tb)
#'
#' # Classify a raster file with 23 instances for one year
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'                        package = "sits"))
#' # create a data cube based on the information about the files
#' sinop <- sits_cube(type = "BRICK", satellite = "TERRA",
#'                    sensor = "MODIS", name = "Sinop-crop",
#'                    timeline = timeline_modis_392,
#'                    bands = c("ndvi"), files = files)
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop, ml_model = rfor_model,
#'                              memsize = 2, multicores = 1)
#'
#' # label the classified image
#' sinop_label <- sits_label_classification(sinop_probs)
#'
#' # plot the raster image
#' plot(sinop_label, time = 1, title = "Sinop-2013-2014")
#'
#' # smooth the result with a bayesian filter
#' sinop_bayes <- sits_label_classification(sinop_probs,
#'                                smoothing = "bayesian")
#'
#' # plot the smoothened image
#' plot(sinop_bayes, time = 1, title = "Sinop-smooth")
#'
#' # remove the files (cleanup)
#' file.remove(unlist(sinop_probs$files))
#' file.remove(unlist(sinop_label$files))
#' file.remove(unlist(sinop_bayes$files))
#' }
#' @export
sits_classify <- function(data        = NULL,
                          ml_model    = NULL,
                          interval    = "12 month",
                          filter      = NULL,
                          multicores  = 2,
                          memsize     = 4,
                          output_dir  = "./",
                          version     = "v1")
{
    # check if we are running in Windows
    if (.Platform$OS.type != "unix")
        multicores <-  1

    # only savitsky-golay and whittaker filters are supported
    if (!purrr::is_null(filter)) {
      call_names <-  deparse(sys.call())
      assertthat::assert_that(any(grepl("sgolay", (call_names))) ||
                              any(grepl("whittaker", (call_names))),
           msg = "sits_classify_cube: only savitsky-golay and whittaker filters
                  are supported")
    }
    if ("time_series" %in% names(data))
        result <- .sits_classify_ts(data = data,
                                    ml_model = ml_model,
                                    interval = interval,
                                    filter   = filter,
                                    multicores = multicores)
    else {
        result <- .sits_classify_cube(cube = data,
                                    ml_model = ml_model,
                                    interval = interval,
                                    filter = filter,
                                    memsize = memsize,
                                    multicores = multicores,
                                    output_dir = output_dir,
                                    version = version)
    }


    return(result)
}

#' @title Post-process a classified data raster probs to obtain a labelled image
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              whose metadata is]created by \code{\link[sits]{sits_cube}},
#'              and label them, with an optional bayesian smoothing process.
#'
#' @param  cube              Classified image data cube.
#' @param  smoothing         (optional) smoothing method to be applied
#'                           ("none", "bayesian", "majority")
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
#' @return A tibble with metadata about the output RasterLayer objects.
#' @examples
#' \donttest{
#' # Retrieve the samples for Mato Grosso
#' # select the band "ndvi"
#'
#' samples_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
#'
#' #select a random forest model
#' rfor_model <- sits_train(samples_ndvi, ml_method = sits_rfor())
#'
#' # Classify a raster file with 23 instances for one year
#' files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
#'                        package = "sits"))
#'
#' # create a data cube based on the information about the files
#' sinop <- sits_cube(type = "BRICK", satellite = "TERRA",
#'                    sensor = "MODIS", name = "Sinop-crop",
#'                    timeline = timeline_modis_392,
#'                    bands = c("ndvi"), files = files)
#'
#' # classify the raster image
#' sinop_probs <- sits_classify(sinop, ml_model = rfor_model,
#'                                     memsize = 2, multicores = 1)
#'
#' # label the classified image
#' sinop_label <- sits_label_classification(sinop_probs)
#'
#' # plot the raster image
#' plot(sinop_label, time = 1, title = "Sinop-2013-2014")
#'
#' # smooth the result with a bayesian filter
#' sinop_bayes <- sits_label_classification(sinop_probs, smoothing = "bayesian")
#'
#' # plot the smoothened image
#' plot(sinop_bayes, time = 1, title = "Sinop-smooth")
#'
#' # remove the files (cleanup)
#' file.remove(unlist(sinop_probs$files))
#' file.remove(unlist(sinop_label$files))
#' file.remove(unlist(sinop_bayes$files))
#' }
#' @export
sits_label_classification <- function(cube,
                                      smoothing    = "none",
                                      window       = matrix(1,
                                                            nrow = 3,
                                                            ncol = 3,
                                                            byrow = TRUE),
                                      variance     = 20,
                                      output_dir   = "./",
                                      version      = "v1") {

    # precondition 1 - check if cube has probability data
    file_name <- .sits_cube_file(cube)
    assertthat::assert_that(as.logical(grep("probs",(file_name))),
      msg = "sits_label_classification: input is not probability cube")

    # precondition 2 - test smoothing parameters
    assertthat::assert_that(smoothing %in% c("none", "bayesian",
                                             "majority", "bayesian+majority"),
        msg = "sits_label_classification: unknown smoothing method")

    # precondition 3 - test window size
    assertthat::assert_that(nrow(window) == ncol(window),
        msg = "sits_label_classification: window must have equal sizes")

    # prediction 4 - test variance
    if (smoothing == "bayesian" || smoothing == "bayesian+majority")
        assertthat::assert_that(variance > 1,
        msg = "sits_label_classification: variance must be more than 1")


    # extract parameters
    in_files  <- .sits_cube_files(cube)
    r_obj     <- .sits_cube_robj(cube)
    cube_size <- raster::nrow(r_obj)*raster::ncol(r_obj)
    labels    <- .sits_cube_labels(cube)
    n_labels  <- length(labels)
    nrows     <- cube$nrows

    # allocate matrix of  probability image
    values <- matrix(NA, nrow = cube_size, ncol = n_labels)

    # create metadata for labelled raster cube
    cube_labels <- .sits_cube_labelled(cube_probs = cube,
                                       smoothing = smoothing,
                                       output_dir = output_dir,
                                       version = version)
    # retrieve the files to be written
    out_files   <-  .sits_cube_files(cube_labels)

    purrr::map2(in_files, out_files, function(in_file, out_file) {

        for (b in 1:n_labels) {
            # read band values from file using GDAL
            data <- matrix(as.matrix(
                           suppressWarnings(rgdal::readGDAL(
                             fname = in_file,
                             band = b, silent = TRUE)@data)),
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
        layer <- suppressWarnings(raster::raster(r_obj))
        raster::dataType(layer) <- "INT1U"

        # select the best class by choosing the maximum value
        # copy classes to raster
        layer[] <- apply(values, 1, which.max)

        # # apply majority filter
        if (smoothing == "majority" || smoothing == "bayesian+majority") {
            layer <- suppressWarnings(raster::focal(x = layer, w = window,
                            pad = TRUE, na.rm = TRUE, fun = raster::modal))
        }
        # save raster output to file
        layer <- suppressWarnings(raster::writeRaster(layer,
                                                      filename = out_file,
                                                      overwrite = TRUE))
    })

    return(cube_labels)
}
#' @title Classify a set of time series using machine learning models
#' @name .sits_classify_ts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function classifies a set of time series, given
#' a set of training samples, an inference model, and an interval.
#' To perform the classification, users should provide a set of
#' labelled samples. Each samples should be associated to one spatial location
#' (latitude/longitude), one time interval and a label.
#'
#' The model should be precomputed by the user. This model should be
#' passed to the function using the parameter "ml_model".
#'
#' @param  data           Tibble with time series metadata and data.
#' @param  ml_model          Pre-built machine learning model
#'                             (see \code{\link[sits]{sits_train}}).
#' @param  interval          Interval used for classification (in months).
#' @param  filter            Smoothing filter to be applied (if desired).
#' @param  multicores        Number of cores to be used for classification.
#' @return A tibble with the predicted labels for each input segment.
#'
.sits_classify_ts <- function(data, ml_model, interval, filter, multicores) {

    # backward compatibility
    data <- .sits_tibble_rename(data)

    # verify that the data is correct
    .sits_test_tibble(data)

    # ensure the machine learning model has been built
    assertthat::assert_that(!purrr::is_null(ml_model),
        msg = "sits_classify_ts: please provide a trained ML model")

    # filter the data, if filter is not NULL
    if (!purrr::is_null(filter))
        data <- filter(data)

    # retrieve the samples
    samples.tb <- environment(ml_model)$data
    assertthat::assert_that(NROW(samples.tb) > 0,
        msg = "sits_classify_ts: missing original samples")

    # get normalization params
    stats   <- environment(ml_model)$stats
    if (!purrr::is_null(stats))
        # obtain the distances after normalizing data by band
        distances_DT <- .sits_distances(
            .sits_normalize_data(data = data,stats = stats,
                                 multicores = multicores))
    else
        # obtain the distances after normalizing data by band
        distances_DT <- .sits_distances(data)

    assertthat::assert_that(NROW(distances_DT) > 0,
        msg = "sits_classify_ts: problem with normalization")

    # calculate the breaks in the time classification
    class_info.tb <- .sits_timeline_class_info(data = data,
                                               samples = samples.tb,
                                               interval = interval)

    # create a matrix to store the predicted results
    predict.mtx <- .sits_classify_distances(distances_DT = distances_DT,
                                            class_info.tb = class_info.tb,
                                            ml_model = ml_model,
                                            multicores = multicores)

    # Store the result in the input data
    data <- .sits_tibble_prediction(data = data,
                                    class_info.tb = class_info.tb,
                                    pred.mtx = predict.mtx,
                                    interval = interval)

    return(data)
}

#' @title Classify a distances tibble using machine learning models
#' @name .sits_classify_distances
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Returns a sits tibble with the results of the ML classifier.
#'
#' @param  distances_DT    data.table with distances.
#' @param  class_info.tb   classification information.
#' @param  ml_model        model trained by \code{\link[sits]{sits_train}}.
#' @param  multicores      number of threads to process the time series.
#' @return A vector with the predicted labels.
.sits_classify_distances <- function(distances_DT, class_info.tb,
                                     ml_model, multicores) {

    # keras-based models run in single-core mode
    if ("keras_model" %in% class(ml_model) || "rfor_model" %in% class(ml_model))
        multicores <- 1
    # define the column names
    attr_names <- names(.sits_distances(environment(ml_model)$data[1,]))
    assertthat::assert_that(length(attr_names) > 0,
        msg = "sits_classify_distances: training data not available")
    # create a data table to store the distances
    dist_DT <- data.table::data.table(nrow = 0, ncol = length(attr_names))

    # select the data table indexes for each time index
    select.lst <- .sits_timeline_distance_indexes(class_info.tb,
                                                  ncol(distances_DT))

    # classify a block of data
    classify_block <- function(block_DT) {
        # create a list to store the data tables to be used for prediction
        row.lst <- purrr::map(select.lst, function(sel_index) {
            rows_DT <- block_DT[, sel_index, with = FALSE]
        })
        # create a set of distances to be classified
        dist_DT <- data.table::rbindlist(row.lst, use.names = FALSE)
        # set the attribute names of the columns
        colnames(dist_DT) <- attr_names

        # classify the subset data
        prediction_DT <- ml_model(dist_DT)

        return(prediction_DT)
    }

    join_blocks <- function(blocks.lst) {
        pred.mtx <-
            blocks.lst %>%
            dplyr::bind_rows()
        return(pred.mtx)
    }
    n_rows_dist <- nrow(distances_DT)
    if (multicores > 1) {
        blocks.lst <- split.data.frame(distances_DT, cut(1:n_rows_dist,
                                       multicores, labels = FALSE))
        # apply parallel processing to the split dat
        results.lst <- parallel::mclapply(blocks.lst, classify_block,
                                          mc.cores = multicores)
        pred.mtx <- join_blocks(results.lst)
    }
    else
        pred.mtx <- classify_block(distances_DT)

    return(pred.mtx)
}

#' @title Classify a data cube using multicore machines
#' @name .sits_classify_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a data cube, whose metadata is
#'    described by tibble (created by \code{\link[sits]{sits_cube}}),
#'    a set of samples used for training a classification model,
#'    a prediction model (created by \code{\link[sits]{sits_train}}),
#'    and produces a classified set of RasterLayers. These
#'    parameters are "memsize" and "multicores".
#'    The "multicores" parameter defines the
#'    number of cores used for processing. The "memsize" parameter  controls
#'    the amount of memory available for classification.
#'
#' @param  cube            Tibble with information about a data cube.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Interval between two sucessive classifications,
#'                         expressed in months.
#' @param  filter          Smoothing filter to be applied (if desired).
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores to be used for classification.
#' @param  output_dir      Directory for output file
#' @param  version         Version of the output (for multiple classifications)
#' @return A tibble with the metadata for the vector of probabilities
#'         for classified RasterLayers.
#'
.sits_classify_cube <- function(cube, ml_model, interval, filter,
                                memsize, multicores, output_dir, version) {

    # checks the classification params
    .sits_check_classify_params(cube, ml_model)

    # CRAN limits the number of cores to 2
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    # if running on check mode, multicores must be 2
    if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        multicores <- 2L
    }
    # find the number of cores
    if (purrr::is_null(multicores))
        multicores <- max(parallel::detectCores(logical = FALSE) - 1, 1)

    # retrieve the samples from the model
    samples  <- environment(ml_model)$data
    assertthat::assert_that(NROW(samples) > 0,
        msg = "sits_classify: original samples not saved")

    # Sanity check - are the cube bands the same as the sample bands
    cube_bands   <- .sits_cube_bands(cube)
    sample_bands <- sits_bands(samples)
    assertthat::assert_that(
        all(cube_bands %in% sample_bands) && all(sample_bands %in% cube_bands),
        msg = "sits_classify: bands in samples different from cube bands")

    # classify the data
    cube_probs <- .sits_classify_multicores(cube = cube,
                                            samples = samples,
                                            ml_model = ml_model,
                                            interval = interval,
                                            filter = filter,
                                            memsize = memsize,
                                            multicores = multicores,
                                            output_dir = output_dir,
                                            version = version)

    return(cube_probs)
}



#' @title Classify a chunk of raster bricks using multicores
#' @name .sits_classify_multicores
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Classifies a block of data using multicores. It breaks
#' the data into horizontal blocks and divides them between the available cores.
#'
#' Reads data using Rgdal, cleans the data for NAs and missing values.
#' The clean data is stored in a data table with the time instances
#' for all pixels of the block. The algorithm then classifies data on
#' an year by year basis. For each year, extracts the sub-blocks for each band.
#'
#' After all cores process their blocks, it joins the result and then writes it
#' in the classified images for each corresponding year.
#'
#' @param  cube            Metadata cube derived from a raster brick.
#' @param  samples         Samples used for training the classification model.
#' @param  ml_model        A model trained by \code{\link[sits]{sits_train}}.
#' @param  interval        Classification interval.
#' @param  filter          Smoothing filter to be applied to the data.
#' @param  memsize         Memory available for classification (in GB).
#' @param  multicores      Number of cores.
#' @param  output_dir      Output directory
#' @param  version         Version of result
#' @return List of the classified raster layers.
.sits_classify_multicores <-  function(cube,
                                       samples,
                                       ml_model,
                                       interval,
                                       filter,
                                       memsize,
                                       multicores,
                                       output_dir,
                                       version) {


    # get the reference r_obj from the existing cube
    r_obj <- .sits_cube_robj(cube)

    # create the medata for the classified cube
    cube_class <- .sits_cube_classified(cube = cube,
                                        samples = samples,
                                        interval = interval,
                                        output_dir = output_dir,
                                        version = version)
    # find out how many layers per brick
    n_layers  <- length(sits_labels(samples)$label)

    # create the Raster objects
    n_objs <- length(.sits_cube_files(cube_class))
    bricks <- vector("list", n_objs)

    # clone the bricks from existing r_obj
    bricks <- purrr::map2(bricks, c(1:n_objs), function(brick, i){
        brick <- suppressWarnings(raster::brick(r_obj, nl = n_layers))
        raster::dataType(brick) <- "INT2U"
        brick@file@name <- .sits_cube_file(cube_class, i)
        return(brick)
    })

    # initiate writing
    bricks <- purrr::map(bricks, function(brick){
        brick <- suppressWarnings(raster::writeStart(brick,
                                                     brick@file@name,
                                                     overwrite = TRUE))
    })
    # retrieve the normalization stats
    stats     <- environment(ml_model)$stats

    # divide the input data in blocks
    bs <- .sits_raster_blocks(cube, ml_model, interval, memsize, multicores)

    # build a list with columns of data table to be processed for each interval
    select.lst <- .sits_timeline_raster_indexes(cube, samples, interval)

    # get the attribute names
    attr_names <- names(.sits_distances(environment(ml_model)$data[1,]))
    assertthat::assert_that(length(attr_names) > 0,
        msg = "sits_classify_distances: training data not available")
    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting classification at %s", start_time))

    # read the blocks
    for (block in 1:bs$n) {
        # read the data
         data_DT <- .sits_raster_read_data(cube = cube, samples = samples,
                                           ml_model = ml_model,
                                           first_row = bs$row[block],
                                           n_rows_block = bs$nrows[block],
                                           stats = stats, filter = filter,
                                           multicores = multicores)
        # process one temporal instance at a time
        n_bricks <- length(bricks)

        bricks <- purrr::pmap(list(select.lst, bricks, c(1:n_bricks)),
                              function(time, brick, iter) {
            # retrieve the values used for classification
            if (all(time))
                dist_DT <- data_DT
            else {
                dist_DT <- data_DT[, time, with = FALSE]
                # set column names for DT
            }
            colnames(dist_DT) <- attr_names
            # predict the classification values
            prediction_DT <- .sits_classify_interval(DT          = dist_DT,
                                                     ml_model   = ml_model,
                                                     multicores = multicores)


            # convert probabilities matrix to INT2U
            scale_factor_save <- 10000
            probs  <- .sits_raster_scale_matrix_integer(
                             values.mx    = as.matrix(prediction_DT),
                             scale_factor = scale_factor_save,
                             multicores   = multicores)

            # write the probabilities
            brick <- suppressWarnings(raster::writeValues(brick, probs,
                                                          bs$row[block]))

            # memory management
            rm(prediction_DT)
            gc()
            .sits_log_debug(paste0("Memory used after processing block ",
                        block, " of interval ", iter, " - ",
                        .sits_mem_used(), " GB"))

            # estimate processing time
            .sits_classify_estimate_processing_time(start_time = start_time,
                                                    select.lst = select.lst,
                                                    bs = bs, block = block,
                                                    time = iter)
            return(brick)
        })

        # save information about memory use for debugging later
        .sits_log_debug(paste0("Processed block starting from ",
                 bs$row[block], " to ", (bs$row[block] + bs$nrows[block] - 1)))
        .sits_log_debug(paste0("Memory used after processing block ",
                 block,  " - ", .sits_mem_used(), " GB"))

    }

    # finish writing
    bricks <- purrr::map(bricks, function(brick){
        brick <- suppressWarnings(raster::writeStop(brick))
    })
    return(cube_class)
}


#' @title Classify one interval of data
#' @name  .sits_classify_interval
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  DT                A data.table with distance values.
#' @param  ml_model          Machine learning model to be applied.
#' @param  multicores        Number of cores to process the time series.
#' @return                   A data table with predicted values of probs
.sits_classify_interval <- function(DT, ml_model, multicores) {
    nrows_DT <- nrow(DT)
    proc_cores <- multicores
    if (!(purrr::is_null(environment(ml_model)$model.keras)) ||
        !(purrr::is_null(environment(ml_model)$result_ranger)) ) {
        proc_cores <- 1
        .sits_log_debug(
            paste0("keras and ranger run on multiple CPUs -
                   setting multicores to 1"))
    }

    # classify a block of data (with data split)
    classify_block <- function(block) {
        # predict the values for each time interval
        pred_block <- ml_model(block)
        return(pred_block)
    }
    # set up multicore processing
    if (proc_cores > 1) {
        # estimate the list for breaking a block
        .sits_log_debug(
            paste0("Memory used before split data - ", .sits_mem_used(), " GB"))
        block.lst <- .sits_raster_split_data(DT, proc_cores)
        # memory management
        rm(DT)
        gc()

        .sits_log_debug(
            paste0("Memory used before mcapply - ", .sits_mem_used(), " GB"))
        # apply parallel processing to the split data
        # (return the results in a list inside a prototype)
        predictions.lst <- parallel::mclapply(block.lst,
                                              classify_block,
                                              mc.cores = proc_cores)

        #memory management
        rm(block.lst)
        gc()
        .sits_log_debug(
            paste0("Memory used after mclapply - ", .sits_mem_used(), " GB"))
        # compose result based on output from different cores
        prediction_DT <- data.table::as.data.table(do.call(rbind,predictions.lst))
        # memory management
        rm(predictions.lst)
        gc()
        .sits_log_debug(
            paste0("Memory after removing predictions - ",
                   .sits_mem_used(), " GB"))
    }
    else {
        # memory management
        .sits_log_debug(
            paste0("Memory used before prediction - ", .sits_mem_used(), " GB"))

        # estimate the prediction vector
        prediction_DT <- ml_model(DT)
        # memory management
        rm(DT)
        gc()
    }

    # are the results consistent with the data input?
    assertthat::assert_that(nrow(prediction_DT) == nrows_DT,
        msg = ".sits_classify_cube -
                    number of rows of probability matrix is different
                    from number of input pixels")

    return(prediction_DT)
}

#' @title Check clasification parameters
#' @name .sits_check_classify_params
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Verify that required parameters are correct.
#'
#' @param  cube            Tibble with information about a data cube.
#' @param  ml_model        An R model trained by \code{\link[sits]{sits_train}}.
#' @return Tests succeeded?
.sits_check_classify_params <- function(cube, ml_model){
    # ensure metadata tibble exists
    assertthat::assert_that(NROW(cube) > 0,
        msg = "sits_classify: invalid metadata for the cube")

    # ensure the machine learning model has been built
    assertthat::assert_that(!purrr::is_null(ml_model),
        msg = "sits-classify: trained ML model not available")

    return(invisible(TRUE))
}

#' @title Estimate the processing time
#' @name .sits_classify_estimate_processing_time
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description This function normalizes values read from a raster brick.
#'
#' @param  start_time     Initial processing time.
#' @param  select.lst     List of time intervals.
#' @param  bs             Raster block parameters.
#' @param  block          Current block.
#' @param  time           Current interval.
#' @return Scaled matrix.
.sits_classify_estimate_processing_time <- function(start_time,
                                                    select.lst,
                                                    bs,
                                                    block,
                                                    time) {
    # compute current time
    current_time <- lubridate::now()

    # compute elapsed time and estimates remaining time
    elapsed_time <- lubridate::time_length(current_time - start_time,
                                            unit = "minute")
    elapsed_intervals <- (block - 1) * length(select.lst) + time
    total_intervals   <- bs$n * length(select.lst)
    if (elapsed_intervals < total_intervals) {
        message(sprintf(
        "Elapsed time %s minute(s).
         Estimated total process time %s minute(s)...",
         round(as.numeric(elapsed_time), 1),
         round(as.numeric((total_intervals/elapsed_intervals) * elapsed_time),
                          1)))
    } else {
        message(sprintf(
        "Classification finished at %s. Total elapsed time: %s minute(s).",
        current_time,
        round(as.numeric(elapsed_time), 1)))
    }
    return(invisible(TRUE))
}


#' @title Shows the predicted labels for a classified tibble
#' @name sits_show_prediction
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function takes a tibble with a classified time series
#' by a machine learning method and displays the result.
#'
#' @param  class.tb  A SITS tibble that has been classified
#' @return returns a tibble with the columns "from", "to", "class"
#' @export
sits_show_prediction <- function(class.tb) {
    .sits_test_tibble(class.tb)
    assertthat::assert_that(all(names(class.tb$predicted[[1]])
                              %in% c("from", "to", "class", "probs")),
                  msg = "sits_show_prediction: tibble has not been classified")
    return(dplyr::select(dplyr::bind_rows(class.tb$predicted),
                       c("from", "to", "class")))
}
#' @title Create an empty tibble to store the results of predictions
#' @name .sits_tibble_prediction
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Create a tibble to store the results of predictions.
#' @param  data            A tibble with the input data.
#' @param  class_info.tb   A tibble with the information on classification.
#' @param  pred.mtx        The result of the classification
#'                          (one class per column and one row per interval).
#' @param  interval        The time interval between two classifications.
#' @return A tibble storing the predictions.
.sits_tibble_prediction <- function(data, class_info.tb, pred.mtx, interval) {
    # retrieve the list of reference dates
    # this list is a global one and it is created based on the samples
    ref_dates.lst   <- class_info.tb$ref_dates[[1]]

    # retrieve the global timeline
    timeline_global <- class_info.tb$timeline[[1]]

    # size of prediction tibble
    nrows <- length(ref_dates.lst)

    # get the labels of the data
    labels <- class_info.tb$labels[[1]]
    n_labels <- length(labels)
    # create a named vector with integers match the class labels
    int_labels <- c(1:n_labels)
    names(int_labels) <- labels

    # compute pred.vec
    pred.vec <-  names(int_labels[max.col(pred.mtx)])

    class_idx <-  1

    predicted.lst <- purrr::pmap(
      list(data$start_date, data$end_date, data$time_series),
          function(row_start_date, row_end_date, row_time_series) {

            # get the timeline of the row
            timeline_row <- lubridate::as_date(row_time_series$Index)

            # the timeline of the row may differe\ from the global timeline
            # this happens when we are processing samples with different dates
            if (timeline_row[1] != timeline_global[1]) {
                # what is the reference start date?
                ref_start_date <- lubridate::as_date(row_start_date)
                # what is the reference end date?
                ref_end_date <- lubridate::as_date(row_end_date)
                # what are the reference dates to do the classification?
                ref_dates.lst <- sits_timeline_match(timeline_row,
                                                     ref_start_date,
                                                     ref_end_date,
                                                     interval)
            }

            # store the classification results
            pred_row.lst <- ref_dates.lst %>%
                purrr::map(function(rd){
                    pred_row <- tibble::tibble(
                                          from      = as.Date(rd[1]),
                                          to        = as.Date(rd[2]),
                                          class     = pred.vec[class_idx],
                                          probs     = list(as.data.frame(pred.mtx[class_idx,]))
                                )
                    class_idx  <<- class_idx + 1
                    return(pred_row)
                })
          # transform the list into a tibble
          predicted.tb <- dplyr::bind_rows(pred_row.lst)
          return(predicted.tb)
        })

    data$predicted <- predicted.lst
    class(data) <- append(class(data), "predicted", after = 0)

    return(data)
}


