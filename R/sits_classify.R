#' @title Classify time series or data cubes
#' @name sits_classify
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Felipe Carvalho, \email{lipecaso@@gmail.com}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description
#' This function classifies a set of time series or data cube using
#' a trained model prediction model created by \code{\link[sits]{sits_train}}.
#'
#' The \code{sits_classify} function takes three types of data as input
#'    and produce there types of output. Users should call
#'    \code{\link[sits]{sits_classify}} but be aware that the parameters
#'    are different for each type of input.
#' \itemize{
#'    \item{\code{\link[sits]{sits_classify.sits}} is called when the input is
#'    a set of time series. The output is the same set
#'    with the additional column \code{predicted}.}
#'    \item{\code{\link[sits]{sits_classify.raster_cube}} is called when the
#'    input is a regular raster data cube. The output is a probability cube,
#'    which has the same tiles as the raster cube. Each tile contains
#'    a multiband image; each band contains the probability that
#'    each pixel belongs to a given class.
#'    Probability cubes are objects of class "probs_cube".}
#'    \item{\code{\link[sits]{sits_classify.vector_cube}} is called for
#'    vector data cubes. Vector data cubes are produced when
#'    closed regions are obtained from raster data cubes using
#'    \code{\link[sits]{sits_segment}}. Classification of a vector
#'    data cube produces a vector data structure with additional
#'    columns expressing the class probabilities for each object.
#'    Probability cubes for vector data cubes
#'    are objects of class "probs_vector_cube".}
#'    }
#'
#' @param  data              Data cube (tibble of class "raster_cube")
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}
#' @param  ...               Other parameters for specific functions.
#' @return                   Time series with predicted labels for
#'                           each point (tibble of class "sits")
#'                           or a data cube with probabilities for each class
#'                           (tibble of class "probs_cube").
#'
#' @note
#' The main \code{sits} classification workflow has the following steps:
#' \enumerate{
#'      \item{\code{\link[sits]{sits_cube}}: selects a ARD image collection from
#'          a cloud provider.}
#'      \item{\code{\link[sits]{sits_cube_copy}}: copies an ARD image collection
#'          from a cloud provider to a local directory for faster processing.}
#'      \item{\code{\link[sits]{sits_regularize}}: create a regular data cube
#'          from an ARD image collection.}
#'      \item{\code{\link[sits]{sits_apply}}: create new indices by combining
#'          bands of a  regular data cube (optional).}
#'      \item{\code{\link[sits]{sits_get_data}}: extract time series
#'          from a regular data cube based on user-provided labelled samples.}
#'      \item{\code{\link[sits]{sits_train}}: train a machine learning
#'          model based on image time series.}
#'      \item{\code{\link[sits]{sits_classify}}: classify a data cube
#'          using a machine learning model and obtain a probability cube.}
#'      \item{\code{\link[sits]{sits_smooth}}: post-process a probability cube
#'          using a spatial smoother to remove outliers and
#'          increase spatial consistency.}
#'      \item{\code{\link[sits]{sits_label_classification}}: produce a
#'          classified map by selecting the label with the highest probability
#'          from a smoothed cube.}
#' }
#'
#' SITS supports the following models:
#' \itemize{
#'      \item{support vector machines:  \code{\link[sits]{sits_svm}};}
#'      \item{random forests:  \code{\link[sits]{sits_rfor}};}
#'      \item{extreme gradient boosting: \code{\link[sits]{sits_xgboost}};}
#'      \item{multi-layer perceptrons: \code{\link[sits]{sits_mlp}};}
#'      \item{temporal CNN: \code{\link[sits]{sits_tempcnn}};}
#'      \item{temporal self-attention encoders:
#'         \code{\link[sits]{sits_lighttae}} and
#'         \code{\link[sits]{sits_tae}}.}
#' }
#'
#'    Please refer to the sits documentation available in
#'    <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @export
sits_classify <- function(data, ml_model, ...) {
    UseMethod("sits_classify", data)
}

#' @title  Classify a set of time series
#' @name sits_classify.sits
#' @description
#' \code{\link[sits]{sits_classify.sits}} is called when the input is
#'    a set of time series. The output is the same set
#'    with the additional column \code{predicted}.
#'
#' @param  data              Set of time series ("sits tibble")
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}
#'                           (closure of class "sits_model")
#' @param  ...               Other parameters for specific functions.
#' @param  filter_fn         Smoothing filter to be applied - optional
#'                           (closure containing object of class "function").
#' @param  impute_fn         Imputation function to remove NA.
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  gpu_memory        Memory available in GPU in GB (default = 4)
#' @param  batch_size        Batch size for GPU classification.
#' @param  progress          Logical: Show progress bar?
#'
#' @return                   Time series with predicted labels for
#'                           each point (tibble of class "sits").
#' @note
#'    Parameter \code{filter_fn} specifies a smoothing filter
#'    to be applied to each time series for reducing noise. Currently, options
#'    are Savitzky-Golay (see \code{\link[sits]{sits_sgolay}}) and Whittaker
#'    (see \code{\link[sits]{sits_whittaker}}) filters. Note that this
#'    parameter should also have been applied to the training set to obtain
#'    the model.
#'
#'    Parameter \code{impute_fn} defines a 1D function that will be used
#'    to interpolate NA values in each time series. Currently sits supports
#'    the \code{\link{impute_linear}} function, but users can define
#'    imputation functions which are defined externally.
#'
#'    Parameter \code{multicores}  defines the number of cores
#'    used for processing. We recommend using as much memory as possible.
#'
#'    When using a GPU for deep learning, \code{gpu_memory} indicates the
#'    memory of the graphics card which is available for processing.
#'    The parameter \code{batch_size} defines the size of the matrix
#'    (measured in number of rows) which is sent to the GPU for classification.
#'    Users can test different values of \code{batch_size} to
#'    find out which one best fits their GPU architecture.
#'
#'    It is not possible to have an exact idea of the size of Deep Learning
#'    models in GPU memory, as the complexity of the model and factors
#'    such as CUDA Context increase the size of the model in memory.
#'    Therefore, we recommend that you leave at least 1GB free on the
#'    video card to store the Deep Learning model that will be used.
#'
#'    For users of Apple M3 chips or similar with a Neural Engine, be
#'    aware that these chips share memory between the GPU and the CPU.
#'    Tests indicate that the \code{memsize}
#'    should be set to half to the total memory and the \code{batch_size}
#'    parameter should be a small number (we suggest the value of 64).
#'    Be aware that increasing these parameters may lead to memory
#'    conflicts.
#' @examples
#' if (sits_run_examples()) {
#'     # Example of classification of a time series
#'     # Retrieve the samples for Mato Grosso
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#'
#'     # classify the point
#'     point_ndvi <- sits_select(point_mt_6bands, bands = c("NDVI"))
#'     point_class <- sits_classify(
#'         data = point_ndvi, ml_model = rf_model
#'     )
#'     plot(point_class)
#' }
#' @export
sits_classify.sits <- function(data,
                               ml_model,
                               ...,
                               filter_fn = NULL,
                               impute_fn = impute_linear(),
                               multicores = 2L,
                               gpu_memory = 4L,
                               batch_size = 2L^gpu_memory,
                               progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_classify_sits")
    # Pre-conditions
    .check_samples_ts(data)
    .check_is_sits_model(ml_model)
    .check_model_has_stats(ml_model)
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    progress <- .message_progress(progress)
    .check_function(impute_fn)
    .check_filter_fn(filter_fn)
    # save batch_size for later use
    sits_env[["batch_size"]] <- batch_size
    # Update multicores
    multicores <- .ml_update_multicores(ml_model, multicores)
    # Do classification
    .classify_ts(
        samples = data,
        ml_model = ml_model,
        filter_fn = filter_fn,
        impute_fn = impute_fn,
        multicores = multicores,
        gpu_memory = gpu_memory,
        progress = progress
    )
}

#' @title    Classify a regular raster cube
#' @name sits_classify.raster_cube
#' @description
#'    Called when the input is a regular raster data cube.
#'    The output is a probability cube,
#'    which has the same tiles as the raster cube. Each tile contains
#'    a multiband image; each band contains the probability that
#'    each pixel belongs to a given class.
#'    Probability cubes are objects of class "probs_cube".
#' @param  data              Data cube (tibble of class "raster_cube")
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}
#' @param  ...               Other parameters for specific functions.
#' @param  roi               Region of interest (either an sf object, shapefile,
#'                           or a numeric vector in WGS 84 with named XY values
#'                           ("xmin", "xmax", "ymin", "ymax") or
#'                           named lat/long values
#'                           ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param  exclusion_mask    Areas to be excluded from the classification
#'                           process. It can be defined by a sf object or by a
#'                           shapefile.
#' @param  filter_fn         Smoothing filter to be applied - optional
#'                           (closure containing object of class "function").
#' @param  impute_fn         Imputation function to remove NA.
#' @param  start_date        Starting date for the classification
#'                           (Date in YYYY-MM-DD format).
#' @param  end_date          Ending date for the classification
#'                           (Date in YYYY-MM-DD format).
#' @param  memsize           Memory available for classification in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  gpu_memory        Memory available in GPU in GB (default = 4)
#' @param  batch_size        Batch size for GPU classification.
#' @param  output_dir        Directory for output file.
#' @param  version           Version of the output.
#' @param  verbose           Logical: print information about processing time?
#' @param  progress          Logical: Show progress bar?
#'
#' @return                   Time series with predicted labels for
#'                           each point (tibble of class "sits")
#'                           or a data cube with probabilities for each class
#'                           (tibble of class "probs_cube").
#'
#' @note
#'  The \code{roi} parameter defines a region of interest. Either:
#'    \enumerate{
#'    \item{A path to a shapefile with polygons;}
#'    \item{An \code{sf} object with POLYGON or MULTIPOLYGON geometry;}
#'    \item{A named XY vector (\code{xmin}, \code{xmax}, \code{ymin},
#'         \code{ymax}) in WGS84;}
#'    \item{A name lat/long vector (\code{lon_min}, \code{lon_max},
#'          \code{lat_min}, \code{lat_max}); }
#'    }
#'
#'    Parameter \code{filter_fn} parameter specifies a smoothing filter
#'    to be applied to each time series for reducing noise. Currently, options
#'    are Savitzky-Golay (see \code{\link[sits]{sits_sgolay}}) and Whittaker
#'    (see \code{\link[sits]{sits_whittaker}}) filters.
#'
#'    Parameter \code{impute_fn} defines a 1D function that will be used
#'    to interpolate NA values in each time series. Currently sits supports
#'    the \code{\link{impute_linear}} function, but users can define
#'    imputation functions which are defined externally.
#'
#'    Parameter \code{memsize} controls the amount of memory available
#'    for classification, while \code{multicores}  defines the number of cores
#'    used for processing. We recommend using as much memory as possible.
#'
#'    Parameter \code{exclusion_mask} defines a region that will not be
#'    classify. The region can be defined by multiple polygons.
#'    Either a path to a shapefile with polygons or
#'    a \code{sf} object with POLYGON or MULTIPOLYGON geometry;
#'
#'    When using a GPU for deep learning, \code{gpu_memory} indicates the
#'    memory of the graphics card which is available for processing.
#'    The parameter \code{batch_size} defines the size of the matrix
#'    (measured in number of rows) which is sent to the GPU for classification.
#'    Users can test different values of \code{batch_size} to
#'    find out which one best fits their GPU architecture.
#'
#'    It is not possible to have an exact idea of the size of Deep Learning
#'    models in GPU memory, as the complexity of the model and factors
#'    such as CUDA Context increase the size of the model in memory.
#'    Therefore, we recommend that you leave at least 1GB free on the
#'    video card to store the Deep Learning model that will be used.
#'
#'    For users of Apple M3 chips or similar with a Neural Engine, be
#'    aware that these chips share memory between the GPU and the CPU.
#'    Tests indicate that the \code{memsize}
#'    should be set to half to the total memory and the \code{batch_size}
#'    parameter should be a small number (we suggest the value of 64).
#'    Be aware that increasing these parameters may lead to memory
#'    conflicts.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Retrieve the samples for Mato Grosso
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#'     # Example of classification of a data cube
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(
#'         data = cube,
#'         ml_model = rf_model,
#'         output_dir = tempdir(),
#'         version = "classify"
#'     )
#'     # label the probability cube
#'     label_cube <- sits_label_classification(
#'         probs_cube,
#'         output_dir = tempdir(),
#'         version = "ex_classify"
#'     )
#'     # plot the classified image
#'     plot(label_cube)
#' }
#' @export
sits_classify.raster_cube <- function(data,
                                      ml_model, ...,
                                      roi = NULL,
                                      exclusion_mask = NULL,
                                      filter_fn = NULL,
                                      impute_fn = impute_linear(),
                                      start_date = NULL,
                                      end_date = NULL,
                                      memsize = 8L,
                                      multicores = 2L,
                                      gpu_memory = 4L,
                                      batch_size = 2L^gpu_memory,
                                      output_dir,
                                      version = "v1",
                                      verbose = FALSE,
                                      progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_classify_raster")
    # preconditions
    .check_is_raster_cube(data)
    .check_cube_is_regular(data)
    .check_is_sits_model(ml_model)
    .check_model_has_stats(ml_model)
    .check_int_parameter(memsize, min = 1L)
    .check_int_parameter(multicores, min = 1L)
    .check_int_parameter(gpu_memory, min = 1L)
    .check_output_dir(output_dir)
    # preconditions - impute and filter functions
    .check_function(impute_fn)
    .check_filter_fn(filter_fn)
    # version is case-insensitive in sits
    version <- .message_version(version)
    # documentation mode? progress is FALSE
    progress <- .message_progress(progress)
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        data <- .cube_filter_spatial(cube = data, roi = roi)
    }
    # Exclusion mask
    if (.has(exclusion_mask)) {
        exclusion_mask <- .mask_as_sf(exclusion_mask)
    }
    # Temporal filter
    start_date <- .default(start_date, .cube_start_date(data))
    end_date <- .default(end_date, .cube_end_date(data))
    data <- .cube_filter_interval(
        cube = data, start_date = start_date, end_date = end_date
    )
    # save batch_size for later use
    sits_env[["batch_size"]] <- batch_size

    # Retrieve the samples from the model
    samples <- .ml_samples(ml_model)
    # Do the samples and tile match their timeline length?
    .check_match_timeline(samples = samples, tile = data)
    # Do the samples and tile match their bands?
    .check_match_bands(samples = samples, tile = data)

    # By default, base bands is null.
    base_bands <- NULL
    if (.cube_is_base(data)) {
        # Get base bands
        base_bands <- intersect(
            .ml_bands(ml_model), .cube_bands(.cube_base_info(data))
        )
    }
    # get non-base bands
    bands <- setdiff(.ml_bands(ml_model), base_bands)

    # Update multicores for models with internal parallel processing
    multicores <- .ml_update_multicores(ml_model, multicores)

    # The following functions define optimal parameters for parallel processing
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0),
        npaths = (
            length(.tile_paths(data, bands)) +
                length(.ml_labels(ml_model)) +
                ifelse(
                    test = .cube_is_base(data),
                    yes = length(.tile_paths(.cube_base_info(data), base_bands)),
                    no = 0
                )
        ),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(data)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    on.exit(.parallel_stop(), add = TRUE)
    # Show processing time information
    start_time <- .classify_verbose_start(verbose, block)
    on.exit(.classify_verbose_end(verbose, start_time), add = TRUE)
    # Classification
    # Process each tile sequentially
    .cube_foreach_tile(data, function(tile) {
        # Classify the data
        .classify_tile(
            tile = tile,
            out_band = "probs",
            bands = bands,
            base_bands = base_bands,
            ml_model = ml_model,
            block = block,
            roi = roi,
            exclusion_mask = exclusion_mask,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            output_dir = output_dir,
            version = version,
            verbose = verbose,
            progress = progress
        )
    })
}
#' @title   Classify a segmented data cube
#' @name sits_classify.segs_cube
#' @description
#' This function is called when the input is a vector data cube.
#' Vector data cubes are produced when closed regions are obtained
#' from raster data cubes using
#' \code{\link[sits]{sits_segment}}. Classification of a vector
#' data cube produces a vector data structure with additional
#' columns expressing the class probabilities for each segment.
#' Probability cubes for vector data cubes
#' are objects of class "probs_vector_cube".
#'
#' @param  data              Data cube (tibble of class "raster_cube")
#' @param  ml_model          R model trained by \code{\link[sits]{sits_train}}
#'                           (closure of class "sits_model")
#' @param  ...               Other parameters for specific functions.
#' @param  roi               Region of interest (either an sf object, shapefile,
#'                           or a numeric vector in WGS 84 with named XY values
#'                           ("xmin", "xmax", "ymin", "ymax") or
#'                           named lat/long values
#'                           ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param  filter_fn         Smoothing filter to be applied - optional
#'                           (closure containing object of class "function").
#' @param  impute_fn         Imputation function to remove NA.
#' @param  start_date        Starting date for the classification
#'                           (Date in YYYY-MM-DD format).
#' @param  end_date          Ending date for the classification
#'                           (Date in YYYY-MM-DD format).
#' @param  memsize           Memory available for classification in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  gpu_memory        Memory available in GPU in GB (default = 4)
#' @param  batch_size        Batch size for GPU classification.
#' @param  n_sam_pol         Number of time series per segment to be classified
#'                           (integer, min = 10, max = 50).
#' @param  output_dir        Directory for output file.
#' @param  version           Version of the output.
#' @param  verbose           Logical: print information about processing time?
#' @param  progress          Logical: Show progress bar?
#'
#' @return                   Vector data cube with probabilities for each class
#'                           included in new columns of the tibble.
#'                           (tibble of class "probs_vector_cube").
#'
#' @note
#' The \code{roi} parameter defines a region of interest. Either:
#'    \enumerate{
#'    \item{A path to a shapefile with polygons;}
#'    \item{An \code{sf} object with POLYGON or MULTIPOLYGON geometry;}
#'    \item{A named XY vector (\code{xmin}, \code{xmax}, \code{ymin},
#'         \code{ymax}) in WGS84;}
#'    \item{A name lat/long vector (\code{lon_min}, \code{lon_max},
#'          \code{lat_min}, \code{lat_max}); }
#'    }
#'
#'    Parameter \code{filter_fn} parameter specifies a smoothing filter
#'    to be applied to each time series for reducing noise. Currently, options
#'    are Savitzky-Golay (see \code{\link[sits]{sits_sgolay}}) and Whittaker
#'    (see \code{\link[sits]{sits_whittaker}}) filters.
#'
#'    Parameter \code{impute_fn} defines a 1D function that will be used
#'    to interpolate NA values in each time series. Currently sits supports
#'    the \code{\link{impute_linear}} function, but users can define
#'    imputation functions which are defined externally.
#'
#'    Parameter \code{memsize} controls the amount of memory available
#'    for classification, while \code{multicores}  defines the number of cores
#'    used for processing. We recommend using as much memory as possible.
#'
#'    For classifying vector data cubes created by
#'    \code{\link[sits]{sits_segment}},
#'    \code{n_sam_pol} controls is the number of time series to be
#'    classified per segment.
#'
#'    When using a GPU for deep learning, \code{gpu_memory} indicates the
#'    memory of the graphics card which is available for processing.
#'    The parameter \code{batch_size} defines the size of the matrix
#'    (measured in number of rows) which is sent to the GPU for classification.
#'    Users can test different values of \code{batch_size} to
#'    find out which one best fits their GPU architecture.
#'
#'    It is not possible to have an exact idea of the size of Deep Learning
#'    models in GPU memory, as the complexity of the model and factors
#'    such as CUDA Context increase the size of the model in memory.
#'    Therefore, we recommend that you leave at least 1GB free on the
#'    video card to store the Deep Learning model that will be used.
#'
#'    For users of Apple M3 chips or similar with a Neural Engine, be
#'    aware that these chips share memory between the GPU and the CPU.
#'    Tests indicate that the \code{memsize}
#'    should be set to half to the total memory and the \code{batch_size}
#'    parameter should be a small number (we suggest the value of 64).
#'    Be aware that increasing these parameters may lead to memory
#'    conflicts.
#'
#'    Please refer to the sits documentation available in
#'    <https://e-sensing.github.io/sitsbook/> for detailed examples.
#' @examples
#' if (sits_run_examples()) {
#'     # train a random forest model
#'     rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#'     # Example of classification of a data cube
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # segment the image
#'     segments <- sits_segment(
#'         cube = cube,
#'         seg_fn = sits_slic(
#'             step = 5,
#'             compactness = 1,
#'             dist_fun = "euclidean",
#'             avg_fun = "median",
#'             iter = 50,
#'             minarea = 10,
#'             verbose = FALSE
#'         ),
#'         output_dir = tempdir()
#'     )
#'     # Create a classified vector cube
#'     probs_segs <- sits_classify(
#'         data = segments,
#'         ml_model = rf_model,
#'         output_dir = tempdir(),
#'         multicores = 4,
#'         n_sam_pol = 15,
#'         version = "segs"
#'     )
#'     # Create a labelled vector cube
#'     class_segs <- sits_label_classification(
#'         cube = probs_segs,
#'         output_dir = tempdir(),
#'         multicores = 2,
#'         memsize = 4,
#'         version = "segs_classify"
#'     )
#'     # plot class_segs
#'     plot(class_segs)
#' }
#' @export
sits_classify.vector_cube <- function(data,
                                      ml_model, ...,
                                      roi = NULL,
                                      filter_fn = NULL,
                                      impute_fn = impute_linear(),
                                      start_date = NULL,
                                      end_date = NULL,
                                      memsize = 8L,
                                      multicores = 2L,
                                      gpu_memory = 4L,
                                      batch_size = 2L^gpu_memory,
                                      output_dir,
                                      version = "v1",
                                      n_sam_pol = 15L,
                                      verbose = FALSE,
                                      progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_classify_segs")
    # preconditions
    .check_is_vector_cube(data)
    .check_is_sits_model(ml_model)
    .check_model_has_stats(ml_model)
    .check_int_parameter(n_sam_pol, min = 5L, allow_null = TRUE)
    .check_int_parameter(memsize, min = 1L, max = 16384L)
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    .check_output_dir(output_dir)
    # preconditions - impute and filter functions
    .check_function(impute_fn)
    .check_filter_fn(filter_fn)
    # version is case-insensitive in sits
    version <- .message_version(version)
    # documentation mode? progress is FALSE
    progress <- .message_progress(progress)

    # save GPU memory info for later use
    sits_env[["batch_size"]] <- batch_size

    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        data <- .cube_filter_spatial(cube = data, roi = roi)
    }
    # Temporal filter
    start_date <- .default(start_date, .cube_start_date(data))
    end_date <- .default(end_date, .cube_end_date(data))
    data <- .cube_filter_interval(
        cube = data, start_date = start_date, end_date = end_date
    )
    # Check if cube has a base band
    base_bands <- NULL
    if (.cube_is_base(data)) {
        base_bands <- intersect(
            .ml_bands(ml_model), .cube_bands(.cube_base_info(data))
        )
    }
    # get non-base bands
    bands <- setdiff(.ml_bands(ml_model), base_bands)
    # Update multicores for models with internal parallel processing
    multicores <- .ml_update_multicores(ml_model, multicores)

    # The following functions define optimal parameters for parallel processing
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0L),
        npaths = length(.tile_paths(data)) + length(.ml_labels(ml_model)),
        nbytes = 8L,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter to find optimal size
    # considering kind of model and use of CPU or GPU
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(data)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    on.exit(.parallel_stop(), add = TRUE)
    # Classification
    # Process each tile sequentially
    .cube_foreach_tile(data, function(tile) {
        # Classify all the segments for each tile
        .classify_vector_tile(
            tile = tile,
            bands = bands,
            base_bands = base_bands,
            ml_model = ml_model,
            block = block,
            roi = roi,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            n_sam_pol = n_sam_pol,
            multicores = multicores,
            memsize = memsize,
            gpu_memory = gpu_memory,
            version = version,
            output_dir = output_dir,
            progress = progress
        )
    })
}
#' @rdname sits_classify
#' @export
sits_classify.tbl_df <- function(data, ml_model, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_classify_tbl_df"))
    }
    sits_classify(data, ml_model, ...)
}
#' @rdname sits_classify
#' @export
sits_classify.derived_cube <- function(data, ml_model, ...) {
    stop(.conf("messages", "sits_classify_derived_cube"))
}
#' @rdname sits_classify
#' @export
sits_classify.default <- function(data, ml_model, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_classify_default"))
    }
    sits_classify(data, ml_model, ...)
}
