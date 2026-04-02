#' @title Cross-validate time series samples
#' @name sits_kfold_validate
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Splits the set of time series into training and validation and
#' perform k-fold cross-validation.
#'
#' @note
#' Cross-validation is a technique for assessing how the results
#' of a statistical analysis will generalize to an independent data set.
#' It is mainly used in settings where the goal is prediction,
#' and one wants to estimate how accurately a predictive model will perform.
#' One round of cross-validation involves partitioning a sample of data
#' into complementary subsets, performing the analysis on one subset
#' (called the training set), and validating the analysis on the other subset
#' (called the validation set or testing set).
#'
#' The k-fold cross validation method involves splitting the dataset
#' into k-subsets. For each subset is held out while the model is trained
#' on all other subsets. This process is completed until accuracy
#' is determine for each instance in the dataset, and an overall
#' accuracy estimate is provided.
#'
#' This function returns the confusion matrix, and Kappa values.
#'
#' @param samples            Time series.
#' @param folds              Number of partitions to create.
#' @param ml_method          Machine learning method.
#' @param  filter_fn         Smoothing filter to be applied - optional
#'                           (closure containing object of class "function").
#' @param  impute_fn         Imputation function to remove NA.
#' @param multicores         Number of cores to process in parallel.
#' @param  gpu_memory        Memory available in GPU in GB (default = 4)
#' @param  batch_size        Batch size for GPU classification.
#' @param  progress          Logical: Show progress bar?
#'
#' @return A \code{caret::confusionMatrix} object to be used for
#'         validation assessment.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # A dataset containing a tibble with time series samples
#'     # for the Mato Grosso state in Brasil
#'     # create a list to store the results
#'     results <- list()
#'     # accuracy assessment lightTAE
#'     acc_rfor <- sits_kfold_validate(
#'         samples_modis_ndvi,
#'         folds = 5,
#'         ml_method = sits_rfor()
#'     )
#'     # use a name
#'     acc_rfor$name <- "Rfor"
#'     # put the result in a list
#'     results[[length(results) + 1]] <- acc_rfor
#'     # save to xlsx file
#'     sits_to_xlsx(
#'         results,
#'         file = tempfile("accuracy_mato_grosso_dl_", fileext = ".xlsx")
#'     )
#' }
#'
#' @export
sits_kfold_validate <- function(samples,
                                folds = 5L,
                                ml_method = sits_rfor(),
                                filter_fn = NULL,
                                impute_fn = impute_linear(),
                                multicores = 2L,
                                gpu_memory = 4L,
                                batch_size = 2L^gpu_memory,
                                progress = TRUE) {
    # set caller to show in errors
    .check_set_caller("sits_kfold_validate")
    # require package
    .check_require_packages("caret")
    # pre-condition
    .check_that(inherits(ml_method, "function"))
    # pre-condition
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    # show progress bar?
    progress <- .message_progress(progress)
    # save batch size for later
    sits_env[["batch_size"]] <- batch_size
    # Torch models in GPU need multicores = 1
    if (.torch_gpu_classification() &&
        "optimizer" %in% ls(environment(ml_method))) {
        multicores <- 1L
    }
    # Compute cross-validation accuracy
    acc <- .samples_kfold(
        samples    = samples,
        folds      = folds,
        ml_method  = ml_method,
        filter_fn  = filter_fn,
        impute_fn  = impute_fn,
        multicores = multicores,
        gpu_memory = gpu_memory,
        progress   = progress
    )
    # Return kfold results
    return(acc)
}

#' @title Validate time series samples
#' @name sits_validate
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' One round of cross-validation involves partitioning a sample of data
#' into complementary subsets, performing the analysis on one subset
#' (called the training set), and validating the analysis on the other subset
#' (called the validation set or testing set).
#'
#' The function takes two arguments: a set of time series
#' with a machine learning model and another set with validation samples.
#' If the validation sample set is not provided,
#' The sample dataset is split into two parts, as defined by the parameter
#' validation_split. The accuracy is determined by the result of
#' the validation test set.
#'
#' This function returns the confusion matrix, and Kappa values.
#'
#' @note
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
#' @param samples            Time series to be validated (class "sits").
#' @param samples_validation Optional: Time series used for validation
#'                           (class "sits")
#' @param validation_split   Percent of original time series set to be used
#'                           for validation if samples_validation is NULL
#'                           (numeric value).
#' @param  ml_method         Machine learning method (function)
#' @param  gpu_memory        Memory available in GPU in GB (default = 4)
#' @param  batch_size        Batch size for GPU classification.
#'
#' @return A \code{caret::confusionMatrix} object to be used for
#'         validation assessment.
#'
#' @examples
#' if (sits_run_examples()) {
#'     samples <- sits_sample(cerrado_2classes, frac = 0.5)
#'     samples_validation <- sits_sample(cerrado_2classes, frac = 0.5)
#'     conf_matrix_1 <- sits_validate(
#'         samples = samples,
#'         samples_validation = samples_validation,
#'         ml_method = sits_rfor()
#'     )
#'     conf_matrix_2 <- sits_validate(
#'         samples = cerrado_2classes,
#'         validation_split = 0.2,
#'         ml_method = sits_rfor()
#'     )
#' }
#' @export
sits_validate <- function(samples,
                          samples_validation = NULL,
                          validation_split = 0.2,
                          ml_method = sits_rfor(),
                          gpu_memory = 4L,
                          batch_size = 2L^gpu_memory) {
    # set caller to show in errors
    .check_set_caller("sits_validate")
    # require package
    .check_require_packages("caret")
    # check samples
    .check_samples_train(samples)
    # check validation
    if (!is.null(samples_validation)) {
        .check_samples_train(samples_validation)
    }
    # check validation split
    .check_num(validation_split,
        min = 0.0, max = 1.0,
        len_min = 1L, len_max = 1L
    )
    # pre-condition for ml_method
    .check_that(inherits(ml_method, "function"))

    acc_obj <- .validate_sits(
        samples = samples,
        samples_validation = samples_validation,
        validation_split = validation_split,
        ml_method = ml_method
    )
    return(acc_obj)
}
