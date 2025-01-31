#' @title Tuning machine learning models hyper-parameters
#' @name sits_tuning
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description
#' Machine learning models use stochastic gradient descent (SGD) techniques to
#' find optimal solutions. To perform SGD, models use optimization
#' algorithms which have hyperparameters that have to be adjusted
#' to achieve best performance for each application.
#'
#' This function performs a random search on values of selected hyperparameters.
#' Instead of performing an exhaustive test of all parameter combinations,
#' it selecting them randomly. Validation is done using an independent set
#' of samples or by a validation split.  The function returns the
#' best hyper-parameters in a list. Hyper-parameters passed to \code{params}
#' parameter should be passed by calling \code{sits_tuning_hparams()}.
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
#' @references
#'  James Bergstra, Yoshua Bengio,
#'  "Random Search for Hyper-Parameter Optimization".
#'  Journal of Machine Learning Research. 13: 281–305, 2012.
#'
#' @param samples            Time series set to be validated.
#' @param samples_validation Time series set used for validation.
#' @param  validation_split  Percent of original time series set to be used
#'   for validation (if samples_validation is NULL)
#' @param  ml_method         Machine learning method.
#' @param  params            List with hyper parameters to be passed to
#'   \code{ml_method}. User can use \code{uniform}, \code{choice},
#'   \code{randint}, \code{normal}, \code{lognormal}, \code{loguniform},
#'   and \code{beta} distribution functions to randomize parameters.
#' @param  trials            Number of random trials to perform the random search.
#' @param  progress          Show progress bar?
#' @param  multicores        Number of cores to process in parallel.
#' @param  gpu_memory        Memory available in GPU in GB (default = 4)
#' @param  batch_size        Batch size for GPU classification.
#'
#' @return
#' A tibble containing all parameters used to train on each trial
#'   ordered by accuracy
#'
#' @examples
#' if (sits_run_examples()) {
#'     # find best learning rate parameters for TempCNN
#'     tuned <- sits_tuning(
#'         samples_modis_ndvi,
#'         ml_method = sits_tempcnn(),
#'         params = sits_tuning_hparams(
#'             optimizer = choice(
#'                 torch::optim_adamw
#'             ),
#'             opt_hparams = list(
#'                 lr = loguniform(10^-2, 10^-4)
#'             )
#'         ),
#'         trials = 4,
#'         multicores = 2,
#'         progress = FALSE
#'     )
#'     # obtain best accuracy, kappa and best_lr
#'     accuracy <- tuned$accuracy[[1]]
#'     kappa <- tuned$kappa[[1]]
#'     best_lr <- tuned$opt_hparams[[1]]$lr
#' }
#'
#' @export
sits_tuning <- function(samples,
                        samples_validation = NULL,
                        validation_split = 0.2,
                        ml_method = sits_tempcnn(),
                        params = sits_tuning_hparams(
                            optimizer = torch::optim_adamw,
                            opt_hparams = list(
                                lr = loguniform(10^-2, 10^-4)
                            )
                        ),
                        trials = 30,
                        multicores = 2,
                        gpu_memory = 4,
                        batch_size = 2^gpu_memory,
                        progress = FALSE) {
    # set caller to show in errors
    .check_set_caller("sits_tuning")
    # pre-conditions
    # check samples
    .check_samples_train(samples)
    if (.has(samples_validation)) {
        # check samples_validation parameter if it exists
        .check_samples_train(samples_validation)
    } else {
        # check validation_split parameter if samples_validation is not passed
        .check_num_parameter(validation_split, exclusive_min = 0, max = 0.5)
    }
    # check 'ml_functions' parameter
    ml_function <- substitute(ml_method, env = environment())
    if (is.call(ml_function))
        ml_function <- ml_function[[1]]
    ml_function <- eval(ml_function, envir = asNamespace("sits"))
    # check 'params' parameter
    .check_lst_parameter(params, len_min = 1)
    .check_that(!"samples" %in% names(params),
        msg = .conf("messages", "sits_tuning_samples")
    )
    params_default <- formals(ml_function)
    .check_chr_within(
        x = names(params),
        within = names(params_default)
    )
    # update formals with provided parameters in params
    params <- utils::modifyList(params_default, params)
    # check trials
    .check_int_parameter(trials)
    # check 'multicores' parameter
    .check_int_parameter(multicores, min = 1, max = 2048)
    # generate random params
    params_lst <- purrr::map(
        as.list(seq_len(trials)),
        .tuning_pick_random,
        params = params
    )
    # save batch_size for later use
    sits_env[["batch_size"]] <- batch_size
    # Update multicores
    if (.torch_gpu_classification() &&
        "optimizer" %in% ls(environment(ml_method)))
        multicores <-  1
    # start processes
    .parallel_start(workers = multicores)
    on.exit(.parallel_stop())
    # validate in parallel
    result_lst <- .parallel_map(params_lst, function(params) {
        # Prepare parameters
        params <- purrr::map(params, eval)
        # Prepare ml_method
        ml_method <- do.call(ml_function, args = params)
        # Do validation
        acc <- .validate_sits(
            samples = samples,
            samples_validation = samples_validation,
            validation_split = validation_split,
            ml_method = ml_method
        )
        # Prepare result
        result <- tibble::tibble(
            accuracy = acc[["overall"]][["Accuracy"]],
            kappa = acc[["overall"]][["Kappa"]],
            acc = list(acc)
        )
        # Remove variable 'ml_method'
        remove(ml_method)
        return(result)
    }, progress = progress, n_retries = 0)

    # prepare result
    result <- dplyr::bind_rows(result_lst)
    # convert parameters to a tibble
    params_tb <- .map_dfr(params_lst, .tuning_params_as_tibble)
    # bind results and parameters
    tuning_tb <- dplyr::bind_cols(result, params_tb)
    # order by accuracy
    tuning_tb <- dplyr::arrange(tuning_tb, dplyr::desc(.data[["accuracy"]]))
    # prepare result class
    class(tuning_tb) <- c("sits_tuned", class(tuning_tb))
    return(tuning_tb)
}
#' @title Tuning machine learning models hyper-parameters
#' @name sits_tuning_hparams
#'
#' @description
#' This function allow user building the hyper-parameters space used
#' by \code{sits_tuning()} function search randomly the best parameter
#' combination.
#'
#' Users should pass the possible values for hyper-parameters as
#' constants or by calling the following random functions:
#'
#' \itemize{
#'   \item \code{uniform(min = 0, max = 1, n = 1)}: returns random numbers
#'   from a uniform distribution with parameters min and max.
#'   \item \code{choice(..., replace = TRUE, n = 1)}: returns random objects
#'   passed to \code{...} with replacement or not (parameter \code{replace}).
#'   \item \code{randint(min, max, n = 1)}: returns random integers
#'   from a uniform distribution with parameters min and max.
#'   \item \code{normal(mean = 0, sd = 1, n = 1)}: returns random numbers
#'   from a normal distribution with parameters min and max.
#'   \item \code{lognormal(meanlog = 0, sdlog = 1, n = 1)}: returns random
#'   numbers from a lognormal distribution with parameters min and max.
#'   \item \code{loguniform(minlog = 0, maxlog = 1, n = 1)}: returns random
#'   numbers from a loguniform distribution with parameters min and max.
#'   \item \code{beta(shape1, shape2, n = 1)}: returns random numbers
#'   from a beta distribution with parameters min and max.
#' }
#'
#' These functions accepts \code{n} parameter to indicate how many values
#' should be returned.
#'
#' @param ...  Used to prepare hyper-parameter space
#'
#' @return A list containing the hyper-parameter space to be passed to
#'   \code{sits_tuning()}'s \code{params} parameter.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # find best learning rate parameters for TempCNN
#'     tuned <- sits_tuning(
#'         samples_modis_ndvi,
#'         ml_method = sits_tempcnn(),
#'         params = sits_tuning_hparams(
#'             optimizer = choice(
#'                 torch::optim_adamw,
#'                 torch::optim_adagrad
#'             ),
#'             opt_hparams = list(
#'                  lr = loguniform(10^-2, 10^-4),
#'                  weight_decay = loguniform(10^-2, 10^-8)
#'             )
#'         ),
#'         trials = 20,
#'         multicores = 2,
#'         progress = FALSE
#'     )
#' }
#'
#' @export
#'
sits_tuning_hparams <- function(...) {
    params <- substitute(list(...), environment())
    params <- as.list(params)[-1]
    return(params)
}
