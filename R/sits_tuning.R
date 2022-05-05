#' @title Tuning machine learning models hyper-parameters
#' @name sits_tuning
#'
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
#' best hyper-parameters in a list.
#'
#' hyper-parameters passed to \code{params} parameter should be passed
#' by calling \code{sits_tuning_hparams()} function.
#'
#' @references
#'  James Bergstra, Yoshua Bengio,
#'  "Random Search for Hyper-Parameter Optimization".
#'  Journal of Machine Learning Research. 13: 281–305, 2012.
#'
#' @param samples            Time series set to be validated.
#' @param samples_validation Time series set used for validation.
#' @param validation_split   Percent of original time series set to be used
#'   for validation (if samples_validation is NULL)
#' @param ml_method          Machine learning method.
#' @param params             List with hyper parameters to be passed to
#'   \code{ml_method}. User can use \code{uniform}, \code{choice},
#'   \code{randint}, \code{normal}, \code{lognormal}, \code{loguniform},
#'   and \code{beta} distribution functions to randomize parameters.
#' @param trials Number of random trials to perform the random search.
#' @param progress           Show progress bar?
#' @param multicores         Number of cores to process in parallel
#' @param ...                Used by \code{sits_tuning_hparams()} to
#'   prepare tuning hyper-parameters
#'
#' @return
#' A tibble containing all parameters used to train on each trial
#'   ordered by accuracy
#'
#' @note
#' Please refer to the sits documentation available in
#'   <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()){
#' # find best learning rate parameters for TempCNN
#' tuned <- sits_tuning(
#'     samples_modis_4bands,
#'     ml_method = sits_tempcnn(),
#'     params = sits_tuning_hparams(
#'         optimizer = choice(
#'             torchopt::optim_adamw,
#'             torchopt::optim_yogi
#'         ),
#'         opt_hparams = list(
#'             lr = beta(0.3, 5)
#'         )
#'     ),
#'     trials = 4,
#'     multicores = 4,
#'     progress = FALSE
#' )
#' # obtain accuracy, kappa and best_lr
#' accuracy <- tuned$tuning$accuracy
#' kappa <- tuned$tuning$accuracy
#' best_lr <- tuned$tuning$params[[1]]$opt_hparams
#'
#' }
#'
#' @export
#'
sits_tuning <- function(samples,
                        samples_validation = NULL,
                        validation_split = 0.2,
                        ml_method = sits_tempcnn(),
                        params = sits_tuning_hparams(
                            optimizer = torchopt::optim_adamw,
                            opt_hparams = list(
                                lr = beta(0.3, 5)
                            )
                        ),
                        trials = 30,
                        multicores = 2,
                        progress = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_tuning_random")

    # pre-conditions
    # check samples parameter
    .sits_tibble_test(samples)
    # check samples_validation parameter if it is passed
    if (!purrr::is_null(samples_validation)) {
        .sits_tibble_test(samples_validation)
    }
    # check validation_split parameter if samples_validation is not passed
    if (purrr::is_null(samples_validation)) {
        .check_num(
            x = validation_split,
            min = 0, max = 0.5, allow_zero = FALSE,
            len_min = 1, len_max = 1,
            msg = "invalid 'validation_split' parameter"
        )
    }
    # check 'ml_functions' parameter
    ml_function <- substitute(ml_method, env = environment())
    if (is.call(ml_function)) ml_function <- ml_function[[1]]
    ml_function <- eval(ml_function, envir = asNamespace("sits"))
    # check 'params' parameter
    .check_lst(
        x = params,
        min_len = 1,
        msg = "invalid 'params' parameter"
    )
    .check_that(
        x = !"samples" %in% names(params),
        local_msg = "cannot pass 'samples' via hyper-parameters",
        msg = "invalid 'params' parameter"
    )
    params_default <- formals(ml_function)
    .check_chr_within(
        x = names(params),
        within = names(params_default),
        msg = "invalid 'params' parameter"
    )
    # update formals with provided parameters in params
    params <- utils::modifyList(params_default, params)
    # check 'multicores' parameter
    .check_num(
        x = multicores, min = 1, len_min = 1, len_max = 1, is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )
    # generate random params
    params_lst <- purrr::map(
        seq_len(trials),
        .tuning_pick_random, params = params
    )

    # start processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # validate in parallel
    result_lst <- .sits_parallel_map(params_lst, function(params) {
        # prepare parameters
        params <- purrr::map(params, eval)

        # prepare ml_method
        ml_method <- do.call(ml_function, args = params)

        # do validation
        acc <- sits_validate(
            samples = samples,
            samples_validation = samples_validation,
            validation_split = validation_split,
            ml_method = ml_method
        )

        result <- tibble::tibble(
            accuracy = acc[["overall"]][["Accuracy"]],
            kappa = acc[["overall"]][["Kappa"]],
            acc = list(acc)
        )

        return(result)
    }, progress = progress, n_retries = 0)

    # prepare result
    result <- dplyr::bind_rows(result_lst)
    # convert parameters to a tibble
    params_tb <- purrr::map_dfr(params_lst, .tuning_params_as_tibble)
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
#' User should pass the possible values for hyper-parameters as
#' constant or by calling the following random functions:
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
#' if (sits_run_examples()){
#' # find best learning rate parameters for TempCNN
#' tuned <- sits_tuning(
#'     samples_modis_4bands,
#'     ml_method = sits_tempcnn(),
#'     params = sits_tuning_hparams(
#'         optimizer = choice(
#'             torchopt::optim_adamw,
#'             torchopt::optim_yogi
#'         ),
#'         opt_hparams = list(
#'             lr = beta(0.3, 5)
#'         )
#'     ),
#'     trials = 4,
#'     multicores = 4,
#'     progress = FALSE
#' )
#'
#' }
#'
#' @export
#'
sits_tuning_hparams <- function(...) {
    params <- substitute(list(...), environment())
    params <- as.list(params)[-1]
    return(params)
}

#' @title Get random hyper-parameter
#'
#' @description
#' Evaluate params by returning random numbers according
#' to params definition returned by \code{sits_tuning_hparams}
#'
#' @keywords internal
.tuning_pick_random <- function(trial, params) {

    uniform <- function(min = 0, max = 1, n = 1) {
        val <- stats::runif(n = n, min = min, max = max)
        return(val)
    }

    choice <- function(..., replace = TRUE, n = 1) {
        options <- as.list(substitute(list(...), environment()))[-1]
        val <- sample(x = options, replace = replace, size = n)
        if (length(val) == 1) val <- val[[1]]
        return(unlist(val))
    }

    randint <- function(min, max, n = 1) {
        val <- as.integer((max - min) * stats::runif(n = n) + min)
        return(val)
    }

    normal <- function(mean = 0, sd = 1, n = 1) {
        val <- stats::rnorm(n = n, mean = mean, sd = sd)
        return(val)
    }

    lognormal <- function(meanlog = 0, sdlog = 1, n = 1) {
        val <- stats::rlnorm(n = n, meanlog = meanlog, sdlog = sdlog)
        return(val)
    }

    loguniform <- function(minlog = 0, maxlog = 1, n = 1) {
        val <- exp((maxlog - minlog) * stats::runif(n = n) + minlog)
        return(val)
    }

    beta <- function(shape1, shape2, n = 1) {
        val <- stats::rbeta(n = 1, shape1 = shape1, shape2 = shape2)
        return(val)
    }

    params <- purrr::map(params, eval, envir = environment())

    params[["samples"]] <- NULL

    return(params)
}

#' @title Convert hyper-parameters list to a tibble
#'
#' @description
#' Generate a tibble (one row per trial) with all model parameters
#'
#' @keywords internal
.tuning_params_as_tibble <- function(params) {
    params <- lapply(params, function(x) {
        if (purrr::is_atomic(x)) {
            if (length(x) != 1) return(list(x))
            return(x)
        }
        if (purrr::is_list(x)) return(list(.tuning_params_as_tibble(x)))
        if (is.language(x)) return(deparse(x))
        return(list(x))
    })
    return(tibble::tibble(!!!params))
}
