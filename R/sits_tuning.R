#' @title Tuning deep learning models hyper-parameters
#' @name sits_tuning
#'
#' @description
#' Deep learning models use stochastic gradient descent techniques to
#' find optimal solutions. To that end, these models use optimization
#' algorithms that approximate the actual solution, which would be
#' computationally expensive. Each of these algorithms uses a set of
#' hyperparameters, that have to be adjusted to achieve best performance
#' for each application.
#' This function combines all parameters and computes torch models to
#' parameter combination, do a validation using validation samples or
#' splitting samples using validation_split. The function returns the
#' best hyper-parameters in a list.
#'
#' @param samples            Time series set to be validated.
#' @param samples_validation Time series set used for validation.
#' @param validation_split   Percent of original time series set to be used
#'   for validation (if samples_validation is NULL)
#' @param ml_method          Machine learning method.
#' @param params             List with hyper parameters to be passed to
#'   \code{ml_method}. User can use \code{uniform}, \code{choice},
#'   \code{randint}, \code{normal}, \code{lognormal}, \code{loguniform},
#'   and \code{beta} functions to randomize parameters.
#' @param trials Number of random trials to perform the random search.
#' @param progress           Show progress bar?
#' @param multicores         Number of cores to process in parallel
#'
#' @return A list containing the best model and a tibble with all performances
#'
#' @examples
#' if (sits_active_tests()) {
#' samples <- sits_select(samples_modis_4bands, bands = "NDVI")
#'
#' tuned <- sits_tuning_random(
#'    samples,
#'    ml_method = sits_tempcnn(),
#'    params = list(
#'        optimizer = torchopt::optim_adamw,
#'        opt_hparams = list(
#'            lr = uniform(0, 1)
#'        )
#'    ),
#'    trials = 4,
#'    multicores = 1,
#'    progress = FALSE
#'  )
#'}
#'
#' @export
#'
sits_tuning <- function(samples,
                        samples_validation = NULL,
                        validation_split = 0.2,
                        ml_method = sits_tempcnn(),
                        params = list(
                            optimizer = torchopt::optim_adamw,
                            opt_hparams = list(
                                lr = uniform(0, 1)
                            )
                        ),
                        trials = 30,
                        multicores = 2,
                        progress = FALSE) {

    # set caller to show in errors
    .check_set_caller("sits_tuning")

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

    # check 'multicores' parameter
    .check_num(
        x = multicores, min = 1, len_min = 1, len_max = 1, is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )

    # generate random params
    params <- substitute(params, environment())
    params_lst <- purrr::map(
        seq_len(trials),
        function(x) .sits_tuning_pick_random(params)
    )

    # start processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # validate in parallel
    acc_lst <- .sits_parallel_map(params_lst, function(params) {

        # prepare optimizer function
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
            params = list(params),
            ml_method = list(ml_method),
            acc = list(acc)
        )

        return(result)
    }, progress = progress, n_retries = 0)

    # unlist all overall accuracies
    tuning_tb <- dplyr::bind_rows(acc_lst) %>%
        dplyr::arrange(dplyr::desc(.data[["accuracy"]]))

    # train best model parameters
    ml_model <- do.call(
        tuning_tb[["ml_method"]][[1]],
        args = list(samples = samples)
    )

    # prepare result
    tuning_lst <- list(
        best_ml_model = ml_model,
        tuning = tuning_tb
    )

    class(tuning_lst) <- c("tuned_model", class(tuning_lst))

    return(tuning_lst)
}

#' @keywords internal
.sits_tuning_pick_random <- function(params) {

    uniform <- function(min = 0, max = 1) {
        stats::runif(n = 1, min = min, max = max)
    }

    choice <- function(...) {
        sample(x = list(...), size = 1)[[1]]
    }

    randint <- function(min, max) {
        rn = as.integer((max - min) * stats::runif(1) + min)
    }

    normal <- function(mean = 0, sd = 1) {
        stats::rnorm(1, mean = mean, sd = sd)
    }

    lognormal <- function(meanlog = 0, sdlog = 1) {
        stats::rlnorm(1, meanlog = meanlog, sdlog = sdlog)
    }

    loguniform <- function(minlog = 0, maxlog = 1) {
        exp((maxlog - minlog) * stats::runif(1) + minlog)
    }

    beta <- function(shape1, shape2) {
        stats::rbeta(1, shape1 = shape1, shape2 = shape2)
    }

    params <- eval(params, envir = environment())

    params[["samples"]] <- NULL

    params
}
