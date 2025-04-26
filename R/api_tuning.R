#' @title Get random hyper-parameter
#'
#' @description
#' Evaluate params by returning random numbers according
#' to params definition returned by \code{sits_tuning_hparams}
#'
#' @keywords internal
#' @noRd
#' @param trial current trial
#' @param params Hyperparameters
#' @return A list with random values for the hyperparameters
#'
.tuning_pick_random <- function(trial, params) {
    # uniform distribution
    uniform <- function(min = 0.0, max = 1.0) {
        stats::runif(n = 1L, min = min, max = max)

    }
    # random choice
    choice <- function(..., replace = TRUE) {
        options <- as.list(substitute(list(...), environment()))[-1L]
        val <- sample(x = options, replace = replace, size = 1L)
        if (length(val) == 1L) val <- val[[1L]]
        unlist(val)
    }
    # normal distribution
    normal <- function(mean = 0.0, sd = 1.0) {
        stats::rnorm(n = 1L, mean = mean, sd = sd)
    }
    # lognormal distribution
    lognormal <- function(meanlog = 0.0, sdlog = 1.0) {
        stats::rlnorm(n = 1L, meanlog = meanlog, sdlog = sdlog)
    }
    # loguniform distribution
    loguniform <- function(minlog = 0.0, maxlog = 1.0) {
        base <- exp(1L)
        exp(stats::runif(1L, log(min(c(minlog, maxlog)), base),
                         log(max(c(minlog, maxlog)), base)))
    }
    # beta distribution
    beta <- function(shape1, shape2) {
        stats::rbeta(n = 1L, shape1 = shape1, shape2 = shape2)
    }
    # get
    params <- purrr::map(as.list(params), eval, envir = environment())
    params[["samples"]] <- NULL
    params
}
#' @title Convert hyper-parameters list to a tibble
#' @name .tuning_params_as_tibble
#' @keywords internal
#' @noRd
#' @description
#' Generate a tibble (one row per trial) with all model parameters
#' @param  params   hyperparams from sits_tuning function
#' @return A named list with provided parameters
#'
.tuning_params_as_tibble <- function(params) {
    params <- lapply(params, function(x) {
        if (purrr::is_atomic(x)) {
            if (length(x) != 1L) {
                list(x)
            }
            x
        }
        if (purrr::is_list(x)) {
            list(.tuning_params_as_tibble(x))
        }
        if (is.language(x)) {
            deparse(x)
        }
        list(x)
    })
    tibble::tibble(!!!params)
}
