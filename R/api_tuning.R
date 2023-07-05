#' @title Get random hyper-parameter
#'
#' @description
#' Evaluate params by returning random numbers according
#' to params definition returned by \code{sits_tuning_hparams}
#'
#' @keywords internal
#' @noRd
#' @return A list with evaluated random values
#'
.tuning_pick_random <- function(trial, params) {
    uniform <- function(min = 0, max = 1) {
        val <- stats::runif(n = 1, min = min, max = max)
        return(val)
    }

    choice <- function(..., replace = TRUE) {
        options <- as.list(substitute(list(...), environment()))[-1]
        val <- sample(x = options, replace = replace, size = 1)
        if (length(val) == 1) val <- val[[1]]
        return(unlist(val))
    }

    normal <- function(mean = 0, sd = 1) {
        val <- stats::rnorm(n = 1, mean = mean, sd = sd)
        return(val)
    }

    lognormal <- function(meanlog = 0, sdlog = 1) {
        val <- stats::rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog)
        return(val)
    }

    loguniform <- function(minlog = 0, maxlog = 1) {
        base <- exp(1)
        return(exp(stats::runif(1, log(min(c(minlog, maxlog)), base),
                         log(max(c(minlog, maxlog)), base))))
    }

    beta <- function(shape1, shape2) {
        val <- stats::rbeta(n = 1, shape1 = shape1, shape2 = shape2)
        return(val)
    }

    params <- purrr::map(as.list(params), eval, envir = environment())

    params[["samples"]] <- NULL

    return(params)
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
            if (length(x) != 1) {
                return(list(x))
            }
            return(x)
        }
        if (purrr::is_list(x)) {
            return(list(.tuning_params_as_tibble(x)))
        }
        if (is.language(x)) {
            return(deparse(x))
        }
        return(list(x))
    })
    return(tibble::tibble(!!!params))
}
