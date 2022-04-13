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
#' @param ml_functions       List of machine learning functions to be tested.
#' @param opt_functions      Optimization functions to be tested.
#' @param opt_learning_rates Learning rates to be tested.
#' @param opt_eps_values     Values of eps to be tested. Each eps value
#'   is the term added to the denominator to improve numerical stability.
#' @param opt_weight_decays  Values of weight decay to be tested.
#'   These are the the L2 regularization params (note the weight decay is
#'   not correctly implemented in the adam optimization)
#' @param multicores         Multicores to be used
#'
#'
#' @return A list containing the best torch optimizer and its parameters.
#'
#' @examples
#' \donttest{
#'
#' # tuning cerrado samples
#' data(cerrado_2classes)
#' hparams <- sits_tuning(
#'     samples = cerrado_2classes,
#'     ml_functions = list(sits_tempcnn),
#'     opt_functions = list(torch::optim_adam),
#'     opt_learning_rates = c(0.005, 0.001),
#'     opt_eps_values = 1e-06,
#'     opt_weight_decays = 0)
#' }
#' @export
#'
sits_tuning <- function(samples,
                        samples_validation = NULL,
                        validation_split   = 0.2,
                        ml_functions       = c("tempcnn", "lighttae",
                                               "tae", "resnet"),
                        opt_functions      = c("adam", "adamw"),
                        opt_learning_rates = c(1e-02, 5e-03, 1e-03),
                        opt_eps_values     = c(1e-06, 1e-07, 1e-08),
                        opt_weight_decays  = c(0, 1e-05, 1e-06),
                        multicores         = 2) {

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
    .check_chr_within(
        x = ml_functions,
        within = .config_names(key = "tuning_supported_ml_functions"),
        msg = "invalid 'ml_functions' values"
    )
    # check 'opt_functions' parameter
    .check_chr_within(
        x = opt_functions,
        within = .config_names(key = "tuning_supported_dl_optim_functions"),
        msg = "invalid 'opt_functions' values"
    )
    # check necessary packages
    purrr::map(opt_functions, function(opt) {
        opt_conf <- .config_get(
            key = c("tuning_supported_dl_optim_functions", opt)
        )
        opt_pckg <- opt_conf[[1]]
        .check_that(
            x = requireNamespace(opt_pckg, quietly = TRUE),
            local_msg = paste0("Please, install ", opt_pckg, " package"),
            msg = paste0("optimizer function '", opt, "' is not supported")
        )
    })

    # check 'opt_learning_rates' parameter
    .check_num(
        x = opt_learning_rates, min = 0, allow_zero = FALSE, len_min = 1,
        msg = "invalid 'opt_learning_rates' parameter"
    )
    # check 'opt_eps_values' parameter
    .check_num(
        x = opt_eps_values, min = 0, allow_zero = FALSE, len_min = 1,
        msg = "invalid 'opt_eps_values' parameter"
    )
    # check 'opt_weight_decays' parameter
    .check_num(
        x = opt_weight_decays, min = 0, len_min = 1,
        msg = "invalid 'opt_weight_decays' parameter"
    )
    # check 'multicores' parameter
    .check_num(
        x = multicores, min = 1, len_min = 1, len_max = 1, is_integer = TRUE,
        msg = "invalid 'opt_learning_rates' parameter"
    )

    # combine hyper parameters
    params <- purrr::cross(
        list(ml_functions, opt_functions,
             opt_learning_rates, opt_eps_values,
             opt_weight_decays)
    )

    # start processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # validate in parallel
    acc_lst <- .sits_parallel_map(params, function(param) {

        # initialize variables
        param_ml_fn <- param[[1]]
        param_opt_fn <- param[[2]]
        lr <- param[[3]]
        eps <- param[[4]]
        weight_decay <- param[[5]]

        # resolve ml function
        ml_fn <- .check_error(
            get(.config_get(key = c("tuning_supported_ml_functions",
                                    param_ml_fn)),
                envir = asNamespace("sits"), inherits = FALSE),
            msg = paste0("function '", param_ml_fn, "' not found")
        )

        # resolve optim function
        opt_conf <- .config_get(key = c("tuning_supported_dl_optim_functions",
                                        param_opt_fn))
        opt_pckg <- opt_conf[[1]]

        .check_that(
            x = requireNamespace(opt_pckg, quietly = TRUE),
            local_msg = paste0("Please, install ", opt_pckg, " package"),
            msg = paste0("optimizer function '", param_opt_fn, "' is not supported")
        )

        opt <- opt_conf[[2]]
        opt_fn <-
            .check_error(
                get(opt, envir = asNamespace(opt_pckg), inherits = FALSE),
                msg = paste0("optimizer function '", opt, "' not found in",
                             "package '", opt_pckg, "'")
            )

        # prepare optimizer function
        method_fn <- do.call(
            ml_fn, args = list(
                optimizer = opt_fn,
                opt_hparams = list(
                    lr = lr,
                    eps = eps,
                    weight_decay = weight_decay
                )
            )
        )

        # do validation
        acc <- sits_validate(
            samples = samples,
            samples_validation = samples_validation,
            validation_split = validation_split,
            ml_method = method_fn
        )

        result <- tibble::tibble(
            ml_function = param_ml_fn,
            opt_function = param_opt_fn,
            lr = lr,
            eps = eps,
            weight_decay = weight_decay,
            accuracy = acc[["overall"]][["Accuracy"]],
            kappa = acc[["overall"]][["Kappa"]]
        )

        return(result)
    })

    # unlist all overall accuracies
    tuning_tb <- dplyr::bind_rows(acc_lst) %>%
        dplyr::arrange(dplyr::desc(.data[["accuracy"]]))

    # resolve best ml function
    ml_fn <- get(
        x = .config_get(key = c("tuning_supported_ml_functions",
                                tuning_tb[["ml_function"]][[1]])),
        envir = asNamespace("sits"),
        inherits = FALSE
    )

    # resolve optimizer function
    opt_conf <- .config_get(key = c("tuning_supported_dl_optim_functions",
                                    tuning_tb[["opt_function"]][[1]]))
    opt_pckg <- opt_conf[[1]]
    opt <- opt_conf[[2]]
    opt_fn <- get(
        x = opt,
        envir = asNamespace(opt_pckg),
        inherits = FALSE
    )

    # train best model parameters
    ml_model <- do.call(
        what = ml_fn,
        args = list(
            samples = samples,
            optimizer = opt_fn,
            opt_hparams = list(
                lr = tuning_tb[["lr"]][[1]],
                eps = tuning_tb[["eps"]][[1]],
                weight_decay = tuning_tb[["weight_decay"]][[1]]
            )
        )
    )

    # prepare result
    tuning_lst <- list(
        best_ml_model = ml_model,
        tuning = tuning_tb
    )

    class(tuning_lst) <- c("sits_tune", class(tuning_lst))

    return(tuning_lst)
}
