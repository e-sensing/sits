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
#'                           for validation (if samples_validation is NULL)
#' @param ml_methods         Name of machine learning methods to be tested.
#' @param opt_function       Name of optimization functions to be tested.
#' @param learning_rates     Learning rates to be tested.
#' @param eps_values         Values of eps to be tested.
#' @param weight_decays      Values of weight decay to be tested.
#' @param multicores         Multicores to be used
#'
#'
#' @return A list containing the best torch optimizer and its parameters.
#'
#' @examples
#' \donttest{
#' # read a set of samples
#' data(samples_modis_4bands)
#' # two fold validation with random forest
#' hparams <- sits_tuning(samples_modis_4bands)
#' }
#' @export
#'
sits_tuning <- function(samples,
                        samples_validation = NULL,
                        validation_split = 0.2,
                        ml_methods = list(sits_tempcnn,
                                          sits_lighttae),
                        opt_functions = list(torch::optim_adam,
                                             optim_adamw),
                        learning_rates = c(0.01, 0.005, 0.001),
                        eps_values = c(1e-06, 1e-07, 1e-08),
                        weight_decays = c(0, 1e-05, 1e-06),
                        multicores = 2) {

    # combine hyper parameters
    params <- purrr::cross(list(ml_methods,
                                opt_functions,
                                learning_rates,
                                eps_values,
                                weight_decays))

    # start processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # validate in parallel
    val_lst <- .sits_parallel_cluster_apply(params, function(param) {

        ml_fn <- param[[1]]
        opt_fn <- param[[2]]
        lr <- param[[3]]
        eps <- param[[4]]
        weight_decay <- param[[5]]

        method_fn <- do.call(ml_fn, args = list(
            optimizer = list(
                opt_fn
            ),
            opt_hparams = list(
                lr = lr,
                eps = eps,
                weight_decay = weight_decay
            )
        ))

        val <- sits_validate(
            samples = samples,
            samples_validation = samples_validation,
            validation_split = validation_split,
            ml_method = method_fn
        )

        return(val[["overall"]][["Accuracy"]])
    })

    # unlist all overall accuracies
    accuracies <- unlist(val_lst)

    # select best parameter combination
    param <- params[[which.max(accuracies)]]

    # prepare result
    ml_fn <- param[[1]]
    opt_fn <- param[[2]]
    lr <- param[[3]]
    eps <- param[[4]]
    weight_decay <- param[[5]]

    hparams <- list(
        optimizer = list(
            opt_fn
        ),
        opt_hparams = list(
            lr = lr,
            eps = eps,
            weight_decay = weight_decay
        )
    )

    return(hparams)
}
