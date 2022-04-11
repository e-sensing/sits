#' @title Tuning torch optmizer's hyper-parameters
#' @name sits_torch_tuning
#'
#' @description
#' This function combines all parameters and computes torch models to
#' parameter combination, do a validation using validation samples or
#' splitting samples using validation_split. The function returns the
#' best hyper-parameters in a list.
#'
#'
#' @param samples            Time series with the training samples.
#' @param ...                Additional parameters.
#' @param samples_validation Time series with the validation samples. if the
#'                           \code{samples_validation} parameter is provided,
#'                           the \code{validation_split}
#'                           parameter is ignored.
#' @param validation_split   Fraction of training data
#'                           to be used as validation data.
#' @param ml_fns             A list of torch functions to be used.
#' @param opt_fns            A list of optimizer functions to be used.
#' @param lr                 A vector with the learning rate values to
#'                           be tested.
#' @param eps                A vector with the term added to the denominator to
#'                           improve numerical stability.
#' @param weight_decay       A vector with the L2 regularization param.
#' @param multicores         Number of cores to be used for tuning.
#'
#' @return A list containing the best torch optimizer and its parameters.
#'
#' @examples
#' \donttest{
#' # read a set of samples
#' data(cerrado_2classes)
#'
#' # tuning cerrado samples
#' hparams <- sits_torch_tuning(samples = cerrado_2classes,
#'                              ml_fns = list(sits_tempcnn(epochs = 5)),
#'                              opt_fns = list(torch::optim_adam),
#'                              lr = 0.001,
#'                              eps = 1e-06,
#'                              weight_decay = 0,
#'                              multicores = 1)
#' }
#' @export
#'
sits_torch_tuning <- function(samples, ...,
                              samples_validation = NULL,
                              validation_split = 0.2,
                              ml_fns = list(sits_tempcnn,
                                            sits_lighttae),
                              opt_fns = list(torch::optim_adam,
                                             optim_adamw),
                              lr = c(0.01, 0.005, 0.001),
                              eps = c(1e-06, 1e-07, 1e-08),
                              weight_decay = c(0, 1e-05, 1e-06),
                              multicores = 2) {

    # combine hyper parameters
    params <- purrr::cross(list(ml_fns, opt_fns, lr, eps, weight_decay))
    env <- environment()
    # start processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # validate in parallel
    val_lst <- .sits_parallel_map(params, function(param) {

        ml_fn <- param[[1]]
        opt_fn <- param[[2]]
        lr <- param[[3]]
        eps <- param[[4]]
        weight_decay <- param[[5]]

        method_fn <- do.call(ml_fn, args = list(
            optimizer = opt_fn,
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
