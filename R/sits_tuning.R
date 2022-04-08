#' @title Tuning torch optmizer's hyper-parameters
#' @name sits_torch_optim_tuning
#'
#' @description
#' This function combines all parameters and computes torch models to
#' parameter combination, do a validation using validation samples or
#' splitting samples using validation_split. The function returns the
#' best hyper-parameters in a list.
#'
#' @param ...
#'
#' @return A list containing the best torch optimizer and its parameters.
#'
#' @examples
#' \donttest{
#' # read a set of samples
#' data(cerrado_2classes)
#' # two fold validation with random forest
#' hparams <- sits_torch_optim_tuning(cerrado_2classes)
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
