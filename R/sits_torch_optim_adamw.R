#'@title AdamW optimizer
#'
#'@name optim_adamw
#'
#'@author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'@author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'@author Felipe Souza, \email{lipecaso@@gmail.com}
#'@author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#'@description  R implementation of the AdamW optimizer proposed
#' by Loshchilov & Hutter (2019). We used the PyTorch implementation
#' developed by Collin Donahue-Oponski available at:
#' https://gist.github.com/colllin/0b146b154c4351f9a40f741a28bff1e3
#'
#'
#'
#' From the abstract by the paper by Loshchilov & Hutter (2019):
#' L2 regularization and weight decay regularization are equivalent for standard
#' stochastic gradient descent (when rescaled by the learning rate),
#' but as we demonstrate this is not the case for adaptive gradient algorithms,
#' such as Adam. While common implementations of these algorithms
#' employ L2 regularization (often calling it “weight decay”
#' in what may be misleading due to the inequivalence we expose),
#' we propose a simple modification to recover the original formulation of
#' weight decay regularization by decoupling the weight decay from the optimization
#' steps taken w.r.t. the loss function
#'
#' @references
#' Ilya Loshchilov, Frank Hutter,
#' "Decoupled Weight Decay Regularization",
#' International Conference on Learning Representations (ICLR) 2019.
#' https://arxiv.org/abs/1711.05101
#'
#' @param params         List of parameters to optimize.
#' @param lr             Learning rate (default: 1e-3)
#' @param betas          Coefficients computing running averages of gradient
#'                       and its square (default: (0.9, 0.999))
#' @param eps            Term added to the denominator to improve numerical stability
#'                       (default: 1e-8)
#' @param weight_decay   Weight decay (L2 penalty) (default: 1e-6)
#'
#' @returns
#' An optimizer object implementing the `step` and `zero_grad` methods.
#'
#' @export
optim_adamw <- torch::optimizer(
    classname = "optim_adamw",
    initialize = function(
        params,
        lr                  = 0.01,
        betas               = c(0.9, 0.999),
        eps                 = 0.001,
        weight_decay        = 1e-6
    ) {
        if (lr <= 0.0)
            rlang::abort("Learning rate must be positive.")
        if (eps < 0.0)
            rlang::abort("eps must be non-negative.")
        if (betas[1] > 1.0 | betas[1] <= 0.0)
            rlang::abort("Invalid beta parameter.")
        if (betas[2] > 1.0 | betas[1] <= 0.0)
            rlang::abort("Invalid beta parameter.")
        if (weight_decay < 0)
            rlang::abort("Invalid weight_decay value")

        defaults = list(
            lr                  = lr,
            betas               = betas,
            eps                 = eps,
            weight_decay        = weight_decay
        )
        super$initialize(params, defaults)
    },
    step = function(closure = NULL){
        loop_fun <- function(group, param, g, p) {
            if (purrr::is_null(param$grad))
                next
            grad <- param$grad


            # State initialization
            if (length(state(param)) == 0) {
                state(param) <- list()
                state(param)[["step"]] <- 0
                # Exponential moving average of gradient values
                state(param)[["exp_avg"]] <- torch::torch_zeros_like(param)
                # Exponential moving average of squared gradient values
                state(param)[["exp_avg_sq"]] <- torch::torch_zeros_like(param)
            }
            # Define variables for optimization function
            exp_avg    <-  state(param)[["exp_avg"]]
            exp_avg_sq <-  state(param)[["exp_avg_sq"]]

            beta1        <-  group[['betas']][[1]]
            beta2        <-  group[['betas']][[2]]
            weight_decay <-  group[['weight_decay']]
            eps          <-  group[["eps"]]
            lr           <-  group[['lr']]

            # take one step
            state(param)[["step"]] <- state(param)[["step"]] + 1

            # Decay the first moment
            exp_avg$mul_(beta1)$add_(grad, alpha = 1 - beta1)
            # Decay the second moment
            exp_avg_sq$mul_(beta2)$addcmul_(grad, grad, value = (1 - beta2))

            # calculate denominator
            denom = exp_avg_sq$sqrt()$add_(eps)

            # bias correction
            bias_correction1 <-  1 - beta1 ^ state(param)[['step']]
            bias_correction2 <-  1 - beta2 ^ state(param)[['step']]
            # calculate step size
            step_size <- lr * sqrt(bias_correction2) / bias_correction1

            # L2 correction (different from adam)
            if (weight_decay != 0)
                param$add_(-weight_decay * lr)
            # go to next step
            param$addcdiv_(exp_avg, denom, value = -step_size)
        }
        private$step_helper(closure, loop_fun)
    }
)
