#' A Momentumized, Adaptive, Dual Averaged Gradient Method for Stochastic
#' Optimization.
#'
#' @author Daniel Falbel, \email{dfalbel@@gmail.com}
#'
#' [MADGRAD](https://arxiv.org/abs/2101.11075) is a general purpose optimizer that
#' can be used in place of SGD or Adam may converge faster and generalize better.
#' Currently GPU-only. Typically, the same learning rate schedule that is used
#' for SGD or Adam may be used. The overall learning rate is not comparable to
#' either method and should be determined by a hyper-parameter sweep.
#'
#' MADGRAD requires less weight decay than other methods, often as little as
#' zero. Momentum values used for SGD or Adam's beta1 should work here also.
#'
#' On sparse problems both weight_decay and momentum should be set to 0.
#' (not yet supported in the R implementation).
#'
#' This code has been lifted from the "madgrad" R package developed by
#' Daniel Falbel, licensed as an MIT licence work.
#'
#' @param params        List of parameters to optimize.
#' @param lr            Learning rate (default: 1e-2).
#' @param momentum      Momentum value in  the range [0,1) (default: 0.9).
#' @param weight_decay  Weight decay, i.e. a L2 penalty (default: 0).
#' @param eps           Term added to the denominator outside of
#'                      the root operation to improve numerical stability
#'                      (default: 1e-6).
#'
#' @examples
#' if (torch::torch_is_installed()) {
#' library(torch)
#' x <- torch_randn(1, requires_grad = TRUE)
#' opt <- optim_madgrad(x)
#' for (i in 1:100) {
#'   opt$zero_grad()
#'   y <- x^2
#'   y$backward()
#'   opt$step()
#' }
#' all.equal(x$item(), 0, tolerance = 1e-9)
#' }
#'
#' @returns
#' An optimizer object implementing the `step` and `zero_grad` methods.
#'
#' @export
optim_madgrad <- torch::optimizer(
    initialize = function(params,
                          lr = 1e-2,
                          momentum = 0.9,
                          weight_decay = 0,
                          eps = 1e-6) {

        if (momentum < 0 || momentum >= 1)
            rlang::abort("Momentum must be in the range [0,1].")

        if (lr <= 0)
            rlang::abort("Learning rate must be positive.")

        if (weight_decay < 0)
            rlang::abort("Weight decay must be non-negative.")

        if (eps < 0)
            rlang::abort("Eps must be non-negative.")

        defaults <- list(lr = lr,
                         eps = eps,
                         momentum = momentum,
                         weight_decay = weight_decay)

        super$initialize(params, defaults)
    },
    step = function(closure = NULL) {
        if (is.null(self$k))
            self$k <- 0
        loss <- super$step_helper(
            closure = closure,
            loop_fun = function(group, param, ...) {
                eps <- group$eps
                lr <- group$lr + eps
                decay <- group$weight_decay
                momentum <- group$momentum

                ck <- 1 - momentum
                lamb <- lr * (self$k + 1)^0.5

                grad <- param$grad

                if (is.null(state(param))) {
                    state(param) <- list()
                    state(param)[["grad_sum_sq"]] <- torch::torch_zeros_like(param)$detach()
                    state(param)[["s"]] <- torch::torch_zeros_like(param)$detach()
                    if (momentum != 0)
                        state(param)[["x0"]] <- param$clone()
                }

                if (decay != 0) {
                    grad$add_(param, alpha = decay)
                }

                if (momentum == 0) {
                    # Compute x_0 from other known quantities
                    rms <- state(param)[["grad_sum_sq"]]$pow(1 / 3)$add_(eps)
                    x0 <- param$addcdiv(state(param)[["s"]], rms, value = 1)
                } else {
                    x0 <- state(param)[["x0"]]
                }

                # Accumulate second moments
                state(param)[["grad_sum_sq"]]$addcmul_(grad, grad, value = lamb)
                rms <- state(param)[["grad_sum_sq"]]$pow(1 / 3)$add_(eps)

                # Update s
                state(param)[["s"]]$add_(grad, alpha = lamb)

                # Step
                if (momentum == 0) {
                    p$copy_(x0$addcdiv(state(param)[["s"]], rms, value = -1))
                } else {
                    z <- x0$addcdiv(state(param)[["s"]], rms, value = -1)
                }

                # p is a moving average of z
                param$mul_(1 - ck)$add_(z, alpha = ck)

            })
        self$k <- self$k + 1
        loss
    }
)


state <- function(self) {
    attr(self, "state")
}

`state<-` <- function(self, value) {
    attr(self, "state") <- value
    self
}
