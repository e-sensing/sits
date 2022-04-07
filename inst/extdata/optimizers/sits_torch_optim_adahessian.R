#'@title Adahessian optimizer
#'
#'@name optim_adahessian
#'
#'@author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'@author Felipe Souza, \email{lipecaso@@gmail.com}
#'@author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'@author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#'@description  R implementation of the Adahessian optimizer proposed
#' by Yao et al.(2020). The original implementation is available at
#' https://github.com/jettify/pytorch-optimizer/blob/master/torch_optimizer/adahessian.py
#'
#'
#' @references
#' Yao, Z., Gholami, A., Shen, S., Mustafa, M., Keutzer, K., & Mahoney, M. (2021).
#' ADAHESSIAN: An Adaptive Second Order Optimizer for Machine Learning.
#' Proceedings of the AAAI Conference on Artificial Intelligence, 35(12), 10665-10673.
#' https://arxiv.org/abs/2006.00719
#'
#' @param params                        Iterable of parameters to optimize.
#' @param lr                            Learning rate (default: 0.15).
#' @param betas                         Coefficients for computing
#'                                      running averages of gradient
#'                                      and is square(default: (0.9, 0.999)).
#' @param eps                           Term added to the denominator to improve
#'                                      numerical stability (default: 1e-4).
#' @param weight_decay                  L2 penalty (default: 0).
#' @param hessian_power                 Hessian power (default: 0.5).
#' @param spatial_average_block_size    Block size to perform averaging by
#'                                      dimension
#'                                      (default: (-1, -1, -1, -1).
#'
#' @note
#' spatial_average_block_size controls
#' whether to perform spatial averaging in dimension (1D, 2D, 3D, 4D)
#'  FALSE for 1D: no spatial average
#'  FALSE for 2D: use the entire row as the spatial average
#'  FALSE for 3D: assumes 1D Conv
#'  use the channel (last dimension) of 1D Conv as spatial average
#'  FALSE for 4D: assumes 2D Conv
#'  use the channel (last two dimension) of 2D Conv as spatial average
#'
#' @returns
#' An optimizer object implementing the `step` and `zero_grad` methods.
#'
#' @export
optim_adahessian <- torch::optimizer(
    classname = "optim_adahessian",
    initialize = function(
        params,
        lr = 1e-3,
        betas = c(0.9, 0.999),
        eps = 1e-8,
        weight_decay = 0,
        hessian_power = 0.5,
        spatial_average_block_size = c(-1, -1, -1, -1)
    ) {
        if (lr <= 0.0)
            rlang::abort("Learning rate must be positive.")
        if (eps < 0.0)
            rlang::abort("eps must be non-negative.")
        if (betas[1] > 1.0 | betas[1] <= 0.0)
            rlang::abort("Invalid beta parameter.")
        if (betas[2] > 1.0 | betas[1] <= 0.0)
            rlang::abort("Invalid beta parameter.")
        if (hessian_power >= 1.0 | hessian_power <= 0.0)
            rlang::abort("Invalid beta parameter.")
        if (weight_decay < 0)
            rlang::abort("Invalid weight_decay value")

        torch::torch_manual_seed(sample.int(10^5, 1))

        defaults = list(
            lr           = lr,
            betas        = betas,
            eps          = eps,
            hessian_power = hessian_power,
            weight_decay = weight_decay
        )
        super$initialize(params, defaults)
        self$spatial_average_block_size = spatial_average_block_size
    },
    #     Get an estimate of Hessian Trace.
    #     This is done by computing the Hessian vector product with a random
    #     vector v at the current gradient point, to estimate Hessian trace by
    #     computing the gradient of <gradsH,v>.
    get_trace = function(grads){
        # Check backward was called with create_graph set to True
        grad_list <- purrr::map(grads, function(grad) {
            if (purrr::is_null(grad$grad_fn)) {
                msg <- paste("Gradient tensor does not have grad_fn",
                "When calling loss.backward(), set create_graph to True.")
                rlang::abort(msg)
            }
        })
        params = self$param_groups[[1]][['params']]
        # random vector to estimate Hessian matrix diagonal
        v <- purrr::map(params, function(p){
            return(2 * torch::torch_randint_like(input = p,
                                                 low = 0,
                                                 high = 2) - 1)
        })

        # Computes the sum of gradients of outputs w.r.t. the inputs.
        hvs <- torch::autograd_grad(
            outputs = grads,
            inputs  = params,
            grad_outputs = v,
            retain_graph = TRUE
        )
        # variables controlling spatial averaging by dimension
        bs_1D <- self$spatial_average_block_size[1]
        bs_2D <- self$spatial_average_block_size[2]
        bs_3D <- self$spatial_average_block_size[3]
        bs_4D <- self$spatial_average_block_size[4]

        # calculate hutchinson_trace
        # approximation of hessian diagonal
        hutchinson_trace <- purrr::map(seq_along(hvs), function(hv_ind){
            hv <- hvs[[hv_ind]]
            param_size <-  hv$size()
            hv_abs <- hv$abs()
            if (length(param_size) <= 1) {
                # For 1D tensor, e.g.,, bias, BatchNorm, LayerNorm etc.
                # Usually, you do not need to set spatial averaging for it,
                # i.e., Hessian diagonal block size is 1 here.
                if (bs_1D == -1)
                    return(hv_abs)
                else {
                    # flatten to N * bs_1D
                    tmp_output1 <- hv_abs$view(-1, bs_1D)
                    tmp_output2 = torch::torch_mean(tmp_output1, dim = 1 )
                    tmp_output3 = tmp_output2$repeat_interleave(bs_1D)$view(param_size)
                    return(tmp_output3)
                }
            } else if (length(param_size) == 2) {
                # For 2D tensor, e.g., the matrix in the fully-connected layer.
                # This is a normal case for MLP, Transformer models.
                # Usually, a spatial averaging needs to be used here
                # to get the best result
                if (bs_2D == -1)
                    return(torch::torch_mean(hv_abs, dim = 1, keepdim = TRUE))
                else {
                    # flatten to N * bs_2D
                    tmp_output1 = hv_abs$view(-1, bs_2D)
                    tmp_output2 = torch::torch_mean(tmp_output1, dim = 1)
                    tmp_output3 = tmp_output2$repeat_interleave(bs_2D)$view(param_size)
                    return(tmp_output3)
                }
            } else if (length(param_size) == 3) {
                # # For 3D tensors, e.g., the 1D Conv layer
                if (bs_3D == -1)
                    return(torch::torch_mean(hv_abs, dim = 1, keepdim = TRUE))
                else {
                    # flatten to N * bs_3D
                    tmp_output1 = hv_abs$view(-1, bs_3D)
                    tmp_output2 = torch::torch_mean(tmp_output1, dim = 1)
                    tmp_output3 = tmp_output2$repeat_interleave(bs_3D)$view(param_size)
                    return(tmp_output3)
                }
            } else if (length(param_size) == 4) {
                # # For 4D tensors, e.g., the 2D Conv layer
                if (bs_4D == -1)
                    return(torch::torch_mean(hv_abs, dim = c(2, 3), keepdim = TRUE))
                else {
                    # flatten to N * bs_3D
                    tmp_output1 = hv_abs$view(-1, bs_4D)
                    tmp_output2 = torch::torch_mean(tmp_output1, dim = 1)
                    tmp_output3 = tmp_output2$repeat_interleave(bs_4D)$view(param_size)
                    return(tmp_output3)
                }
            } else
                rlang::abort("Only 1D to 4D tensors are supported.")
        })
        return(hutchinson_trace)
    },
    step = function(closure = NULL) {
        loop_fun <- function(group, param, g, p) {

            #  Flatten params and grads into lists
            #
            grads <- purrr::map(group[["params"]], function(ph)
                return(ph[["grad"]]))
            # Get the Hessian diagonal
            hut_traces = self$get_trace(grads)
            # state initialization
            if (length(state(param)) == 0) {
                state(param) <- list()
                state(param)[["step"]] <- 0
                state(param)[["exp_avg"]] <- torch_zeros_like(param)
                state(param)[["exp_hessian_diag_sq"]] <- torch_zeros_like(param)
            }
            exp_avg             <- state(param)[["exp_avg"]]
            exp_hessian_diag_sq <- state(param)[["exp_hessian_diag_sq"]]

            beta1 <-  group[['betas']][[1]]
            beta2 <-  group[['betas']][[2]]

            state(param)[["step"]] <- state(param)[["step"]] + 1

            # Decay the first and second moment
            # running average coefficient
            exp_avg$mul_(beta1)$add_(grad, alpha = 1 - beta1)
            exp_avg_sq$mul_(beta2)$addcmul_(hut_traces[i],
                                            hut_traces[i],
                                            value = 1 - beta2)

            # bias correction
            bias_correction1 <-  1 - beta1 ^ state(param)[['step']]
            bias_correction2 <-  1 - beta2 ^ state(param)[['step']]

            # make the square root, and the Hessian power
            k  <- group[['hessian_power']]
            denom <- ((exp_hessian_diag_sq$sqrt() ^ k) /
                          sqrt(bias_correction2) ^ k)$add_(group[['eps']])

            # make update
            param = param -
                group[['lr']] * (
                    exp_avg / bias_correction1 / denom
                    + group[['weight_decay']] * param)
        }
        private$step_helper(closure, loop_fun)
    }
)
