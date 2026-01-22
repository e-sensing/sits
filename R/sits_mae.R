#' @title Pre-train a Masked Autoencoder on SITS time-series data
#'
#' @name sits_mae
#'
#' @author Alexandre Assuncao \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' `sits_mae()` returns a training-factory function for a Masked Autoencoder (MAE)
#' that you can pass to [sits_pre_train()].  The resulting closure takes a SITS
#' `samples` object, runs a self-supervised reconstruction loop over masked time-steps,
#' and returns the pretrained encoder.
#'
#' @param encoder_model  Character. Which encoder backbone to use. Supported values:
#'                       `"lighttae"` (temporal attention encoder) or
#'                       `"tempcnn"` (1D convolutional).
#' @param decoder_model  Character. Which decoder head to use. Currently only `"mlp"`.
#' @param masking_method Character. How to select masked positions. One of
#'                       `"random"` or `"contiguous"`.
#' @param mask_ratio     Numeric in (0,1). Fraction of time-steps to mask.
#' @param epochs         Integer. Number of training epochs.
#' @param batch_size     Integer. Batch size for both training and validation.
#' @param validation_split Numeric in (0,1). Fraction of samples held out for validation.
#' @param optimizer_fn   Function. A `torch` optimizer constructor (e.g. `torch::optim_adamw`).
#' @param loss_fn        Function. A `torch` loss function (e.g. `torch::nnf_mse_loss`).
#' @param device         `torch::device` or string. Where to perform training
#'                       (e.g. `"cpu"` or `torch::cuda_device()`).
#' @param lr             Numeric. Initial learning rate for the optimizer.
#' @param verbose        Logical. If `TRUE`, print per-epoch training/validation losses.
#' @param output_dir     Character. Optional. Path to save model log.
#'
#' @return
#' A function with signature `function(samples)` which, when called on a SITS
#' samples object, trains a masked autoencoder and returns the pretrained encoder
#' (as a `torch` module).
#'
#' @references
#' He, K., Chen, X., Xie, S., Li, Y., Dollár, P., & Girshick, R. (2022).
#' *Masked Autoencoders Are Scalable Vision Learners*. Proceedings of the
#' IEEE/CVF Conference on Computer Vision and Pattern Recognition (CVPR).
#'
#' @export

sits_mae <- function(samples            = NULL,
                     encoder_model      = "lighttae",
                     decoder_model      = "mlp",
                     masking_method     = "contiguous",
                     mask_ratio         = 0.6,
                     epochs             = 150L,
                     batch_size         = 128L,
                     validation_split   = 0.2,
                     optimizer_fn       = torch::optim_adamw,
                     loss_fn            = torch::nnf_mse_loss,
                     device             = if (torch::cuda_is_available()) torch::torch_device("cuda") else torch::torch_device("cpu"),
                     lr                 = 0.001,
                     bands_prefix       = "EMB", # TODO Add to docstring
                     verbose            = FALSE,
                     output_dir         = NULL,
                     seed               = 10L) {
    # set caller for error msg
    .check_set_caller("sits_mae")
    # Verifies if 'torch' and 'luz' packages is installed
    .check_require_packages(c("torch", "luz", "coro"))
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid add a global variable for 'self'
        self <- NULL
        # Pre-conditions
        .check_pre_sits_mae(samples, epochs, batch_size,
                            encoder_model, decoder_model,
                            masking_method, mask_ratio,
                            bands_prefix, verbose)

        # Other pre-conditions:
        .check_int_parameter(seed, allow_null = TRUE)
        # Samples labels
        labels <- .samples_labels(samples)
        # Samples bands
        bands <- .samples_bands(samples)
        # Samples timeline
        timeline <- .samples_timeline(samples)
        # Number of labels, bands, and number of samples (used below)
        n_labels <- length(labels)
        n_bands  <- length(bands)
        n_times  <- .samples_ntimes(samples)
        train_samples <- .predictors(samples)
        # -------------
        # Process samples for mae training
        # ------------
        ml_stats <- .samples_stats(samples)
        mae_data <- .mask_data_split_train_val(samples, mask_ratio, masking_method, validation_split)

        # Get stats for denormalization during reconstruction
        q02_band <- mae_data$q02[seq(1, length(mae_data$q02), by = n_times)]
        q98_band <- mae_data$q98[seq(1, length(mae_data$q98), by = n_times)]

        # Torch dataset and dataloaders
        train_ds <- .MaskedAETimeseriesDataset(mae_data$train_x, mae_data$train_y, mae_data$train_mask)
        train_dl <- torch::dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)
        val_ds   <- .MaskedAETimeseriesDataset(mae_data$val_x, mae_data$val_y, mae_data$val_mask)
        val_dl   <- torch::dataloader(val_ds, batch_size = batch_size)

        torch_seed <- .torch_seed(seed)
        torch::torch_manual_seed(torch_seed)

        # -------------
        # Set the encoder model closure
        # ------------
        encoder_fn <- switch(encoder_model,
                             "lighttae"         = .sits_mae_encoder_lighttae,
                             "mlp"              = .sits_mae_encoder_mlp,
                             "tempcnn"          = .sits_mae_encoder_tempcnn,
                             stop("`", encoder_model, "` is not a supported encoder. See ?sits_mae")
        )
        encoder <- encoder_fn(
            samples  = samples,
            n_bands  = n_bands,
            timeline = timeline
        )

        # -------------
        # Set the decoder model closure
        # ------------
        decoder_fn <- switch(decoder_model,
                             "mlp"    = .sits_mae_decoder_mlp,
                             "linear" = .sits_mae_decoder_linear,
                             stop("`", decoder_model, "` is not a supported decoder. See ?sits_mae")
        )
        decoder <- decoder_fn(
            embedding_dim = encoder$embedding_dim,
            n_times = n_times,
            n_bands = n_bands
        )

        # -------------
        # Define full MAE model
        # ------------
        self  <- NULL
        super <- NULL
        MAE_model <- torch::nn_module(
            classname = "MAE_model",

            initialize = function(encoder, decoder) {
                super$initialize()
                self$encoder <- encoder
                self$decoder <- decoder
            },

            forward = function(x) {
                x <- self$encoder(x)
                x <- self$decoder(x)
                x <- torch::nnf_sigmoid(x)
                x
            }
        )

        model <- MAE_model(
            encoder = encoder,
            decoder = decoder)
        embedding_dim <- encoder$embedding_dim
        model <- model$to(device = device)
        optim <- optimizer_fn(params = model$parameters, lr = lr)

        # -------------
        # Setup model log
        # ------------
        model_log <- list(
            epoch           = integer(),
            train_loss      = numeric(),
            val_loss        = numeric(),
            originals       = list(),    # 1st sample’s x each epoch
            reconstructions = list()     # 1st sample’s y_pred each epoch
        )

        # -------------
        # THE TRAINING LOOP
        # ------------
        for (epoch in seq_len(epochs)) {
            model$train()
            total_loss <- 0
            batch_count <- 0

            coro::loop(for (batch in train_dl) {
                optim$zero_grad()
                x      <- batch$x$to(device = device)
                y_true <- batch$y$to(device = device)
                mask   <- batch$mask$to(device = device)

                # forward
                y_pred <- model(x)
                loss   <- loss_fn(y_pred * mask, y_true * mask, reduction = "sum") / mask$sum()

                # backward
                loss$backward()
                optim$step()

                total_loss  <- total_loss + loss$item()
                batch_count <- batch_count + 1
            })

            avg_train_loss <- total_loss / batch_count

            # -- Validation
            model$eval()
            x0          <- NULL # For reconstructrion logging
            y0_pred     <- NULL # For reconstructrion logging
            val_loss    <- 0
            val_batches <- 0

            coro::loop(for (batch in val_dl) {
                x_val    <- batch$x$to(device = device)
                mask_val <- batch$mask$to(device = device)
                y_val    <- batch$y$to(device = device)

                # Saves original and reconstructed x (one per epoch)
                if (val_batches == 0) {
                    torch::with_no_grad({
                        x0      <- batch$x$to(device = "cpu")[1,,, drop = FALSE]
                        y0_true <- batch$y$to(device = "cpu")[1,,, drop = FALSE]
                        y0_pred <- model(x0$to(device = device))$to(device = "cpu")
                        mask0   <- batch$mask$to(device = "cpu")
                    })
                }

                torch::with_no_grad({
                    y_pred_val <- model(x_val)
                    l          <- loss_fn(y_pred_val * mask_val, y_val * mask_val, reduction = "sum") / mask_val$sum()
                })

                val_loss    <- val_loss + l$item()
                val_batches <- val_batches + 1
            })

            avg_val_loss <- val_loss / val_batches

            #
            # -- Logging
            #
            x0_arr      <- as.array(y0_true)   # [1 × time × bands]
            y0_pred_arr <- as.array(y0_pred)   # [1 × time × bands]

            # Sanity check: one quantile per band
            stopifnot(length(q02_band) == dim(x0_arr)[3])
            stopifnot(length(q98_band) == dim(x0_arr)[3])

            # Scale by (q98 - q02) per band
            x0_scaled     <- sweep(x0_arr,      3, q98_band - q02_band, `*`)
            y0_scaled_pred<- sweep(y0_pred_arr, 3, q98_band - q02_band, `*`)

            # Shift by q02 per band
            x0_un <- sweep(x0_scaled,      3, q02_band, `+`)
            y0_un <- sweep(y0_scaled_pred, 3, q02_band, `+`)

            # Store in log
            #model_log$originals[[epoch]]       <- x0_un
            #model_log$reconstructions[[epoch]] <- y0_un
            #model_log$epoch         <- c(model_log$epoch,      epoch)
            #model_log$mask[[epoch]] <- as.array(mask0[1,,])
            model_log$train_loss    <- c(model_log$train_loss, avg_train_loss)
            model_log$val_loss      <- c(model_log$val_loss,   avg_val_loss)

            # Monitoring
            if (verbose) {
                cat(sprintf(
                    "Epoch %3d/%d — train_loss: %.4f — val_loss: %.4f\n",
                    epoch, epochs, avg_train_loss, avg_val_loss
                ))
            }
        }
        #
        # Wrap in a luz stub so predict_fun() dispatches correctly
        #
        cpu_mod <- model$encoder$to(device = "cpu")
        # 2) Define a trivial nn_module *generator*
        stub_module <- torch::nn_module(
            "StubModule",
            initialize = function(n_bands, n_labels, timeline, ...) {
                # stash trained module
                self$model <- cpu_mod
            },
            forward = function(x) {
                self$model(x)
            }
        )
        torch_model <- luz::setup(
            module    = stub_module,
            loss      = torch::nn_cross_entropy_loss(),
            optimizer = function(params) optimizer_fn(params, lr = lr)
        ) |>
            # Set hyperparams
            luz::set_hparams(
                n_bands  = n_bands,
                n_labels = n_labels,
                timeline = timeline
            ) |>
            # zero epochs -- just registers module
            luz::fit(
                data    = list(mae_data$train_x, mae_data$train_y),
                epochs  = 0L,
                verbose = FALSE
            )

        # Grab the pure state‐dict from cpu_mod
        cpu_sd <- cpu_mod$state_dict()
        # Add "model." prefix to every name
        names(cpu_sd) <- paste0("model.", names(cpu_sd))
        # Inject the real weights back into the luz model
        torch_model[["model"]]$load_state_dict(cpu_sd)


        # Serialize model
        serialized_model <- .torch_serialize_model(torch_model[["model"]])

        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            suppressWarnings(torch::torch_set_num_threads(1L))
            # Unserialize model
            torch_model[["model"]] <- .torch_unserialize_model(serialized_model)
            # Transform input into a 3D tensor
            # Reshape the 2D matrix into a 3D array
            n_samples <- nrow(values)
            n_times <- .samples_ntimes(samples)
            n_bands <- length(bands)
            # Performs data normalization
            values <- .pred_normalize(pred = values, stats = ml_stats)
            # Represent matrix values as array
            values <- array(
                data = as.matrix(values), dim = c(n_samples, n_times, n_bands)
            )
            # GPU or CPU classification?
            if (.torch_gpu_classification()) {
                # Get batch size
                batch_size <- sits_env[["batch_size"]]
                # Transform the input array to a dataset
                values <- .torch_as_dataset(values)
                # Transform to dataloader to use the batch size
                values <- torch::dataloader(values, batch_size = batch_size)
                # Do GPU classification
                values <- .try(
                    stats::predict(object = torch_model, values),
                    .msg_error = .conf("messages", ".check_gpu_memory_size")
                )
            } else {
                # Do CPU classification
                values <- stats::predict(object = torch_model, values)
            }
            # Convert from tensor to array
            values <- torch::as_array(values)
            # Update the columns names to labels
            colnames(values) <- paste0(bands_prefix, "_", seq_len(ncol(values)))
            values
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "torch_model", "sits_encoder", class(predict_fun)
        )
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    .factory_function(samples, train_fun)
}
