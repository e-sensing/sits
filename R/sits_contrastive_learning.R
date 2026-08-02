#' @title Supervised contrastive learning for time series
#' @name sits_contrastive_learning
#'
#' @description
#' Supervised contrastive (SupCon) pre-training of \code{sits} encoders,
#' based on the loss proposed by Khosla et al. (2020). The method learns
#' an embedding space in which time-series samples sharing the same label
#' are pulled together while samples with different labels are pushed
#' apart.
#'
#' For each batch, two views per sample are created by pairing every anchor
#' with a same-class sample. Both views are passed through a shared encoder
#' and projection head and are L2-normalised. The resulting \code{2B}
#' embeddings are pooled and each embedding is contrasted against every
#' other embedding (excluding itself) using temperature-scaled cosine
#' similarity, with positives defined by matching labels.
#'
#' After pre-training, the projection head is discarded and only the encoder
#' is kept for downstream use via \code{\link[sits]{sits_encode}}.
#'
#' The function can be used in two ways:
#' \itemize{
#'   \item If \code{samples} is provided, it trains immediately and returns
#'     an encoder-ready model object (see Value).
#'   \item If \code{samples = NULL}, it returns a training function with
#'     signature \code{function(samples)} that can be passed to
#'     \code{\link[sits]{sits_pre_train}} or called later.
#' }
#'
#' @param samples          A \code{sits} samples object. If \code{NULL}
#'   (default), returns a training function. If provided, triggers immediate
#'   training. Base data samples (e.g., \code{sits_base}) are not supported.
#' @param embedding_dim    Integer. Dimensionality of the encoder embedding
#'   (exported features). Default: 64L.
#' @param proj_dim         Integer. Dimensionality of the projection head output
#'   used only during pre-training (discarded afterwards). Default: 128L.
#' @param scaling      Numeric. Scaling for the contrastive
#'   loss. Lower values sharpen the similarity distribution.
#'   Default: 0.07.
#' @param num_pairs        Integer or \code{NULL}. Total number of pairs
#'   to form. When \code{NULL} (default), one pair is formed per sample.
#' @param encoder_model    Function. Deep learning method that takes time series
#' as input and produces latent representations that
#' are used to compute the loss function (suggested options:
#'   \code{\link[sits]{sits_tempcnn}()}, \code{\link[sits]{sits_lighttae}()},
#'    \code{\link[sits]{sits_resnet}()}).  Default: \code{sits_tempcnn()}.
#' @param epochs           Integer. Maximum number of training epochs.
#' @param batch_size       Integer. Batch size for training. Larger batches
#'   provide more positives/negatives per sample. Default: 128L.
#' @param validation_split Numeric in (0, 1). Fraction of samples held out
#'   for validation loss monitoring.
#' @param optimizer        Function. A \code{torch} optimizer constructor
#'   (default: \code{torch::optim_adamw}).
#' @param opt_hparams      Named list of optimizer hyperparameters.
#'   Common entries: \code{lr}, \code{eps}, \code{weight_decay}.
#' @param lr_decay_epochs  Integer. Step size (in epochs) for LR decay.
#' @param lr_decay_rate    Numeric. Multiplicative LR decay factor.
#' @param patience         Integer. Early-stopping patience (epochs without
#'   improvement).
#' @param min_delta        Numeric. Minimum improvement required to reset
#'   the patience counter.
#' @param verbose          Logical. Print training progress?
#' @param seed             Integer. Random seed for reproducibility.
#'
#' @return
#' If \code{samples = NULL}, a training function with signature
#' \code{function(samples)} that trains a supervised contrastive model and
#' returns a pretrained encoder (a \code{sits_encoder} closure).
#'
#' If \code{samples} is provided, the result of applying the training function
#' to \code{samples} directly.
#'
#' @details
#' The supervised contrastive (SupCon) loss generalises the InfoNCE/NT-Xent
#' objective by allowing multiple positives per anchor. For each of the
#' \code{2B} L2-normalised embeddings in a batch, all other embeddings that
#' share the anchor's label act as positives and the remaining embeddings act
#' as negatives. Pairwise cosine similarities are divided by the temperature
#' \code{scaling} and combined in a log-softmax form; the loss is averaged
#' over the positives of each anchor and then over the batch.
#'
#' The \code{scaling} parameter (temperature) controls the sharpness of the
#' similarity distribution. Lower temperatures sharpen it and place more
#' weight on the hardest negatives, encouraging stronger separation at the
#' cost of noisier gradients; higher temperatures soften it. The default of
#' \code{0.07} follows Khosla et al. (2020). Larger batches also help, since
#' they expose more positives and negatives per anchor and yield a stronger
#' contrastive signal.
#'
#' @references
#' Khosla, P., Teterwak, P., Wang, C., et al. (2020).
#' \emph{Supervised Contrastive Learning}. arXiv:2004.11362.
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @examples
#' if (sits_run_examples()) {
#'     model <- sits_pre_train(
#'         samples_modis_ndvi,
#'         sits_contrastive_learning(
#'             embedding_dim  = 32L,
#'             epochs         = 20L
#'         )
#'     )
#' }
#'
#' @export
sits_contrastive_learning <- function(
        samples            = NULL,
        embedding_dim      = 64L,
        proj_dim           = 128L,
        scaling            = 0.07,
        num_pairs          = NULL,
        encoder_model      = sits_tempcnn(),
        epochs             = 150L,
        batch_size         = 128L,
        validation_split   = 0.2,
        optimizer          = torch::optim_adamw,
        opt_hparams = list(
            lr           = 5.0e-04,
            eps          = 1.0e-08,
            weight_decay = 1.0e-06
        ),
        lr_decay_epochs    = 1L,
        lr_decay_rate      = 0.95,
        patience           = 20L,
        min_delta          = 0.01,
        verbose            = FALSE,
        seed               = 10L) {
    # set caller for error msg
    .check_set_caller("sits_contrastive_learning")
    # Verifies if 'torch' and 'luz' packages are installed
    .check_require_packages(c("torch", "luz"))
    # Band prefix for embeddings
    bands_prefix <- .conf("embedding_band_prefix")
    .check_chr(bands_prefix, len_min = 1, len_max = 1, allow_empty = FALSE)
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Function that trains a torch model based on samples
    train_fun <- function(samples) {
        # does not support working with DEM or other base data
        if (inherits(samples, "sits_base")) {
            stop(.conf("messages", "sits_train_base_data"), call. = FALSE)
        }
        # Avoid adding a global variable for 'self'
        self <- NULL
        # Pre-conditions
        .check_pre_sits_contrastive_learning(
            samples         = samples,
            epochs          = epochs,
            batch_size      = batch_size,
            encoder_model   = encoder_model,
            bands_prefix    = bands_prefix,
            verbose         = verbose
        )
        # Other pre-conditions
        .check_int_parameter(seed, allow_null = TRUE)
        # Build optimizer hyperparameters
        optim_params_function <- .torch_optim_params(optimizer, opt_hparams)
        # Samples metadata
        labels   <- .samples_labels(samples)
        bands    <- .samples_bands(samples)
        timeline <- .samples_timeline(samples)
        n_labels <- length(labels)
        n_bands  <- length(bands)
        n_times  <- .samples_ntimes(samples)

        # Copy closure variables to local
        embedding_dim <- embedding_dim
        proj_dim      <- proj_dim
        scaling       <- scaling
        bands_prefix  <- bands_prefix

        # ------------------------------------------------------------------
        # Build view-pairs for contrastive training
        #
        # Each dataset item is a tensor of shape [2, n_times, n_bands]
        # plus an integer label for the anchor.
        # ------------------------------------------------------------------
        ml_stats <- .samples_stats(samples)
        pairs <- .contrastive_learning_data_split(
            samples          = samples,
            validation_split = validation_split,
            num_pairs        = num_pairs
        )

        # Torch datasets
        train_ds <- .contrastive_learning_dataset(pairs[["train"]],
                                                n_times = n_times)
        val_ds   <- .contrastive_learning_dataset(pairs[["val"]],
                                                n_times = n_times)

        # Dummy data used only to register the luz module structure
        stub_data <- .ssl_stub_data(samples, ml_stats, n_times, n_bands)
        # Set torch seed
        torch_seed <- .torch_set_seed(seed)

        # Set the encoder model closure
        encoder <- encoder_model(
            samples       = samples,
            embedding_dim = embedding_dim
        )
        # ------------------------------------------------------------------
        # Define contrastive model: encoder + projection head
        #   encoder → MLP head (Linear → ReLU → Linear) → L2-normalize
        # ------------------------------------------------------------------
        self  <- NULL
        super <- NULL

        contrastive_model <- torch::nn_module(
            classname = "contrastive_model",
            initialize = function(encoder, embedding_dim, proj_dim = 128L,
                                  n_bands = NULL, n_labels = NULL,
                                  timeline = NULL) {
                super$initialize()
                self$encoder <- encoder

                # MLP projection head
                self$head <- torch::nn_sequential(
                    torch::nn_linear(embedding_dim, embedding_dim),
                    torch::nn_relu(),
                    torch::nn_linear(embedding_dim, proj_dim)
                )

                self$n_bands  <- n_bands
                self$n_labels <- n_labels
                self$timeline <- timeline
                self$n_times  <- length(timeline)
            },
            forward = function(x) {
                # x: [batch, 2, time, band]
                # Encode both views through
                # shared encoder + head, L2-normalize
                z_a <- x[, 1, , ]$contiguous() |>
                    self$encoder() |>
                    self$head() |>
                    torch::nnf_normalize(p = 2, dim = 2)

                z_b <- x[, 2, , ]$contiguous() |>
                    self$encoder() |>
                    self$head() |>
                    torch::nnf_normalize(p = 2, dim = 2)

                # Output: [batch, 2, proj_dim]
                torch::torch_stack(list(z_a, z_b), dim = 2)
            }
        )
        #
        # Use SupCon contrastive loss
        #
        contrastive_loss <- function(input, target){
            .contrastive_learning_loss(input, target, scaling)
        }

        # Verify if GPU is available
        cpu_train <- .torch_cpu_train()

        # Train the model using luz
        model <-
            luz::setup(
                module    = contrastive_model,
                loss      = contrastive_loss,
                metrics   = list(),
                optimizer = optimizer
            ) |>
            luz::set_hparams(
                encoder       = encoder,
                embedding_dim = embedding_dim,
                proj_dim      = proj_dim,
                n_bands       = n_bands,
                timeline      = timeline,
                n_labels      = n_labels
            ) |>
            luz::set_opt_hparams(
                !!!optim_params_function
            ) |>
            luz::fit(
                data       = train_ds,
                epochs     = epochs,
                valid_data = val_ds,
                callbacks  = .ssl_callbacks(
                    patience, min_delta, lr_decay_epochs, lr_decay_rate,
                    has_validation = TRUE
                ),
                accelerator        = luz::accelerator(cpu = cpu_train),
                dataloader_options = list(
                    batch_size = batch_size,
                    shuffle    = TRUE
                ),
                verbose = verbose
            )

        # Wrap the encoder in a luz stub for sits_encode() compatibility.
        # The projection head is discarded — standard practice.
        torch_model <- .ssl_wrap_encoder(
            model     = model,
            optimizer = optimizer,
            n_bands   = n_bands,
            n_labels  = n_labels,
            timeline  = timeline,
            stub_data = stub_data
        )
        # Serialize model for later deserialization inside predict_fun
        serialized_model <- .torch_serialize_model(torch_model[["model"]])
        # Function that predicts labels of input values
        predict_fun <- function(values) {
            # Verifies if torch package is installed
            .check_require_packages("torch")
            # Set torch threads to 1
            suppressWarnings(torch::torch_set_num_threads(1L))
            # Unserialize model
            torch_model$model <- .torch_unserialize_model(
                model = torch_model$model,
                raw = serialized_model
            )
            # Use dataloader?
            if (.torch_use_dataloader(values)) {
                # Encode!
                values <- .torch_predict_chunks(
                    torch_model = torch_model,
                    dataset = values[["dataset"]],
                    callback = values[["callback"]]
                )
            } else {
                # Transform input into a 3D tensor
                n_samples <- nrow(values)
                n_times <- .samples_ntimes(samples)
                n_bands <- length(bands)
                # Performs data normalization on CPU
                values <- .pred_features_normalize(pred = values, stats = ml_stats)
                values <- C_as_array_inplace(
                    x = as.matrix(values),
                    dim = c(n_samples, n_times, n_bands)
                )
                # CPU classification
                values <- stats::predict(
                    object = torch_model,
                    newdata = values,
                    accelerator = luz::accelerator(cpu = TRUE)
                )
                # Convert from tensor to array
                values <- torch::as_array(values)
                # Update the columns names to labels
                colnames(values) <- paste0(bands_prefix, seq_len(ncol(values)))
            }
            # Return!
            values
        }
        # Tag with sits model classes
        predict_fun <- .set_class(
            predict_fun, "sits_encoder", "torch_model", "sits_model", class(predict_fun)
        )
    }
    # If samples is provided, train immediately; otherwise return train_fun
    .factory_function(samples, train_fun)
}
