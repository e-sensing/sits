
#' @title Torch predict wrapper for sits models
#' @name .torch_model_wrap
#' @keywords internal
#' @noRd
#' @description Wraps a fitted torch model to enhance GPU operations.
#'
#' @param model      Trained torch module.
#' @param batch_size Maximum number of rows per forward pass.
#'
#' @return A torch module.
.torch_model_wrap <- torch::nn_module(
    ".torch_model_wrap",
    initialize = function(model, batch_size) {
        self$model <- model
        self$batch_size <- batch_size
    },
    # luz calls the `predict` method of a module when it defines one, and
    # its `forward` method otherwise (see luz:::predict.luz_module_fitted).
    # https://github.com/mlverse/luz/blob/e148679b42e16c1ed03a4c4a0c8f3358e37d8247/R/module.R#L383
    # Resolve the same function the unwrapped module would have used
    model_fn = function() {
        if (is.null(self$model$predict)) {
            return(self$model)
        }

        self$model$predict
    },
    # Run the model over row slices of at most `batch_size` rows
    forward_batched = function(model_fn, values, batch_size) {
        # Get number of rows of the input values
        n_rows <- values$shape[[1L]]
        # Define the start values of each slice
        slices_start <- seq.int(1L, n_rows, by = batch_size)
        # Predict each slice
        outputs <- purrr::map(slices_start, function(start) {
            # Slices are views over the input, so no data is copied here
            model_fn(values$narrow(
                dim = 1L,
                start = start,
                length = min(batch_size, n_rows - start + 1L)
            ))
        })
        # Concatenate list elements into one tensor
        torch::torch_cat(outputs, dim = 1L)
    },
    forward_wrapped = function(model_fn, values) {
        # In case where all the values are NA
        if (as.character(values$dtype) == "Bool") {
            return(NA)
        }
        # Inputs that already fit go directly to the model
        if (values$shape[[1L]] <= self$batch_size) {
            return(model_fn(values))
        }
        # Get user defined batch size
        batch_size <- self$batch_size
        # Get minimum reasonable pre-defined batch size
        min_batch_size <- .conf("torch_min_batch_size")
        # Define output value
        output <- NULL
        while (is.null(output)) {
            # Try to predict the input values
            output <- .try(
                expr = {
                    self$forward_batched(model_fn, values, batch_size)
                },
                .default = function(e) {
                    # Is not out of memory error
                    is_error_oom <- .torch_error_is_oom(e)
                    # Is batch size smaller or equal than the minimum required
                    is_batch_smaller <- batch_size <= min_batch_size
                    # Verify if the error can be fixed
                    if (!is_error_oom || is_batch_smaller) {
                        stop(e)
                    }
                    # Otherwise flag operation to reduce the batch size
                    NULL
                }
            )
            # If the output is null try a smaller batch size
            if (is.null(output)) {
                # In this case, to help users, we split the batch size
                batch_size <- max(batch_size %/% 2L, min_batch_size)
                # Warning users the batch size was split
                warning(
                    .conf("messages", ".torch_module_bounded"), batch_size,
                    call. = FALSE
                )
                # Clean torch allocations
                if (.torch_cuda_enabled()) {
                    torch::cuda_empty_cache()
                }
            }
        }
        # Return!
        output
    },
    predict = function(values) {
        self$forward_wrapped(self$model_fn(), values)
    }
)

#' @title Verify if the error was caused by GPU memory overflow
#' @name .torch_error_is_oom
#' @keywords internal
#' @noRd
#' @description Identifies out-of-memory error raised by torch.
#'
#' @param error Condition error object
#'
#' @return TRUE/FALSE
.torch_error_is_oom <- function(error) {
    grepl("out of memory", conditionMessage(error), ignore.case = TRUE)
}

#' @title Predict chunks in GPU
#' @name .torch_predict_chunks
#' @keywords internal
#' @noRd
#'
#' @param torch_model Torch model.
#' @param dataset     Torch dataset.
#' @param callback    Post-processing callback.
#'
#' @return A character vector with files.
.torch_predict_chunks <- function(torch_model, dataset, callback) {
    # We set to 0 to reproduce the same behaviour in sits
    # Once mirai starts with 0 in torch
    if (sits_env[["multicores"]] == 1) {
        multicores <- 0
    } else {
        multicores <- sits_env[["multicores"]]
    }
    # Wrap model with custom torch module which enhances GPU handling
    torch_model[["model"]] <- .torch_model_wrap(
        model = torch_model[["model"]],
        batch_size = sits_env[["batch_size"]]
    )
    # Define dataloader
    dataloader <- torch::dataloader(
        dataset = dataset,
        num_workers = multicores,
        batch_size = 1L
    )
    # Predict!
    stats::predict(
        object = torch_model,
        newdata = dataloader,
        callbacks = list(callback),
        stack = FALSE
    )
}

#' @title Create a torch dataset
#' @name .torch_chunk_dataset
#' @keywords internal
#' @noRd
#'
#' @param chunks            A tibble with the chunks positions.
#' @param tile              Single tile of a data cube.
#' @param read_fn           A function for read the time series.
#' @param bands             Bands to extract time series
#' @param base_bands        Base bands to extract values
#' @param stats             Model stats
#' @param ml_features_name  Features used to build the model
#' @param ml_temporal_model Boolean value which identifies temporal models
#' @param impute_fn         Imputation function.
#' @param verbose           Display logs?
#' @param output_dir        Character with output directory.
#'
#' @return A torch dataset
.torch_chunks_dataset <- torch::dataset(
    "dataset",
    initialize = function(chunks = NULL,
                          tile = NULL,
                          read_fn = NULL,
                          bands = NULL,
                          base_bands = NULL,
                          stats = NULL,
                          ml_features_name = NULL,
                          ml_temporal_model = NULL,
                          impute_fn = NULL,
                          output_dir = NULL,
                          verbose = FALSE) {
        self$chunks <- chunks
        self$tile <- tile
        self$read_fn <- read_fn
        self$bands <- bands
        self$base_bands <- base_bands
        self$ml_features_name <- ml_features_name
        self$ml_temporal_model <- ml_temporal_model
        self$impute_fn <- impute_fn
        self$n_bands <- length(.tile_bands(tile))
        self$n_times <- length(.tile_timeline(tile))
        self$output_dir <- output_dir
        self$verbose <- verbose

        # Pre-compute normalization tensors per batch
        if (!is.null(stats)) {
            # Compute min and max from stats
            min <- as.numeric(.stats_q02(stats))
            max <- as.numeric(.stats_q98(stats))
            # Create tensors for min and range
            self$min <- min
            self$range <- max - min
        } else {
            self$min <- NULL
            self$range <- NULL
        }
    },
    .normalize_data = function(values) {
        # Normalize on the batch tensor
        if (!is.null(self$min)) {
            values <- torch::torch_clamp(
                ((values - self$min) / self$range), min = 0.0001, max = 1.0
            )
        }
        values
    },
    .log = function() {
        if (.has(self$verbose) && self$verbose) {
            .debug(flag = TRUE, output_dir = self$output_dir)
        }
    },
    .getbatch = function(i) {
        # Display log?
        self$.log()
        # Get chunk
        chunk <- self$chunks[i, ]
        # Retrieve block to be processed
        block <- .block(chunk)
        # Log start of chunk group read
        .debug_log(
            event = "start_chunk_group_read",
            key = "block_size",
            value = c(block[["nrows"]], block[["ncols"]])
        )
        # Read and preprocess values from files.
        values <- self$read_fn(
            tile = self$tile,
            block = block,
            bands = self$bands,
            base_bands = self$base_bands,
            ml_features_name = self$ml_features_name,
            impute_fn = self$impute_fn
        )
        # Log end of chunk group read
        .debug_log(
            event = "end_chunk_group_read",
            key = "block_size",
            value = c(nrow(values), ncol(values))
        )
        # Log start chunk group prepare
        .debug_log(
            event = "start_chunk_group_prepare",
            key = "block_size",
            value = c(nrow(values), ncol(values))
        )
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Filter out NA pixels - only classify valid pixels
        values <- values[!na_mask, , drop = FALSE]
        # Transform into matrix
        values <- as.matrix(.pred_features(values))
        # Values as tensor
        values <- torch::torch_tensor(values)
        # Normalize values
        if (!is.null(self$min)) {
            values <- self$.normalize_data(values)
        }
        # Organize either temporal or features based layout tensor
        if (self$ml_temporal_model) {
            # The reshape + permute reproduces the layout:
            # array(values, dim = c(batch, n_times, n_bands))
            values <- values$reshape(
                c(values$shape[[1L]], self$n_bands, self$n_times)
            )$permute(c(1L, 3L, 2L))$contiguous()
        } else {
            # The reshape + permute reproduces the layout:
            # array(values, dim = c(batch, n_times*n_bands))
            values <- values$reshape(
                c(values$shape[[1L]], self$n_bands * self$n_times)
            )$permute(c(1L, 2L))$contiguous()
        }
        # Generate block as a vector
        block <- c(
            unlist(block),
            xmin = .xmin(chunk),
            xmax = .xmax(chunk),
            ymin = .ymin(chunk),
            ymax = .ymax(chunk)
        )
        # Log end chunk group prepare
        .debug_log(
            event = "end_chunk_group_prepare",
            key = "block_size",
            value = dim(values)
        )
        # Return input for the model and auxiliary values
        # for the callback
        list(
            input = values,
            na_mask = na_mask,
            block = block
        )
    },
    .getitem = function(i) {
        self$.getbatch(i)
    },
    .length = function() {
        nrow(self$chunks)
    }
)

#' @title Create a torch callback for encoder
#' @name .callback_post_encode
#' @keywords internal
#' @noRd
#'
#' @param output_dir A character with output directory.
#' @param out_bands  A character vector with output bands.
#' @param out_files  A character vector with output files.
#' @param crs        A character with tile crs.
#' @param emb_dims   A numeric vector with embeddings dimensions.
#' @param emb_names  A character vector with embeddings names.
#'
#' @return A torch callback
.callback_post_encode <- function(output_dir,
                                  out_bands,
                                  out_files,
                                  band_conf,
                                  crs,
                                  emb_dims,
                                  emb_names) {
    callback <- luz::luz_callback(
        name = ".callback_post_encode",
        initialize = function(output_dir, out_bands, out_files, band_conf, crs,
                              emb_dims, emb_names) {
            self$output_dir <- output_dir
            self$out_bands <- out_bands
            self$out_files <- out_files
            self$band_conf <- band_conf
            self$crs <- crs
            self$emb_dims <- emb_dims
            self$emb_names <- emb_names
        },
        on_predict_batch_end = function() {
            # Starts with zero and then updates when there are valid values
            input_pixels <- 0
            # Get prediction as a matrix with labels
            # When a chunk has no valid pixels, the wrapped model returns a
            # scalar NA instead of a tensor; only process actual tensors
            values <- ctx$pred[[length(ctx$pred)]]
            if (inherits(values, "torch_tensor")) {
                # Get predicted values
                values <- torch::as_array(values)
                # Get number of valid pixels
                input_pixels <- dim(values)[[1L]]
            }
            # Get auxiliary values for the callback
            na_mask <- as.logical(as.array(ctx$batch[["na_mask"]]))
            block_vec <- as.numeric(as.array(ctx$batch[["block"]]))
            # Rebuild block tibble
            block <- tibble::tibble_row(
                # spatial metadata
                col = block_vec[[1L]],
                row = block_vec[[2L]],
                ncols = block_vec[[3L]],
                nrows = block_vec[[4L]],
                # geometry
                xmin = block_vec[[5L]],
                xmax = block_vec[[6L]],
                ymin = block_vec[[7L]],
                ymax = block_vec[[8L]],
                # crs
                crs = self$crs
            )
            # Log start of block
            .debug_log(
                event = "start_block_data_encode",
                key = "model",
                value = "torch_model"
            )
            if (length(input_pixels) > 0L) {
                # apply offset
                offset <- .offset(self$band_conf)
                if (.has(offset) && offset != 0.0) {
                    values <- values - offset
                }
                # apply scale
                scale <- .scale(self$band_conf)
                max_value <- .max_value(self$band_conf)
                min_value <- .min_value(self$band_conf)
                if (.has(scale) && scale != 1.0) {
                    values <- values / scale
                }
                values[values > max_value] <- max_value
                values[values < min_value] <- min_value
            }
            # Log end of block
            .debug_log(
                event = "end_block_data_encode",
                key = "model",
                value = "torch_model"
            )
            full_values <- matrix(
                NA_real_,
                nrow = length(na_mask),
                ncol = self$emb_dims,
                dimnames = list(NULL, self$emb_names)
            )
            if (input_pixels > 0L) {
                full_values[!na_mask, ] <- values
            }
            # Write block
            block_file <- .encode_write_block(
                values = full_values,
                chunk = block,
                output_dir = self$output_dir,
                out_files = self$out_files,
                out_bands = self$out_bands
            )
            # Replace accumulated tensor with the block file path
            ctx$pred[[length(ctx$pred)]] <- block_file
            ctx$pred
        }
    )
    # Build and return!
    callback(
        output_dir = output_dir,
        out_bands = out_bands,
        out_files = out_files,
        band_conf = band_conf,
        crs = crs,
        emb_dims = emb_dims,
        emb_names = emb_names
    )
}

#' @title Create a torch callback for classify
#' @name .callback_post_classify
#' @keywords internal
#' @noRd
#'
#' @param output_dir A character with output directory.
#' @param out_file   A character vector with output file.
#' @param out_band   A character vector with output band.
#' @param band_conf  A list with band config.
#' @param ml_labels  A character vector with model labels.
#' @param crs        A character with tile crs.
#'
#' @return A torch callback
.callback_post_classify <- function(output_dir,
                                    out_file,
                                    out_band,
                                    band_conf,
                                    ml_labels,
                                    crs) {
    callback <- luz::luz_callback(
        name = ".callback_post_classify",
        initialize = function(output_dir, out_file, out_band, band_conf,
                              ml_labels, crs) {
            self$output_dir <- output_dir
            self$out_file <- out_file
            self$out_band <- out_band
            self$band_conf <- band_conf
            self$ml_labels <- ml_labels
            self$crs <- crs
        },
        on_predict_batch_end = function() {
            # Starts with zero and then updates when there are valid values
            input_pixels <- 0
            # Get prediction as a matrix with labels
            # When a chunk has no valid pixels, the wrapped model returns a
            # scalar NA instead of a tensor; only process actual tensors
            values <- ctx$pred[[length(ctx$pred)]]
            if (inherits(values, "torch_tensor")) {
                # Get predicted values
                values <- torch::as_array(values)
                colnames(values) <- self$ml_labels
                # Get number of valid pixels
                input_pixels <- dim(values)[[1L]]
            }
            # Get auxiliary values for the callback
            na_mask <- as.logical(as.array(ctx$batch[["na_mask"]]))
            block_vec <- as.numeric(as.array(ctx$batch[["block"]]))
            # Rebuild block tibble
            block <- tibble::tibble_row(
                # spatial metadata
                col = block_vec[[1L]],
                row = block_vec[[2L]],
                ncols = block_vec[[3L]],
                nrows = block_vec[[4L]],
                # geometry
                xmin = block_vec[[5L]],
                xmax = block_vec[[6L]],
                ymin = block_vec[[7L]],
                ymax = block_vec[[8L]],
                # crs
                crs = self$crs
            )
            # Apply scaling to classified values
            band_scale <- .scale(self$band_conf)
            # Reconstruct full output matrix with NA for masked pixels
            n_labels <- length(self$ml_labels)
            full_values <- matrix(
                NA_real_,
                nrow = length(na_mask),
                ncol = n_labels,
                dimnames = list(NULL, self$ml_labels)
            )
            # Log start of block
            .debug_log(
                event = "start_block_data_normalization",
                key = "model",
                value = "torch_model"
            )
            if (input_pixels > 0L) {
                # Normalize values
                values <- .ml_normalize.torch_model(values, NULL)
                # Check processed values
                .check_processed_values(
                    values = values,
                    input_pixels = input_pixels
                )
                # Scale values
                full_values[!na_mask, ] <- values / band_scale
            }
            # Log end of block
            .debug_log(
                event = "end_block_data_normalization",
                key = "model",
                value = "torch_model"
            )
            # Write block
            block_file <- .classify_write_block(
                values = full_values,
                block = block,
                output_dir = self$output_dir,
                out_file = self$out_file,
                out_band = self$out_band
            )
            # Replace accumulated tensor with the block file path
            ctx$pred[[length(ctx$pred)]] <- block_file
            ctx$pred
        }
    )
    # Build and return!
    callback(
        output_dir = output_dir,
        out_file = out_file,
        out_band = out_band,
        band_conf = band_conf,
        ml_labels = ml_labels,
        crs = crs
    )
}
#' @title Decide if a value must be processed as a dataloader
#' @name .torch_use_dataloader
#' @keywords internal
#' @noRd
#'
#' @param value Object passed to a sits model.
#'
#' @return Logical indicating to use or not a dataloader
.torch_use_dataloader <- function(value) {
    is.list(value) && inherits(value[["dataset"]], "dataset") &&
        inherits(value[["callback"]], "LuzCallback")
}
