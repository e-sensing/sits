#' @title Torch-backed Bayesian smoother for probability cubes
#' @name .torch_smooth_bayes_fraction
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Drop-in replacement for the C++ \code{bayes_smoother_fraction()} function.
#' Uses torch tensor operations so the computation can run on GPU when
#' CUDA or MPS is available.
#'
#' Padding note: PyTorch \code{"reflect"} mode mirrors from the element
#' adjacent to the boundary (boundary pixel is not repeated), whereas the
#' C++ implementation repeats the boundary pixel once.  The mismatch only
#' affects the outermost \code{leg = floor(window_size / 2)} rows/columns of
#' each processed block; those pixels are discarded by the overlap/crop_block
#' mechanism in \code{.smooth_tile()}, so the final output is unaffected for
#' all interior blocks.
#'
#' @param  logits          Numeric matrix \code{[nrows*ncols, nbands]} of
#'                         logit-transformed probabilities.
#' @param  nrows           Number of rows in the block.
#' @param  ncols           Number of columns in the block.
#' @param  window_size     Side length of the square neighbourhood (odd integer).
#' @param  smoothness      Numeric vector of length \code{nbands}: the prior
#'                         variance parameter per class.
#' @param  neigh_fraction  Fraction of neighbourhood values (highest logits)
#'                         used to estimate the prior.
#' @return Numeric matrix \code{[nrows*ncols, nbands]} of smoothed logits.
#'
.torch_smooth_bayes_fraction <- function(logits,
                                         nrows,
                                         ncols,
                                         window_size,
                                         smoothness,
                                         neigh_fraction) {
    # Select compute device
    device <- if (torch::cuda_is_available()) {
        "cuda"
    } else if (torch::backends_mps_is_available()) {
        "mps"
    } else {
        "cpu"
    }

    nbands  <- ncol(logits)
    npix    <- nrows * ncols
    leg     <- window_size %/% 2L
    win_sq  <- as.integer(window_size * window_size)

    # ---- Build input tensor [1, nbands, nrows, ncols] ----
    x <- torch::torch_tensor(
        t(logits),
        dtype  = torch::torch_float32(),
        device = device
    )$view(c(nbands, nrows, ncols))$unsqueeze(1L)

    # ---- Reflect-pad spatial dimensions (all bands at once — cheap) ----
    x_pad <- torch::nnf_pad(
        x,
        pad  = c(leg, leg, leg, leg),
        mode = "reflect"
    )
    # x_pad: [1, nbands, nrows+2*leg, ncols+2*leg]

    # ---- Reusable scalar tensors and position index ----
    zero_t   <- torch::torch_tensor(
        0.0, dtype = torch::torch_float32(), device = device
    )
    neginf_t <- torch::torch_tensor(
        -Inf, dtype = torch::torch_float32(), device = device
    )
    # Position index for top-fraction selection [1, win_sq]
    pos <- torch::torch_tensor(
        seq_len(win_sq),
        dtype  = torch::torch_float32(),
        device = device
    )$view(c(1L, win_sq))

    # ---- Original pixel values for all bands [npix, nbands] ----
    x0_all <- torch::torch_tensor(
        logits, dtype = torch::torch_float32(), device = device
    )

    # ---- Process one band at a time to limit GPU memory ----
    # Without this, nnf_unfold on all bands produces a
    # [nbands, npix, win_sq] tensor that can easily exceed GPU memory.
    band_results <- vector("list", nbands)
    for (b in seq_len(nbands)) {
        # Extract single band from padded tensor: [1, 1, H_pad, W_pad]
        x_b <- x_pad[, b, , , drop = FALSE]

        # Unfold windows: [1, win_sq, npix] → [npix, win_sq]
        wins_b <- torch::nnf_unfold(
            x_b,
            kernel_size = window_size,
            stride      = 1L,
            padding     = 0L
        )$squeeze(1L)$t()
        # wins_b: [npix, win_sq]

        # ---- NaN handling ----
        nan_mask_b <- torch::torch_isnan(wins_b)

        # ---- Select neighbourhood values ----
        if (neigh_fraction == 1.0) {
            selected_b <- torch::torch_where(nan_mask_b, zero_t, wins_b)
            sel_mask_b <- !nan_mask_b
            n_b        <- (!nan_mask_b)$sum(dim = -1L)$to(
                dtype = torch::torch_float32()
            )
        } else {
            wins_sort_b <- torch::torch_where(nan_mask_b, neginf_t, wins_b)
            sorted_b    <- torch::torch_sort(
                wins_sort_b, dim = -1L, descending = TRUE
            )[[1L]]

            valid_b    <- (!nan_mask_b)$sum(dim = -1L)$to(
                dtype = torch::torch_float32()
            )
            neigh_hi_b <- torch::torch_ceil(
                neigh_fraction * valid_b
            )$clamp_min(1L)

            sel_mask_b <- pos$le(neigh_hi_b$unsqueeze(-1L))
            selected_b <- torch::torch_where(sel_mask_b, sorted_b, zero_t)
            n_b        <- neigh_hi_b
        }

        # ---- Unbiased mean and variance ----
        m0_b <- selected_b$sum(dim = -1L) / n_b

        diff_sq_b <- torch::torch_where(
            sel_mask_b,
            (selected_b - m0_b$unsqueeze(-1L))$pow(2L),
            zero_t
        )
        s0_b <- torch::torch_where(
            n_b$gt(1.0),
            diff_sq_b$sum(dim = -1L) / (n_b - 1.0),
            zero_t
        )

        # ---- Bayesian update for this band ----
        x0_b   <- x0_all[, b]
        w_b    <- s0_b / (s0_b + smoothness[b])
        bayes_b <- w_b * x0_b + (1.0 - w_b) * m0_b
        use_m0  <- torch::torch_isnan(x0_b) | s0_b$lt(1e-4)

        band_results[[b]] <- torch::torch_where(use_m0, m0_b, bayes_b)
    }

    # ---- Stack bands and convert to R matrix [npix, nbands] ----
    result <- torch::torch_stack(band_results, dim = 2L)
    as.matrix(
        result$cpu()$to(dtype = torch::torch_float64())
    )
}

#' @title Torch Bayesian smoother closure factory
#' @name .smooth_fn_bayes_torch
#' @keywords internal
#' @noRd
#'
#' @description
#' Mirrors \code{.smooth_fn_bayes()} from \file{api_smooth.R} but calls the
#' torch-backed \code{.torch_smooth_bayes_fraction()} instead of the C++
#' function.  Applies the same logit / inverse-logit transforms.
#'
#' @param  window_size     Size of the neighbourhood (odd integer, min 5).
#' @param  neigh_fraction  Fraction of highest-valued neighbours to use.
#' @param  smoothness      Numeric vector (length = number of classes).
#' @return A closure \code{function(values, block)} compatible with
#'         \code{.smooth_tile()}.
#'
.smooth_fn_bayes_torch <- function(window_size,
                                   neigh_fraction,
                                   smoothness) {
    # Check window size
    .check_int_parameter(window_size, min = 5L, is_odd = TRUE)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, exclusive_min = 0.0, max = 1.0)

    smooth_fn <- function(values, block) {
        # Record expected number of pixels
        input_pixels <- nrow(values)
        # Clamp to avoid ±Inf in logit
        values[values == 1.0] <- 0.999999
        values[values == 0.0] <- 0.000001
        # Transform to logits
        values <- log(values / (rowSums(values) - values))
        # Apply torch Bayesian smoother
        values <- .torch_smooth_bayes_fraction(
            logits         = values,
            nrows          = .nrows(block),
            ncols          = .ncols(block),
            window_size    = window_size,
            smoothness     = smoothness,
            neigh_fraction = neigh_fraction
        )
        # Inverse logit
        values <- exp(values) / (exp(values) + 1.0)
        # Sanity check
        .check_processed_values(values, input_pixels)
        values
    }
    smooth_fn
}

#' @title Smooth a probability cube using the torch Bayesian smoother
#' @name .smooth_torch
#' @keywords internal
#' @noRd
#'
#' @description
#' Mirrors \code{.smooth()} from \file{api_smooth.R}.  Builds the torch
#' smooth closure and iterates over each tile via \code{.smooth_tile()}.
#'
#' @param  cube            Probability data cube.
#' @param  block           Block specification for chunked processing.
#' @param  window_size     Size of the neighbourhood.
#' @param  neigh_fraction  Fraction of highest-valued neighbours to use.
#' @param  smoothness      Numeric vector (one value per class).
#' @param  exclusion_mask  Optional spatial mask.
#' @param  multicores      Number of parallel workers.
#' @param  memsize         Memory budget in GB.
#' @param  output_dir      Directory for output files.
#' @param  version         Version string for output files.
#' @param  progress        Show progress bar?
#' @return Smoothed data cube (same class as input).
#'
.smooth_torch <- function(cube,
                          block,
                          window_size,
                          neigh_fraction,
                          smoothness,
                          exclusion_mask,
                          multicores,
                          memsize,
                          output_dir,
                          version,
                          progress) {
    smooth_fn <- .smooth_fn_bayes_torch(
        window_size    = window_size,
        neigh_fraction = neigh_fraction,
        smoothness     = smoothness
    )
    overlap <- ceiling(window_size / 2L) - 1L
    .cube_foreach_tile(cube, function(tile) {
        .smooth_tile(
            tile           = tile,
            band           = "bayes",
            block          = block,
            overlap        = overlap,
            exclusion_mask = exclusion_mask,
            smooth_fn      = smooth_fn,
            output_dir     = output_dir,
            version        = version,
            progress       = progress
        )
    })
}
