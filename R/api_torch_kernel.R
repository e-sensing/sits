#' @title Unfold a single-band raster block into local windows using torch
#' @name .torch_kernel_unfold
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Shared setup for all torch kernel functions.  Extracts the requested band
#' column from \code{x}, builds a \code{[1, 1, nrows, ncols]} tensor,
#' reflect-pads it, and unfolds it into a \code{[npix, win_sq]} tensor where
#' each row contains the flattened \code{window_size × window_size}
#' neighbourhood of the corresponding pixel.
#'
#' Padding note: PyTorch \code{"reflect"} and the C++ \code{locus_mirror}
#' differ at the block boundary (identical caveat as in \file{api_smooth_torch.R}
#' and \file{api_texture_torch.R}); the mismatch is discarded by the
#' overlap / crop mechanism of the calling pipelines.
#'
#' @param x           Numeric matrix \code{[npix, nbands]}.
#' @param ncols       Number of columns in the block.
#' @param nrows       Number of rows in the block.
#' @param band        0-indexed band to process (matches C++ convention).
#' @param window_size Side length of the square neighbourhood (odd integer).
#' @param device      torch device string.
#' @return Float32 tensor \code{[npix, window_size^2]} on \code{device}.
.torch_kernel_unfold <- function(x, ncols, nrows, band, window_size, device,
                                   dtype = torch::torch_float32()) {
    leg <- window_size %/% 2L

    # 0-indexed band → 1-indexed R column
    band_vals <- as.numeric(x[, band + 1L])

    t <- torch::torch_tensor(
        matrix(band_vals, nrow = 1L),
        dtype  = dtype,
        device = device
    )$view(c(1L, 1L, nrows, ncols))

    t_pad <- torch::nnf_pad(t, pad = c(leg, leg, leg, leg), mode = "reflect")

    torch::nnf_unfold(
        t_pad,
        kernel_size = window_size,
        stride      = 1L,
        padding     = 0L
    )$squeeze(1L)$t()  # [npix, win_sq]
}

#' @title Torch sliding-window mean
#' @name .torch_kernel_mean
#' @keywords internal
#' @noRd
#' @description
#' Drop-in replacement for \code{C_kernel_mean()}.
#' Mirrors \code{mean(na_omit(neigh))} from \file{src/kernel.cpp}:
#' NaN pixels in each window are excluded from the sum and count;
#' result is NaN only when the entire window is NaN.
#'
#' @param x           Numeric matrix \code{[npix, nbands]}.
#' @param ncols       Number of columns in the block.
#' @param nrows       Number of rows in the block.
#' @param band        0-indexed band column to process.
#' @param window_size Side length of the square neighbourhood.
#' @return Numeric vector of length \code{nrows * ncols}.
.torch_kernel_mean <- function(x, ncols, nrows, band, window_size) {
    device <- if (.torch_gpu_available()) {
        if (torch::cuda_is_available()) "cuda" else "mps"
    } else "cpu"

    wins     <- .torch_kernel_unfold(x, ncols, nrows, band, window_size, device)
    nan_mask <- torch::torch_isnan(wins)                         # [npix, win_sq]
    zero_t   <- torch::torch_tensor(0.0, dtype = torch::torch_float32(), device = device)

    safe_wins <- torch::torch_where(nan_mask, zero_t, wins)      # NaN → 0
    n_valid   <- (!nan_mask)$to(dtype = torch::torch_float32())$sum(dim = 2L) # [npix]
    result    <- safe_wins$sum(dim = 2L) / n_valid               # 0/0 = NaN when all-NaN

    as.numeric(result$cpu()$to(dtype = torch::torch_float64()))
}

#' @title Torch sliding-window sample variance
#' @name .torch_kernel_var
#' @keywords internal
#' @noRd
#' @description
#' Drop-in replacement for \code{C_kernel_var()}.
#' Mirrors \code{var(na_omit(neigh))} (N−1 denominator, Bessel's correction).
#' Returns NaN when the valid count is 0 (all NaN) or 1 (undefined for N−1).
#'
#' @inheritParams .torch_kernel_mean
#' @return Numeric vector of length \code{nrows * ncols}.
.torch_kernel_var <- function(x, ncols, nrows, band, window_size) {
    device <- if (.torch_gpu_available()) {
        if (torch::cuda_is_available()) "cuda" else "mps"
    } else "cpu"

    wins     <- .torch_kernel_unfold(x, ncols, nrows, band, window_size, device)
    nan_mask <- torch::torch_isnan(wins)
    zero_t   <- torch::torch_tensor(0.0, dtype = torch::torch_float32(), device = device)
    nan_t    <- torch::torch_tensor(NaN, dtype = torch::torch_float32(), device = device)

    n_valid  <- (!nan_mask)$to(dtype = torch::torch_float32())$sum(dim = 2L) # [npix]

    safe_wins <- torch::torch_where(nan_mask, zero_t, wins)
    sum_vals  <- safe_wins$sum(dim = 2L)
    # Use a safe mean (replace 0-count pixels with 0 to avoid NaN propagating
    # into the deviation computation; the final mask handles the all-NaN case).
    safe_mu   <- torch::torch_where(
        n_valid$eq(0.0), zero_t, sum_vals / n_valid
    )
    dev_sq    <- torch::torch_where(
        nan_mask,
        zero_t,
        (wins - safe_mu$unsqueeze(2L))^2
    )
    # N-1 denominator: 0/(1-1) = 0/0 = NaN for n=1 ✓
    var_val   <- dev_sq$sum(dim = 2L) / (n_valid - 1.0)
    # n=0 case: 0/(-1) = 0.0 → override with NaN
    var_val   <- torch::torch_where(n_valid$eq(0.0), nan_t, var_val)

    as.numeric(var_val$cpu()$to(dtype = torch::torch_float64()))
}

#' @title Torch sliding-window sample standard deviation (NA-aware)
#' @name .torch_kernel_sd
#' @keywords internal
#' @noRd
#' @description
#' Drop-in replacement for \code{C_kernel_sd()}.
#' Defined as \code{sqrt(.torch_kernel_var(...))}.
#'
#' @inheritParams .torch_kernel_mean
#' @return Numeric vector of length \code{nrows * ncols}.
.torch_kernel_sd <- function(x, ncols, nrows, band, window_size) {
    sqrt(.torch_kernel_var(x, ncols, nrows, band, window_size))
}

#' @title Torch sliding-window minimum
#' @name .torch_kernel_min
#' @keywords internal
#' @noRd
#' @description
#' Drop-in replacement for \code{C_kernel_min()}.
#' Mirrors \code{min(na_omit(neigh))}: NaN values in a window are ignored;
#' result is NaN only when the entire window is NaN.
#'
#' @inheritParams .torch_kernel_mean
#' @return Numeric vector of length \code{nrows * ncols}.
.torch_kernel_min <- function(x, ncols, nrows, band, window_size) {
    device <- if (.torch_gpu_available()) {
        if (torch::cuda_is_available()) "cuda" else "mps"
    } else "cpu"

    wins     <- .torch_kernel_unfold(x, ncols, nrows, band, window_size, device)
    nan_mask <- torch::torch_isnan(wins)
    nan_t    <- torch::torch_tensor(NaN, dtype = torch::torch_float32(), device = device)
    inf_t    <- torch::torch_tensor(Inf, dtype = torch::torch_float32(), device = device)

    # Replace NaN with +Inf so they are ignored by min
    safe_wins <- torch::torch_where(nan_mask, inf_t, wins)
    min_vals  <- safe_wins$min(dim = 2L)[[1L]]                  # [npix]

    # All-NaN pixels still produce Inf; convert to NaN
    all_nan  <- nan_mask$all(dim = 2L)
    min_vals <- torch::torch_where(all_nan, nan_t, min_vals)

    as.numeric(min_vals$cpu()$to(dtype = torch::torch_float64()))
}

#' @title Torch sliding-window maximum
#' @name .torch_kernel_max
#' @keywords internal
#' @noRd
#' @description
#' Drop-in replacement for \code{C_kernel_max()}.
#' Mirrors \code{max(na_omit(neigh))}: NaN values in a window are ignored;
#' result is NaN only when the entire window is NaN.
#'
#' @inheritParams .torch_kernel_mean
#' @return Numeric vector of length \code{nrows * ncols}.
.torch_kernel_max <- function(x, ncols, nrows, band, window_size) {
    device <- if (.torch_gpu_available()) {
        if (torch::cuda_is_available()) "cuda" else "mps"
    } else "cpu"

    wins     <- .torch_kernel_unfold(x, ncols, nrows, band, window_size, device)
    nan_mask <- torch::torch_isnan(wins)
    nan_t    <- torch::torch_tensor(NaN, dtype = torch::torch_float32(), device = device)
    neginf_t <- torch::torch_tensor(-Inf, dtype = torch::torch_float32(), device = device)

    # Replace NaN with -Inf so they are ignored by max
    safe_wins <- torch::torch_where(nan_mask, neginf_t, wins)
    max_vals  <- safe_wins$max(dim = 2L)[[1L]]                  # [npix]

    all_nan  <- nan_mask$all(dim = 2L)
    max_vals <- torch::torch_where(all_nan, nan_t, max_vals)

    as.numeric(max_vals$cpu()$to(dtype = torch::torch_float64()))
}

#' @title Torch sliding-window median
#' @name .torch_kernel_median
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Drop-in replacement for \code{C_kernel_median()}.
#' Mirrors \code{median(neigh, naRm = TRUE)} from \file{src/kernel.cpp}.
#' NaN values in each window are excluded from the sorted sequence and count;
#' for an even number of valid values the result is the average of the two
#' middle values (matching R's \code{median()} definition).
#' Returns NaN only when the entire window is NaN.
#'
#' \strong{Implementation:} NaN values are replaced by \code{+Inf} before
#' sorting so they migrate to the high end of the sorted window and do not
#' affect the median selection; the valid-count tensor determines the correct
#' median indices.  Indices passed to \code{gather()} are 0-based element
#' positions (PyTorch convention), while the \code{dim} argument uses R's
#' 1-based convention.
#'
#' @inheritParams .torch_kernel_mean
#' @return Numeric vector of length \code{nrows * ncols}.
.torch_kernel_median <- function(x, ncols, nrows, band, window_size) {
    device <- if (.torch_gpu_available()) {
        if (torch::cuda_is_available()) "cuda" else "mps"
    } else "cpu"

    wins     <- .torch_kernel_unfold(x, ncols, nrows, band, window_size, device)
    nan_mask <- torch::torch_isnan(wins)
    inf_t    <- torch::torch_tensor(Inf, dtype = torch::torch_float32(), device = device)
    nan_t    <- torch::torch_tensor(NaN, dtype = torch::torch_float32(), device = device)

    n_valid <- (!nan_mask)$to(dtype = torch::torch_float32())$sum(dim = 2L) # [npix]

    # Sort with NaN → +Inf so they accumulate at the tail
    wins_for_sort <- torch::torch_where(nan_mask, inf_t, wins)
    sorted        <- torch::torch_sort(wins_for_sort, dim = 2L)[[1L]]       # [npix, win_sq]

    # Median indices: R's torch gather and argmax are both 1-based.
    # lo = floor((n-1)/2) + 1, hi = floor(n/2) + 1
    # result = (sorted[lo] + sorted[hi]) / 2
    lo_idx <- ((n_valid - 1.0) / 2.0)$floor()$to(
        dtype = torch::torch_long()
    )$add(1L)$clamp_min(1L)$unsqueeze(2L)  # [npix, 1]

    hi_idx <- (n_valid / 2.0)$floor()$to(
        dtype = torch::torch_long()
    )$add(1L)$clamp_min(1L)$unsqueeze(2L)  # [npix, 1]

    lo_val <- sorted$gather(2L, lo_idx)$squeeze(2L)  # [npix]
    hi_val <- sorted$gather(2L, hi_idx)$squeeze(2L)

    med_val <- (lo_val + hi_val) / 2.0

    # All-NaN pixels produce Inf in lo/hi → override with NaN
    all_nan <- n_valid$eq(0.0)
    med_val <- torch::torch_where(all_nan, nan_t, med_val)

    as.numeric(med_val$cpu()$to(dtype = torch::torch_float64()))
}

#' @title Torch sliding-window mode
#' @name .torch_kernel_modal
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Drop-in replacement for \code{C_kernel_modal()}.
#' Mirrors the C++ \code{_modal()} function exactly:
#' \itemize{
#'   \item If \strong{any} value in the window is NaN, the result is NaN.
#'   \item Otherwise, the most frequently occurring value is returned.
#'     Ties are broken by the first maximum (matching \code{argmax} semantics,
#'     equivalent to \code{std::max_element} on the insertion-ordered map in
#'     the C++ implementation for values encountered in raster-scan order).
#' }
#'
#' \strong{Implementation:} batched pairwise equality
#' (\code{wins[k] == wins[l]}) accumulates occurrence counts without
#' requiring a discrete grey-level assumption.  Pixels are processed in
#' batches of 2048 to bound GPU memory usage (≤ ~20 MB per batch for
#' \code{window_size ≤ 7}).
#'
#' @inheritParams .torch_kernel_mean
#' @return Numeric vector of length \code{nrows * ncols}.
.torch_kernel_modal <- function(x, ncols, nrows, band, window_size) {
    # Use CPU + float64 for modal to guarantee exact value equality
    # (float32 can conflate distinct float64 values and produce wrong tie-breaking).
    # MPS does not support float64, so CPU is safest regardless of device.
    device <- "cpu"

    wins     <- .torch_kernel_unfold(x, ncols, nrows, band, window_size, device,
                                    dtype = torch::torch_float64())
    npix     <- nrows * ncols
    nan_mask <- torch::torch_isnan(wins)                         # [npix, win_sq]
    any_nan  <- nan_mask$any(dim = 2L)                           # [npix] bool
    nan_t    <- torch::torch_tensor(NaN, dtype = torch::torch_float32(), device = device)

    result   <- torch::torch_full(
        c(npix),
        NaN,
        dtype  = torch::torch_float64(),
        device = device
    )

    # Process all pixels in batches; mark any-NaN pixels as NaN in the result
    batch_size <- 2048L
    for (b_start in seq(1L, npix, by = batch_size)) {
        b_end <- min(npix, b_start + batch_size - 1L)
        w_b   <- wins[b_start:b_end, ]          # [batch, win_sq]

        # Sort windows ascending: ties then resolved toward smallest value,
        # matching C++ std::map iteration order + std::max_element behavior.
        w_b_sorted <- torch::torch_sort(w_b, dim = 2L)[[1L]]  # [batch, win_sq]

        # Pairwise equality on sorted values: [batch, win_sq, win_sq]
        # NaN == NaN is FALSE in IEEE 754; NaN pixels handled by any_nan mask
        eq <- (w_b_sorted$unsqueeze(3L) == w_b_sorted$unsqueeze(2L))$to(
            dtype = torch::torch_float32()
        )
        counts <- eq$sum(dim = 3L)                 # [batch, win_sq] - frequency per position
        # R torch: argmax and gather both use 1-based indices -- no offset needed
        mode_pos <- counts$argmax(dim = 2L)$unsqueeze(2L)  # [batch, 1]
        mode_val <- w_b_sorted$gather(2L, mode_pos)$squeeze(2L)     # [batch]

        # Override with NaN where any window value was NaN
        has_nan_b <- any_nan[b_start:b_end]
        result[b_start:b_end] <- torch::torch_where(
            has_nan_b,
            torch::torch_full_like(mode_val, NaN),
            mode_val
        )
    }

    as.numeric(result$cpu()$to(dtype = torch::torch_float64()))
}

#' @title Torch-backed kernel function environment factory
#' @name .torch_kern_functions
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Drop-in replacement for \code{.kern_functions()} from \file{api_apply.R}.
#' Returns an environment with the same seven \code{w_*} names
#' (\code{w_median}, \code{w_mean}, \code{w_sd}, \code{w_min}, \code{w_max},
#' \code{w_var}, \code{w_modal}), compatible with
#' \code{eval(expr, envir = values, enclos = ...)}.
#' Each function closes over \code{window_size}, \code{img_nrow}, and
#' \code{img_ncol} and delegates to the corresponding
#' \code{.torch_kernel_*} function.
#'
#' @param window_size Side length of the square neighbourhood (odd integer).
#' @param img_nrow    Number of rows in the block.
#' @param img_ncol    Number of columns in the block.
#' @return An environment containing \code{w_median}, \code{w_mean},
#'   \code{w_sd}, \code{w_min}, \code{w_max}, \code{w_var}, \code{w_modal}.
.torch_kern_functions <- function(window_size, img_nrow, img_ncol) {
    make_fn <- function(torch_fn) {
        force(torch_fn)
        function(m) {
            torch_fn(
                x           = as.matrix(m),
                ncols       = img_ncol,
                nrows       = img_nrow,
                band        = 0L,
                window_size = window_size
            )
        }
    }
    list2env(
        list(
            w_median = make_fn(.torch_kernel_median),
            w_mean   = make_fn(.torch_kernel_mean),
            w_sd     = make_fn(.torch_kernel_sd),
            w_min    = make_fn(.torch_kernel_min),
            w_max    = make_fn(.torch_kernel_max),
            w_var    = make_fn(.torch_kernel_var),
            w_modal  = make_fn(.torch_kernel_modal)
        ),
        parent = parent.env(environment()),
        hash   = TRUE
    )
}
