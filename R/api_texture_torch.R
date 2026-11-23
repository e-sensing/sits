#' @title Compute the Angular Second Moment from pair tensors
#' @name .texture_torch_glcm_asm
#' @keywords internal
#' @noRd
#' @description
#' ASM = Σᵢⱼ P[i,j]², where P is the normalised GLCM.
#' Equivalently: \code{(1/total²) Σₖ Σₗ [pair_code_k == pair_code_l]}, where
#' each pair is encoded as an integer \code{i * n_grey + j}.
#' The codes of each pixel are sorted and the runs of equal codes counted,
#' in pixel batches of about 2^20 codes, so the memory is linear in the pairs.
#'
#' @param ctr        Float32 tensor \code{[npix, total]} of centre grey levels.
#' @param nbr        Float32 tensor \code{[npix, total]} of neighbour grey levels.
#' @param total      Integer: number of pairs per pixel (after symmetrisation).
#' @param n_grey     Number of grey levels.
#' @param npix       Number of pixels in the block.
#' @param device     torch device string (\code{"cpu"}, \code{"cuda"}, or
#'                   \code{"mps"}).
#' @return Float32 tensor \code{[npix]} of per-pixel ASM values.
.texture_torch_glcm_asm <- function(ctr, nbr, total, n_grey, npix, device) {
    # a batch holds about 2^20 codes, so the memory does not grow with npix
    batch_size <- max(1L, 2L^20 %/% total)
    long <- torch::torch_long()
    result <- torch::torch_zeros(
        npix,
        dtype  = torch::torch_float32(),
        device = device
    )
    pos <- torch::torch_arange(
        0L, total - 1L, dtype = long, device = device
    )$unsqueeze(1L)
    for (b_start in seq(1L, npix, by = batch_size)) {
        b_end <- min(npix, b_start + batch_size - 1L)
        # Encode each (i, j) pair as a single non-negative integer and sort
        # the codes of each pixel, so equal codes make runs
        codes <- (
            ctr[b_start:b_end, ]$to(dtype = long) * as.integer(n_grey) +
            nbr[b_start:b_end, ]$to(dtype = long)
        )$sort(dim = 2L)[[1L]]
        n <- b_end - b_start + 1L
        is_new <- torch::torch_cat(list(
            torch::torch_ones(c(n, 1L), dtype = torch::torch_bool(),
                              device = device),
            codes[, 2L:total]$ne(codes[, 1L:(total - 1L)])
        ), dim = 2L)
        is_end <- torch::torch_cat(list(
            is_new[, 2L:total],
            torch::torch_ones(c(n, 1L), dtype = torch::torch_bool(),
                              device = device)
        ), dim = 2L)
        # first and last position of the run of each code
        run_first <- torch::torch_where(
            is_new, pos, torch::torch_zeros_like(codes)
        )$cummax(dim = 2L)[[1L]]
        run_last <- torch::torch_where(
            is_end, pos, torch::torch_full_like(codes, total)
        )$flip(2L)$cummin(dim = 2L)[[1L]]$flip(2L)
        # the sum over codes of run_length^2 is the sum over pairs of the
        # length of their run
        result[b_start:b_end] <- (run_last - run_first + 1L)$sum(dim = 2L)$to(
            dtype = torch::torch_float32()
        ) / (as.numeric(total)^2)
        rm(codes, is_new, is_end, run_first, run_last)
        # torch frees its tensors when R collects them, so each batch does
        gc()
    }
    result
}

#' @title Compute a single GLCM metric for a raster block using torch
#' @name .texture_torch_glcm_compute
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Drop-in replacement for the per-metric C++ functions
#' (\code{C_glcm_contrast}, \code{C_glcm_dissimilarity}, ...).
#' Replicates the algorithm in \file{src/glcm_fns.cpp}:
#' \enumerate{
#'   \item For each angle \eqn{\theta}, compute
#'         \eqn{d_r = \text{round}(\sin\theta)},
#'         \eqn{d_c = \text{round}(\cos\theta)}.
#'   \item Extract all valid centre–neighbour pairs from the local
#'         \code{window_size × window_size} neighbourhood, including the
#'         symmetric (neighbour→centre) direction.
#'   \item Derive the requested GLCM statistic directly from the pair lists
#'         (exact for all metrics; see mathematical notes below).
#'   \item Average over angles.
#' }
#'
#' \strong{Padding note:} PyTorch \code{"reflect"} mode and the C++ mirror
#' function \code{locus_neigh2} differ at the boundary: the C++ version
#' includes the outermost pixel in the first reflected position, while
#' PyTorch skips it.  The mismatch affects only the outermost
#' \code{leg = floor(window_size / 2)} rows/columns of each block,
#' which are discarded by the overlap–crop mechanism in
#' \code{.texture_feature()}.
#'
#' \strong{Mathematical equivalence (pair-based formulas):}
#' All metrics except ASM/Energy are linear in P[i,j], so
#' \eqn{\sum_{i,j} P[i,j]\,f(i,j) = \frac{1}{T}\sum_k f(\text{ctr}_k, \text{nbr}_k)},
#' where T is the total number of pairs and the sum runs over all pair
#' occurrences (not unique pairs).
#' ASM uses a batched pairwise-equality approach (see \code{.texture_torch_glcm_asm}).
#'
#' @param m           Named list element (band values), as passed via
#'                    \code{envir} in \code{eval()}.
#' @param metric      Character string: one of \code{"contrast"},
#'                    \code{"dissimilarity"}, \code{"homogeneity"},
#'                    \code{"energy"}, \code{"asm"}, \code{"mean"},
#'                    \code{"variance"}, \code{"std"}, \code{"correlation"}.
#' @param window_size Odd integer side length of the square neighbourhood.
#' @param angles      Numeric vector of direction angles in radians.
#' @param img_nrow    Number of rows in the block.
#' @param img_ncol    Number of columns in the block.
#' @param n_grey      Number of grey levels (\code{max_value} of texture config).
#' @return Numeric matrix \code{[img_nrow * img_ncol, 1]} of GLCM metric values.
.texture_torch_glcm_compute <- function(m, metric, window_size, angles,
                                img_nrow, img_ncol, n_grey) {
    # Select device
    device <- if (.torch_gpu_available()) {
        if (torch::cuda_is_available()) "cuda" else "mps"
    } else {
        "cpu"
    }

    ws   <- window_size
    leg  <- ws %/% 2L
    npix <- img_nrow * img_ncol
    box  <- metric %in% c(
        "contrast", "dissimilarity", "homogeneity", "mean", "variance",
        "std", "correlation"
    )
    # float64 on the box path: E[x^2] - mean^2 loses digits in float32 for
    # grey levels up to 1000; the pair path only compares grey levels
    dtype <- if (box) torch::torch_float64() else torch::torch_float32()

    # Build input tensor [1, 1, img_nrow, img_ncol] from grey-level values.
    # Truncate to integer first so pair-based statistics operate on discrete grey
    # levels as the GLCM approach.
    x <- torch::torch_tensor(
        matrix(as.integer(unlist(m)), nrow = 1L),
        dtype  = dtype,
        device = device
    )$view(c(1L, 1L, img_nrow, img_ncol))
    x_pad <- torch::nnf_pad(x, pad = c(leg, leg, leg, leg), mode = "reflect")
    rm(x)

    # The pair path unfolds the windows: wins[p, r, c] = grey level at window
    # (r,c) for pixel p, shape [npix, ws, ws]; the box path needs none
    wins <- NULL
    if (!box) {
        wins <- torch::nnf_unfold(
            x_pad,
            kernel_size = ws,
            stride      = 1L,
            padding     = 0L
        )$squeeze(1L)$t()$view(c(npix, ws, ws))
        rm(x_pad)
    }
    hp <- img_nrow + 2L * leg
    wp <- img_ncol + 2L * leg

    # Metric per angle, added into a running sum, then averaged
    result <- NULL
    for (theta in angles) {
        # Offset_row = round(sin), offset_col = round(cos)
        dr <- as.integer(round(sin(theta)))   # row offset (positive = downward)
        dc <- as.integer(round(cos(theta)))   # col offset (positive = rightward)
        metric_val <- if (box) {
            # the mean over a window of f(x[q], x[q + offset]) is a box
            # average of the image f over the valid centres
            r0 <- max(0L, -dr)
            c0 <- max(0L, -dc)
            r1 <- hp - max(0L, dr)
            c1 <- wp - max(0L, dc)
            a <- x_pad[1L, 1L, (r0 + 1L):r1, (c0 + 1L):c1]
            b <- x_pad[1L, 1L, (r0 + dr + 1L):(r1 + dr),
                       (c0 + dc + 1L):(c1 + dc)]
            kernel <- c(ws - abs(dr), ws - abs(dc))
            pool <- function(f) {
                torch::nnf_avg_pool2d(
                    f$unsqueeze(1L)$unsqueeze(1L),
                    kernel_size = kernel, stride = 1L
                )$view(c(img_nrow, img_ncol))
            }
            switch(metric,
                contrast = pool((a - b)^2),
                dissimilarity = pool((a - b)$abs()),
                homogeneity = pool(1.0 / (1.0 + (a - b)^2)),
                mean = pool((a + b) / 2.0),
                variance = pool((a^2 + b^2) / 2.0) - pool((a + b) / 2.0)^2,
                std = (
                    pool((a^2 + b^2) / 2.0) - pool((a + b) / 2.0)^2
                )$clamp(min = 0.0)$sqrt(),
                correlation = {
                    mu  <- pool((a + b) / 2.0)
                    var <- pool((a^2 + b^2) / 2.0) - mu^2
                    cov <- pool(a * b) - mu^2
                    torch::torch_where(
                        var$lt(1e-15),
                        torch::torch_ones_like(var),
                        cov / var
                    )
                }
            )$reshape(npix)
        } else {
            # slice bounds for centre positions
            r_lo_c <- 1L + max(0L, -dr)  # starting row depending on angle
            r_hi_c <- ws  - max(0L,  dr) # ending row depending on angle
            c_lo_c <- 1L + max(0L, -dc)  # starting col depending on angle
            c_hi_c <- ws  - max(0L,  dc) # ending col depending on angle
            # number of valid pairs (double this after making matrix symmetric)
            n_pairs_half <- (r_hi_c - r_lo_c + 1L) * (c_hi_c - c_lo_c + 1L)
            ctr_half <- wins[, r_lo_c:r_hi_c, c_lo_c:c_hi_c
                        ]$reshape(c(npix, n_pairs_half))
            nbr_half <- wins[, (r_lo_c + dr):(r_hi_c + dr),
                                (c_lo_c + dc):(c_hi_c + dc)
                        ]$reshape(c(npix, n_pairs_half))
            # Symmetric GLCM: add both (v_i, v_j) and (v_j, v_i)
            ctr <- torch::torch_cat(list(ctr_half, nbr_half), dim = 2L)
            nbr <- torch::torch_cat(list(nbr_half, ctr_half), dim = 2L)
            rm(ctr_half, nbr_half)
            val <- .texture_torch_glcm_asm(
                ctr, nbr, 2L * n_pairs_half, n_grey, npix, device
            )
            rm(ctr, nbr)
            # frees the pair tensors of this angle before the next one
            gc()
            if (metric == "energy") val$sqrt() else val
        }
        result <- if (is.null(result)) metric_val else result + metric_val
    }
    result <- result / length(angles)

    # Return as [npix, 1] numeric matrix (compatible with .raster_write_block)
    as.matrix(
        result$unsqueeze(2L)$cpu()$to(dtype = torch::torch_float64())
    )
}

#' @title Torch-backed GLCM texture function environment factory
#' @name .texture_torch_functions
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Drop-in replacement for \code{.texture_functions()} from
#' \file{api_texture.R}.  Returns an environment containing the same nine
#' \code{glcm_*} names.  Each function closes over its parameters and
#' delegates to \code{.texture_torch_glcm_compute()}.  The returned environment is
#' compatible with \code{eval(expr, envir = values, enclos = ...)}.
#'
#' @param window_size  Odd integer side of the square neighbourhood.
#' @param angles       Numeric vector of direction angles in radians.
#' @param img_nrow     Number of rows in the block.
#' @param img_ncol     Number of columns in the block.
#' @param n_grey       Number of grey levels (max_value of texture config).
#' @return An environment containing \code{glcm_contrast},
#'         \code{glcm_dissimilarity}, \code{glcm_homogeneity},
#'         \code{glcm_energy}, \code{glcm_asm}, \code{glcm_mean},
#'         \code{glcm_variance}, \code{glcm_std}, \code{glcm_correlation}.
.texture_torch_functions <- function(window_size, angles, img_nrow,
                                     img_ncol, n_grey) {
    # Factory to capture metric string in each closure
    make_fn <- function(metric) {
        force(metric)
        function(m) {
            .texture_torch_glcm_compute(
                m           = m,
                metric      = metric,
                window_size = window_size,
                angles      = angles,
                img_nrow    = img_nrow,
                img_ncol    = img_ncol,
                n_grey      = n_grey
            )
        }
    }
    list2env(
        list(
            glcm_contrast      = make_fn("contrast"),
            glcm_dissimilarity = make_fn("dissimilarity"),
            glcm_homogeneity   = make_fn("homogeneity"),
            glcm_energy        = make_fn("energy"),
            glcm_asm           = make_fn("asm"),
            glcm_mean          = make_fn("mean"),
            glcm_variance      = make_fn("variance"),
            glcm_std           = make_fn("std"),
            glcm_correlation   = make_fn("correlation")
        ),
        parent = parent.env(environment()),
        hash   = TRUE
    )
}
