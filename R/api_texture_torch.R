#' @title Compute the Angular Second Moment from half-pair tensors
#' @name .texture_torch_glcm_asm
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description
#' \eqn{\mathrm{ASM} = \sum_{i,j} P_{i,j}^2}{ASM = sum_ij P[i,j]^2},
#' where \eqn{P} is the normalised GLCM.
#' Each pair is encoded as an integer \code{i * base + j}, with the pair
#' and its mirror \code{j * base + i}, plus the offset
#' \code{pixel * base^2}, so the codes of different pixels never meet;
#' \code{base = n_grey + 1}, since a band value at its maximum gives the grey
#' level \code{n_grey}.
#' The codes of each pixel are sorted, the runs of equal codes counted and
#' the squared counts summed per pixel, in one pass for the whole block.
#'
#' @param ctr_half   Tensor \code{[npix, half]} of centre grey levels.
#' @param nbr_half   Tensor \code{[npix, half]} of neighbour grey levels.
#' @param n_grey     Number of grey levels.
#' @param npix       Number of pixels in the block.
#' @param device     torch device string (\code{"cpu"} or \code{"cuda"}).
#' @return Float32 tensor \code{[npix]} of per-pixel ASM values.
.texture_torch_glcm_asm <- function(ctr_half, nbr_half, n_grey, npix, device) {
    long <- torch::torch_long()
    # grey levels go from 0 to n_grey: a band value at its maximum gives
    # n_grey, and a code base of n_grey would move its pairs to the next pixel
    base <- as.integer(n_grey) + 1L
    half <- ctr_half$size(2L)
    total <- 2L * half
    # float32 holds each code exactly up to 2^24; the writes are in place
    # because torch frees its tensors only when R collects them
    codes <- torch::torch_empty(npix, total, dtype = long, device = device)
    codes[, 1L:half]$copy_(ctr_half * base + nbr_half)
    codes[, (half + 1L):total]$copy_(nbr_half * base + ctr_half)
    # codes of different pixels never meet; npix * base^2 fits in int64
    codes$add_(torch::torch_arange(
        0L, npix - 1L, dtype = long, device = device
    )$unsqueeze(2L) * (base * base))
    # with the offsets the sorted rows, flattened, are sorted
    flat <- codes$sort(dim = 2L)[[1L]]$reshape(-1L)
    rm(codes)
    runs <- torch::torch_unique_consecutive(flat, return_counts = TRUE)
    rm(flat)
    pixel <- torch::torch_div(
        runs[[1L]], base * base, rounding_mode = "floor"
    )
    # index_add_ counts from 1 in R
    result <- torch::torch_zeros(
        npix, dtype = torch::torch_float32(), device = device
    )
    result$index_add_(
        1L, pixel$to(dtype = long) + 1L,
        runs[[3L]]$to(dtype = torch::torch_float32())^2
    )
    rm(runs, pixel)
    result / (as.numeric(total)^2)
}

#' @title Compute a single GLCM metric for a raster block using torch
#' @name .texture_torch_glcm_compute
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
#'
#' @description
#' Drop-in replacement for the per-metric C++ functions
#' (\code{C_glcm_contrast}, \code{C_glcm_dissimilarity}, ...).
#' Replicates the algorithm in \file{src/glcm_fns.cpp}:
#' \enumerate{
#'   \item For each angle \eqn{\theta}, compute
#'         \eqn{d_r = \text{round}(\sin\theta)},
#'         \eqn{d_c = \text{round}(\cos\theta)}.
#'   \item Extract all valid centre-neighbour pairs from the local
#'         \code{window_size} \eqn{\times} \code{window_size}
#'         neighbourhood, including the symmetric (neighbour to centre)
#'         direction.
#'   \item Derive the requested GLCM statistic directly from the pair lists
#'         (exact for all metrics; see mathematical notes below).
#'   \item Average over angles.
#' }
#'
#' \strong{Padding note:} the border is mirrored without repeating the
#' edge pixel, as the C++ code does.
#'
#' \strong{Mathematical equivalence (pair-based formulas):}
#' All metrics except ASM/Energy are linear in \eqn{P_{i,j}}, so
#' \eqn{\sum_{i,j} P_{i,j}\,f(i,j) = \frac{1}{T}\sum_k f(\text{ctr}_k, \text{nbr}_k)},
#' where T is the total number of pairs and the sum runs over all pair
#' occurrences (not unique pairs).
#' ASM counts runs of equal pair codes in one pass (see \code{.texture_torch_glcm_asm}).
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
    device <- .texture_torch_device()

    ws   <- window_size
    leg  <- ws %/% 2L
    npix <- img_nrow * img_ncol
    box  <- metric %in% c(
        "contrast", "dissimilarity", "homogeneity", "mean", "variance",
        "std", "correlation"
    )
    # float32 on every path, as the other torch code of sits
    dtype <- torch::torch_float32()

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
    # E[x^2] - mean^2 cancels digits as the grey levels grow, so the box
    # path works on the levels less their block mean, rounded to keep them
    # whole; only the mean metric moves, and it is moved back
    shift <- 0
    if (box) {
        shift <- round(x_pad$mean()$item())
        x_pad <- x_pad - shift
    }

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
                mean = pool((a + b) / 2.0) + shift,
                # float32 leaves a few thousandths, below 0 too, on a flat
                # window
                variance = (
                    pool((a^2 + b^2) / 2.0) - pool((a + b) / 2.0)^2
                )$clamp(min = 0.0),
                std = (
                    pool((a^2 + b^2) / 2.0) - pool((a + b) / 2.0)^2
                )$clamp(min = 0.0)$sqrt(),
                correlation = {
                    mu  <- pool((a + b) / 2.0)
                    m2  <- pool((a^2 + b^2) / 2.0)
                    var <- m2 - mu^2
                    cov <- pool(a * b) - mu^2
                    # float32 holds about 7 digits of the second moment, so
                    # a smaller variance is a flat window, as 0 is in C++
                    torch::torch_where(
                        var$le(1e-7 * (m2 + 1.0)),
                        torch::torch_ones_like(var),
                        (cov / var)$clamp(min = -1.0, max = 1.0)
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
            # the symmetric GLCM adds both (v_i, v_j) and (v_j, v_i)
            val <- .texture_torch_glcm_asm(
                ctr_half, nbr_half, n_grey, npix, device
            )
            rm(ctr_half, nbr_half)
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
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @keywords internal
#' @noRd
#'
#' @description
#' Drop-in replacement for \code{.texture_cpp_functions()} from
#' \file{api_texture_cpp.R}.  Returns an environment containing the same nine
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
