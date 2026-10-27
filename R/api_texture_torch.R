#' @title Check if the torch texture implementation is available
#' @name .texture_torch_available
#' @keywords internal
#' @noRd
#' @description
#' Returns \code{TRUE} when torch and its native dependencies are functional,
#' unless the environment variable \code{SITS_TEXTURE_FORCE_CPP} is set to
#' \code{TRUE}, in which case the C++ backend is always used.
#'
#' @return A logical scalar.
.texture_torch_available <- function() {
    force_cpp <- Sys.getenv("SITS_TEXTURE_FORCE_CPP", unset = "FALSE")
    if (toupper(force_cpp) == "TRUE") {
        return(FALSE)
    }
    .torch_is_functional()
}

#' @title Estimate memory used by torch GLCM texture windows
#' @name .texture_torch_block_memsize
#' @keywords internal
#' @noRd
#' @description
#' Estimates the GPU/CPU memory needed to materialise the neighbourhood
#' windows tensor \code{wins} of shape \code{[npix, ws, ws]} (float32) for
#' one raster block.  The pair tensors built from \code{wins} per angle are
#' smaller and released between angles, so \code{wins} is the dominant
#' allocation — identical in shape to the unfolded windows in the smooth
#' torch backend, so the same formula and bloat factor apply.
#'
#' @param block_size  Number of pixels in the block, including overlap.
#' @param window_size Side length of the square neighbourhood.
#' @return Estimated memory in GB.
.texture_torch_block_memsize <- function(block_size, window_size) {
    .jobs_block_memsize(
        block_size = block_size,
        npaths     = window_size^2,
        nbytes     = 4L,
        proc_bloat = .conf("processing_bloat_smooth_torch")
    )
}

#' @title Compute the Angular Second Moment from pair tensors
#' @name .texture_torch_glcm_asm
#' @keywords internal
#' @noRd
#' @description
#' ASM = Σᵢⱼ P[i,j]², where P is the normalised GLCM.
#' Equivalently: \code{(1/total²) Σₖ Σₗ [pair_code_k == pair_code_l]}, where
#' each pair is encoded as an integer \code{i * n_grey + j}.
#' The double sum is computed in pixel mini-batches to keep GPU memory bounded
#' (~100 MB per batch for \code{batch_size = 1024} and \code{total ≤ 160}).
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
    batch_size <- 1024L
    # Encode each (i, j) pair as a single non-negative integer
    pair_code <- (
        ctr$to(dtype = torch::torch_long()) * as.integer(n_grey) +
        nbr$to(dtype = torch::torch_long())
    )  # [npix, total]

    result <- torch::torch_zeros(
        npix,
        dtype  = torch::torch_float32(),
        device = device
    )
    for (b_start in seq(1L, npix, by = batch_size)) {
        b_end <- min(npix, b_start + batch_size - 1L)
        pc_b  <- pair_code[b_start:b_end, ]  # [batch, total]
        # Pairwise equality → [batch, total, total]; sum = Σₖ Σₗ [pair_k == pair_l]
        eq <- (pc_b$unsqueeze(3L) == pc_b$unsqueeze(2L))$to(
            dtype = torch::torch_float32()
        )
        result[b_start:b_end] <- eq$sum(dim = c(2L, 3L)) / (as.numeric(total)^2)
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
#' \code{.texture_torch_feature()}.
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

    # Build input tensor [1, 1, img_nrow, img_ncol] from grey-level values.
    # Truncate to integer first so pair-based statistics operate on discrete grey
    # levels as the GLCM approach.
    x <- torch::torch_tensor(
        matrix(as.integer(unlist(m)), nrow = 1L),
        dtype  = torch::torch_float32(),
        device = device
    )$view(c(1L, 1L, img_nrow, img_ncol))

    # Reflect-pad and unfold into local windows
    # wins[p, r, c] = grey level at window (r,c) for pixel p. wins shape: [npix, ws, ws]
    x_pad <- torch::nnf_pad(x, pad = c(leg, leg, leg, leg), mode = "reflect")
    wins  <- torch::nnf_unfold(
        x_pad,
        kernel_size = ws,
        stride      = 1L,
        padding     = 0L
    )$squeeze(1L)$t()$view(c(npix, ws, ws))

    # Compute metric per angle, then average
    angle_results <- purrr::map(angles, function(theta) {
        # Offset_row = round(sin), offset_col = round(cos)
        dr <- as.integer(round(sin(theta)))   # row offset (positive = downward)
        dc <- as.integer(round(cos(theta)))   # col offset (positive = rightward)
        # slice bounds for centre positions
        r_lo_c <- 1L + max(0L, -dr)  # starting row depending on angle
        r_hi_c <- ws  - max(0L,  dr) # ending row depending on angle
        c_lo_c <- 1L + max(0L, -dc)  # starting col depending on sngle
        c_hi_c <- ws  - max(0L,  dc) # ending col depending on angle
        # number of valid pairs (double this after making matrix symmetric)
        n_pairs_half <- (r_hi_c - r_lo_c + 1L) * (c_hi_c - c_lo_c + 1L)
        # get center part of pairs depending on angle
        ctr_half <- wins[, r_lo_c:r_hi_c, c_lo_c:c_hi_c
                    ]$reshape(c(npix, n_pairs_half))
        # get end part of pairs depending on angle
        nbr_half <- wins[, (r_lo_c + dr):(r_hi_c + dr),
                            (c_lo_c + dc):(c_hi_c + dc)
                    ]$reshape(c(npix, n_pairs_half))
        # Symmetric GLCM: add both (v_i, v_j) and (v_j, v_i)
        ctr   <- torch::torch_cat(list(ctr_half, nbr_half), dim = 2L)  # [npix, total]
        nbr   <- torch::torch_cat(list(nbr_half, ctr_half), dim = 2L)
        total <- 2L * n_pairs_half
        # Calc diff statistic used for most metrics. [npix, total]
        diff <- ctr - nbr
        # Compute the requested metric for this angle
        metric_val <- switch(metric,
            contrast = {
                (diff^2)$mean(dim = 2L)
            },
            dissimilarity = {
                diff$abs()$mean(dim = 2L)
            },
            homogeneity = {
                (1.0 / (1.0 + diff^2))$mean(dim = 2L)
            },
            mean = {
                ctr$mean(dim = 2L)
            },
            variance = {
                mu <- ctr$mean(dim = 2L)
                ((ctr - mu$unsqueeze(2L))^2)$mean(dim = 2L)
            },
            std = {
                mu <- ctr$mean(dim = 2L)
                (((ctr - mu$unsqueeze(2L))^2)$mean(dim = 2L))$sqrt()
            },
            correlation = {
                # return 1 when var < 1e-15
                mu  <- ctr$mean(dim = 2L)
                var <- ((ctr - mu$unsqueeze(2L))^2)$mean(dim = 2L)
                cov <- (ctr * nbr)$mean(dim = 2L) - mu^2
                torch::torch_where(
                    var$lt(1e-15),
                    torch::torch_ones_like(var),
                    cov / var
                )
            },
            asm = {
                .texture_torch_glcm_asm(ctr, nbr, total, n_grey, npix, device)
            },
            energy = {
                # sqrt(ASM)
                .texture_torch_glcm_asm(ctr, nbr, total, n_grey, npix, device)$sqrt()
            }
        )
        metric_val  # shape: [npix]
    })

    # Average across angles
    result <- if (length(angle_results) == 1L) {
        angle_results[[1L]]
    } else {
        torch::torch_stack(angle_results, dim = 2L)$mean(dim = 2L)
    }

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

#' @title Apply torch GLCM texture measures to a raster tile
#' @name .texture_torch_feature
#' @keywords internal
#' @noRd
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#'
#' @description
#' Mirrors \code{.texture_feature()} from \file{api_texture.R} with a single
#' change: the inner call to \code{.texture_functions()} is replaced by
#' \code{.texture_torch_functions()}, routing GLCM computation through torch
#' tensor operations instead of C++.
#'
#' @param feature    Subset of a data cube containing one tile–time step.
#' @param block      Block dimensions for chunked I/O.
#' @param window_size  Side length of the neighbourhood (odd integer).
#' @param angles     Direction angles in radians.
#' @param expr       Parsed band expression (output of \code{.apply_capture_expression}).
#' @param out_band   Name of the output band.
#' @param in_bands   Names of the input bands consumed by \code{expr}.
#' @param overlap    Overlap in pixels between adjacent chunks.
#' @param output_dir Directory for output files.
#' @param progress   Show progress bar?
#' @return A tile tibble row for the newly created band.
.texture_torch_feature <- function(feature, block, window_size, angles, expr,
                                   out_band, in_bands, overlap, output_dir,
                                   progress) {
    # Output file
    out_file <- .file_eo_name(
        tile = feature, band = out_band,
        date = .tile_start_date(feature), output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        .check_recovery()
        feature <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(feature)),
            bands = out_band, date = .tile_start_date(feature),
            base_tile = feature, update_bbox = FALSE
        )
        return(feature)
    }
    # Remove remaining incomplete block files
    unlink(out_file)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = feature, overlap = overlap, block = block
    )
    # Get band configuration
    band_conf <- .tile_band_conf(tile = feature, band = out_band)
    if (.has_not(band_conf)) {
        band_conf <- .conf("default_values", "INT2S")
    }
    # Get texture options
    texture_conf <- .conf("texture_options")
    # Process jobs in parallel
    block_files <- .jobs_map_parallel(chunks, function(chunk) {
        block <- .block(chunk)
        block_files <- .file_block_name(
            pattern    = .file_pattern(out_file),
            block      = block,
            output_dir = output_dir
        )
        if (all(.raster_is_valid(block_files))) {
            return(block_files)
        }
        # Read and preprocess values
        values <- .apply_data_read(
            tile = feature, block = block, in_bands = in_bands
        )
        values[[1L]] <- C_fill_na(as.matrix(values[[1L]]), 0.0)
        scale <- .scale(band_conf)
        values <- values / scale
        max_value <- max(values)
        if (max_value > .max_value(band_conf)) {
            values[values > .max_value(band_conf)] <- .max_value(band_conf) - 1
        }
        min_value <- min(values)
        if (min_value < .min_value(band_conf)) {
            values[values < .min_value(band_conf)] <- .min_value(band_conf) + 1
        }
        # Normalise to [min_texture, max_texture] grey-level range
        values <- .texture_normalize(
            values = values,
            source = c(.min_value(band_conf), .max_value(band_conf)),
            dest   = c(.min_value(texture_conf), .max_value(texture_conf))
        )
        # Evaluate expression using torch-backed GLCM functions
        values <- eval(
            expr   = expr[[out_band]],
            envir  = values,
            enclos = .texture_torch_functions(
                window_size = window_size,
                angles      = angles,
                img_nrow    = block[["nrows"]],
                img_ncol    = block[["ncols"]],
                n_grey      = .max_value(texture_conf)
            )
        )
        # Apply offset if needed
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0.0) {
            values <- values - offset
        }
        crop_block <- .block(.chunks_no_overlap(chunk))
        .raster_write_block(
            files         = block_files,
            block         = block,
            bbox          = .bbox(chunk),
            values        = values,
            data_type     = .data_type(band_conf),
            missing_value = .miss_value(band_conf),
            crop_block    = crop_block
        )
        block_files
    }, progress = progress)
    # Merge blocks into a new eo_cube tile
    .tile_eo_merge_blocks(
        files       = out_file,
        bands       = out_band,
        band_conf   = band_conf,
        base_tile   = feature,
        block_files = block_files,
        multicores  = 1L,
        update_bbox = FALSE
    )
}
