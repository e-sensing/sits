# Peak RSS above the base of one torch call, in a new R process: an earlier
# call in the same process leaves freed memory that the allocator reuses
texture_peak_worker <- function(what, side, window_size, metric, n_grey) {
    status <- function(field) {
        line <- grep(paste0("^", field, ":"), readLines("/proc/self/status"),
            value = TRUE)
        as.numeric(gsub("[^0-9]", "", line)) * 1024
    }
    glcm <- get(".texture_torch_glcm_compute", asNamespace("sits"))
    # loads torch and starts its threads before the base is read
    invisible(glcm(m = rep(0L, 64L), metric = "asm", window_size = 3L,
        angles = 0, img_nrow = 8L, img_ncol = 8L, n_grey = 64L))
    set.seed(43L)
    img <- as.integer(sample(0L:(n_grey - 1L), side * side, replace = TRUE))
    invisible(gc())
    base <- status("VmRSS")
    writeLines("5", "/proc/self/clear_refs")
    if (what == "known") {
        # ones, not zeros, so every page is written
        x <- torch::torch_ones(side * side, dtype = torch::torch_float64())
        rm(x)
    } else {
        invisible(glcm(m = img, metric = metric, window_size = window_size,
            angles = c(0, pi / 4, pi / 2, 3 * pi / 4), img_nrow = side,
            img_ncol = side, n_grey = n_grey))
    }
    status("VmHWM") - base
}
environment(texture_peak_worker) <- globalenv()
# one sits worker per measure, set up by sits as in a real call
texture_fresh_peak <- function(what, ...) {
    started <- .parallel_start(workers = 1L, force = TRUE)
    on.exit(.parallel_stop(started), add = TRUE)
    if (!started) {
        stop("a sits cluster is open, so the worker would not be new")
    }
    .parallel_map(list(what), texture_peak_worker, ...)[[1L]]
}
# the peak RSS of a process can be reset only on Linux
texture_skip_no_peak <- function() {
    testthat::skip_on_os(c("windows", "mac", "solaris"))
    ok <- tryCatch({
        writeLines("5", "/proc/self/clear_refs")
        TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) testthat::skip("the peak RSS of the process cannot be reset here")
}
test_that("torch glcm_compute matches C++ for all metrics (single angle)", {
    skip_if_not_installed("torch")

    set.seed(7321)
    nrows       <- 20L
    ncols       <- 20L
    window_size <- 5L
    n_grey      <- 64L   # few grey levels, so the test is fast
    angles      <- 0.0

    # Synthetic grey-level image; values in [0, n_grey - 1] (matching C++ check)
    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))

    check_metric <- function(metric, cpp_fn) {
        cpp_result <- cpp_fn(
            x           = img,
            angles      = angles,
            nrows       = nrows,
            ncols       = ncols,
            n_grey      = n_grey,
            window_size = window_size
        )
        torch_result <- .texture_torch_glcm_compute(
            m           = img,
            metric      = metric,
            window_size = window_size,
            angles      = angles,
            img_nrow    = nrows,
            img_ncol    = ncols,
            n_grey      = n_grey
        )
        # the C++ mirror and torch reflect agree, border included
        max_diff <- max(abs(cpp_result - torch_result[, 1L]))
        expect_lt(max_diff, 1e-3,
            label = paste("max|diff| for", metric))
    }

    check_metric("contrast",      C_glcm_contrast)
    check_metric("dissimilarity", C_glcm_dissimilarity)
    check_metric("homogeneity",   C_glcm_homogeneity)
    check_metric("mean",          C_glcm_mean)
    check_metric("variance",      C_glcm_variance)
    check_metric("std",           C_glcm_std)
    check_metric("correlation",   C_glcm_correlation)
    check_metric("asm",           C_glcm_asm)
    check_metric("energy",        C_glcm_energy)
})

test_that("torch glcm_compute matches C++ for multiple angles", {
    skip_if_not_installed("torch")

    set.seed(8103)
    nrows       <- 20L
    ncols       <- 20L
    window_size <- 5L
    n_grey      <- 64L
    angles      <- c(0.0, pi / 2)

    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))

    cpp_result <- C_glcm_contrast(
        x           = img,
        angles      = angles,
        nrows       = nrows,
        ncols       = ncols,
        n_grey      = n_grey,
        window_size = window_size
    )
    torch_result <- .texture_torch_glcm_compute(
        m           = img,
        metric      = "contrast",
        window_size = window_size,
        angles      = angles,
        img_nrow    = nrows,
        img_ncol    = ncols,
        n_grey      = n_grey
    )

    max_diff <- max(abs(cpp_result - torch_result[, 1L]))
    expect_lt(max_diff, 1e-3)
})

test_that("torch glcm_compute matches C++ on a grid of windows and angles", {
    skip_if_not_installed("torch")
    set.seed(5113)
    # rows differ from columns, so a transposed offset or crop shows
    nrows <- 17L
    ncols <- 29L
    cpp <- list(
        contrast = C_glcm_contrast, dissimilarity = C_glcm_dissimilarity,
        homogeneity = C_glcm_homogeneity, mean = C_glcm_mean,
        variance = C_glcm_variance, std = C_glcm_std,
        correlation = C_glcm_correlation, asm = C_glcm_asm,
        energy = C_glcm_energy
    )
    angle_sets <- c(as.list(c(0, pi / 4, pi / 2, 3 * pi / 4)),
        list(c(0, pi / 4, pi / 2, 3 * pi / 4)))
    for (metric in names(cpp)) {
        # the pair codes of ASM grow with the grey levels squared
        n_grey <- if (metric %in% c("asm", "energy")) 64L else 1000L
        img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols,
            replace = TRUE))
        for (window_size in c(3L, 5L, 9L)) {
            for (angles in angle_sets) {
                want <- cpp[[metric]](
                    x = img, angles = angles, nrows = nrows, ncols = ncols,
                    n_grey = n_grey, window_size = window_size
                )
                got <- .texture_torch_glcm_compute(
                    m = img, metric = metric, window_size = window_size,
                    angles = angles, img_nrow = nrows, img_ncol = ncols,
                    n_grey = n_grey
                )
                # float32 holds about 7 digits, so the bound grows with the
                # value: contrast reaches 1e5 at 1000 grey levels
                expect_lt(
                    max(abs(want - got[, 1L]) / (1 + abs(want))), 1e-5,
                    label = sprintf("%s, window %d, angles %s", metric,
                        window_size, paste(round(angles, 2), collapse = " "))
                )
            }
        }
    }
})

test_that("torch variance and correlation keep their digits on a flat block", {
    skip_if_not_installed("torch")
    set.seed(5114)
    # grey levels near 900 with a spread of 3, as a dense forest in NDVI:
    # E[x^2] - mean^2 cancels 6 digits of 8e5 in float32
    nrows <- 17L
    ncols <- 29L
    img <- 900L + as.integer(sample(0L:3L, nrows * ncols, replace = TRUE))
    cpp <- list(
        variance = C_glcm_variance, std = C_glcm_std,
        correlation = C_glcm_correlation, mean = C_glcm_mean
    )
    for (metric in names(cpp)) {
        for (window_size in c(3L, 7L)) {
            angles <- c(0, pi / 4, pi / 2, 3 * pi / 4)
            want <- cpp[[metric]](
                x = img, angles = angles, nrows = nrows, ncols = ncols,
                n_grey = 1000L, window_size = window_size
            )
            got <- .texture_torch_glcm_compute(
                m = img, metric = metric, window_size = window_size,
                angles = angles, img_nrow = nrows, img_ncol = ncols,
                n_grey = 1000L
            )
            expect_lt(
                max(abs(want - got[, 1L]) / (1 + abs(want))), 1e-5,
                label = sprintf("%s, window %d", metric, window_size)
            )
        }
    }
})

test_that("torch variance stays within the float32 limit on a mixed block", {
    skip_if_not_installed("torch")
    set.seed(5115)
    # flat forest near 900 beside flat water near 100: the block mean is near
    # 500, so one shift per block leaves the forest 400 levels from it
    nrows <- 17L
    ncols <- 29L
    img <- matrix(
        100L + as.integer(sample(0L:3L, nrows * ncols, replace = TRUE)),
        nrows, ncols, byrow = TRUE
    )
    img[, 1L:14L] <- img[, 1L:14L] + 800L
    img <- as.integer(t(img))
    cpp <- list(
        variance = C_glcm_variance, std = C_glcm_std,
        correlation = C_glcm_correlation
    )
    for (metric in names(cpp)) {
        for (window_size in c(3L, 7L)) {
            angles <- c(0, pi / 4, pi / 2, 3 * pi / 4)
            want <- cpp[[metric]](
                x = img, angles = angles, nrows = nrows, ncols = ncols,
                n_grey = 1000L, window_size = window_size
            )
            got <- .texture_torch_glcm_compute(
                m = img, metric = metric, window_size = window_size,
                angles = angles, img_nrow = nrows, img_ncol = ncols,
                n_grey = 1000L
            )
            # the limit of float32 that the docs of sits_texture() state:
            # 1.8% at most on this block, measured on 2026-11-26
            expect_lt(
                max(abs(want - got[, 1L]) / (1 + abs(want))), 0.05,
                label = sprintf("%s, window %d", metric, window_size)
            )
        }
    }
})

test_that("torch glcm_compute holds box metrics in memory free of the window", {
    skip_on_cran()
    skip_if_not_installed("torch")
    texture_skip_no_peak()
    peak <- function(window_size) {
        texture_fresh_peak("glcm", side = 256L, window_size = window_size,
            metric = "contrast", n_grey = 1000L)
    }
    # a block of 256 x 256 in float64 is 0.5 MB; the old pairs of window 15
    # held about 1 GB
    peak5 <- peak(5L)
    peak15 <- peak(15L)
    expect_lt(peak15 - peak5, 50e6)
})

test_that("the peak RSS of a new process reads a tensor of known size", {
    skip_on_cran()
    skip_if_not_installed("torch")
    texture_skip_no_peak()
    # 7000^2 float64 values, 392 MB
    peak <- texture_fresh_peak("known", side = 7000L, window_size = 3L,
        metric = "asm", n_grey = 64L)
    expect_gt(peak, 0.9 * 7000^2 * 8)
    expect_lt(peak, 1.2 * 7000^2 * 8)
})

test_that("torch ASM holds its memory within the bound of the pairs", {
    skip_on_cran()
    skip_if_not_installed("torch")
    texture_skip_no_peak()
    side <- 512L
    peak <- function(window_size) {
        texture_fresh_peak("glcm", side = side, window_size = window_size,
            metric = "asm", n_grey = 64L)
    }
    # the base of the GPU rule of size(): long codes of the axial pairs
    codes <- function(window_size) {
        side^2 * 2 * window_size * (window_size - 1) * 8
    }
    bloat <- .conf(c("texture_options", "memory", "gpu", "gpu_bloat_pairs"))
    # only the bound: the CPU allocator keeps freed memory, so the peaks are
    # not linear here; the GPU series of 13 runs shows the linearity
    expect_lte(peak(9L), bloat * codes(9L))
    expect_lte(peak(15L), bloat * codes(15L))
})

test_that("torch ASM calls gc() at most once per angle", {
    skip_if_not_installed("torch")
    set.seed(44L)
    # 40000 pixels: a kernel that splits the pixels in batches calls gc()
    # more than once per angle
    side <- 200L
    img <- as.integer(sample(0L:63L, side * side, replace = TRUE))
    calls <- new.env()
    calls$n <- 0L
    suppressMessages(trace(
        "gc", tracer = bquote(assign("n", .(calls)$n + 1L, envir = .(calls))),
        where = baseenv(), print = FALSE
    ))
    on.exit(suppressMessages(untrace("gc", where = baseenv())), add = TRUE)
    invisible(.texture_torch_glcm_compute(
        m = img, metric = "asm", window_size = 5L,
        angles = c(0, pi / 4, pi / 2, 3 * pi / 4), img_nrow = side,
        img_ncol = side, n_grey = 64L
    ))
    expect_lte(calls$n, 4L)
})

test_that("torch ASM keeps a grey level equal to n_grey in its pixel", {
    skip_if_not_installed("torch")
    set.seed(46L)
    nrows <- 17L
    ncols <- 29L
    n_grey <- 64L
    # a band value equal to its maximum becomes the grey level n_grey
    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))
    at <- function(r, c) (r - 1L) * ncols + c
    img[at(5L, 5L)] <- n_grey
    img[at(nrows, ncols)] <- n_grey
    # window 3 reaches one pixel; C++ drops the pairs of level n_grey, so
    # only the pixels away from both are compared
    near <- outer(seq_len(nrows), seq_len(ncols), function(r, c) {
        (abs(r - 5L) <= 1L & abs(c - 5L) <= 1L) |
            (r >= nrows - 1L & c >= ncols - 1L)
    })
    far <- !as.vector(t(near))
    angles <- c(0, pi / 4, pi / 2, 3 * pi / 4)
    cpp <- list(asm = C_glcm_asm, energy = C_glcm_energy)
    for (metric in names(cpp)) {
        want <- cpp[[metric]](
            x = img, angles = angles, nrows = nrows, ncols = ncols,
            n_grey = n_grey, window_size = 3L
        )
        got <- .texture_torch_glcm_compute(
            m = img, metric = metric, window_size = 3L, angles = angles,
            img_nrow = nrows, img_ncol = ncols, n_grey = n_grey
        )
        expect_lt(max(abs(want[far] - got[far, 1L])), 1e-3, label = metric)
    }
})

test_that("torch ASM and energy match C++ with 1001 grey levels", {
    skip_if_not_installed("torch")
    set.seed(45L)
    nrows <- 17L
    ncols <- 29L
    n_grey <- 1001L
    # the largest codes test the pixel offset and its floor division
    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))
    img[1:40] <- n_grey - 1L
    angles <- c(0, pi / 4, pi / 2, 3 * pi / 4)
    cpp <- list(asm = C_glcm_asm, energy = C_glcm_energy)
    for (metric in names(cpp)) {
        for (window_size in c(3L, 9L)) {
            want <- cpp[[metric]](
                x = img, angles = angles, nrows = nrows, ncols = ncols,
                n_grey = n_grey, window_size = window_size
            )
            got <- .texture_torch_glcm_compute(
                m = img, metric = metric, window_size = window_size,
                angles = angles, img_nrow = nrows, img_ncol = ncols,
                n_grey = n_grey
            )
            expect_lt(max(abs(want - got[, 1L])), 1e-3,
                label = sprintf("%s, window %d", metric, window_size))
        }
    }
})
