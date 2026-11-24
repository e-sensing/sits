test_that("torch glcm_compute matches C++ for all metrics (single angle)", {
    skip_if_not_installed("torch")

    set.seed(7321)
    nrows       <- 20L
    ncols       <- 20L
    window_size <- 5L
    n_grey      <- 64L   # small so the batched ASM loop is fast and comparable
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
                expect_lt(
                    max(abs(want - got[, 1L])), 1e-3,
                    label = sprintf("%s, window %d, angles %s", metric,
                        window_size, paste(round(angles, 2), collapse = " "))
                )
            }
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

test_that("torch glcm_compute holds ASM in memory linear in the pairs", {
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
    bloat <- .conf(c("texture_options", "gpu", "gpu_bloat_pairs"))
    # only the bound: the CPU allocator keeps freed memory, so the peaks are
    # not linear here; the GPU series of 13 runs shows the linearity
    expect_lte(peak(9L), bloat * codes(9L))
    expect_lte(peak(15L), bloat * codes(15L))
})

test_that("torch ASM calls gc() once per angle, not once per batch", {
    skip_if_not_installed("torch")
    set.seed(44L)
    # 40000 pixels at window 5 exceed a batch of 2^20 / 40 codes
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

test_that(".texture_backend picks torch on a GPU and C++ otherwise", {
    gpu <- testthat::with_mocked_bindings(
        .texture_backend(gpu_memsize = 4),
        .texture_use_gpu = function() TRUE
    )
    expect_equal(gpu[["name"]], "gpu")
    expect_identical(gpu[["fns"]], .texture_torch_functions)
    cpu <- testthat::with_mocked_bindings(
        .texture_backend(gpu_memsize = 4),
        .texture_use_gpu = function() FALSE
    )
    expect_equal(cpu[["name"]], "cpu")
    expect_identical(cpu[["fns"]], .texture_functions)
})

test_that("sits_texture runs torch on the GPU in multicores workers", {
    skip_on_cran()
    skip_if_not_installed("torch")
    suppressMessages(sits_parallel(workers = 0L))
    feature <- texture_cube()
    out_dir <- texture_dir("texture-gpu-workers")
    marks <- texture_dir("texture-gpu-workers-marks")
    on.exit(unlink(c(out_dir, marks), recursive = TRUE), add = TRUE)
    texture_small_blocks()
    # the kernel still picks its device from the hardware, so it runs on
    # the CPU here
    testthat::local_mocked_bindings(
        .texture_use_gpu = function() TRUE,
        .texture_torch_functions = texture_marker(
            .texture_torch_functions, marks
        )
    )
    sits_texture(
        feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size  = 5L,
        multicores   = 2L,
        output_dir   = out_dir,
        progress     = FALSE
    )
    pids <- texture_marker_pids(marks)
    expect_length(pids, 2L)
    expect_false(Sys.getpid() %in% pids)
})

test_that("the GPU backend fits its workers and block in gpu_memsize", {
    bloat_box <- .conf(c("texture_options", "gpu", "gpu_bloat_box"))
    bloat_pairs <- .conf(c("texture_options", "gpu", "gpu_bloat_pairs"))
    # each worker holds a CUDA context besides its block
    worker <- .conf(c("texture_options", "gpu", "gpu_worker_memsize"))
    ram_bloat <- .conf(c("texture_options", "gpu", "ram_bloat"))
    # and the R process with torch in RAM besides its bands
    ram_worker <- .conf(c("texture_options", "gpu", "ram_worker_memsize"))
    expect_true(is.numeric(bloat_box) && bloat_box > 0)
    expect_true(is.numeric(bloat_pairs) && bloat_pairs > 0)
    expect_true(is.numeric(worker) && worker > 0)
    pixels <- function(block, window_size) {
        overlap <- ceiling(window_size / 2L) - 1L
        (block[["nrows"]] + 2L * overlap) * (block[["ncols"]] + 2L * overlap)
    }
    # GB on the GPU of one block: the box metrics hold float64 images of
    # the block; ASM and energy hold long codes of the pairs of each window
    need <- function(block, window_size, metrics) {
        if (any(c("glcm_asm", "glcm_energy") %in% metrics)) {
            pairs <- 2 * window_size * (window_size - 1)
            pixels(block, window_size) * pairs * 8 * bloat_pairs * 1e-9
        } else {
            pixels(block, window_size) * 8 * bloat_box * 1e-9
        }
    }
    # GB in RAM of one block on the GPU path: the bands in doubles
    ram_need <- function(block, window_size, npaths = 2L) {
        pixels(block, window_size) * npaths * 8 * ram_bloat * 1e-9
    }
    block <- c(nrows = 512L, ncols = 512L)
    image <- c(nrows = 8192L, ncols = 8192L)
    # memsize is large by default, so only the GPU limits the workers
    size <- function(gpu_memsize, window_size, metrics, multicores = 64L,
                     memsize = 10000) {
        backend <- testthat::with_mocked_bindings(
            .texture_backend(gpu_memsize = gpu_memsize, metrics = metrics),
            .texture_use_gpu = function() TRUE
        )
        backend[["size"]](
            block = block,
            overlap = ceiling(window_size / 2L) - 1L,
            npaths = 2L,
            window_size = window_size,
            image_size = image,
            memsize = memsize,
            multicores = multicores
        )
    }
    box <- "glcm_contrast"
    pairs <- c("glcm_contrast", "glcm_asm")
    # budgets in blocks of window 5, so no case reaches the 64 workers
    g_box <- 20 * (need(block, 5L, box) + worker)
    g_pairs <- 20 * (need(block, 5L, pairs) + worker)
    # the workers the budget allows, for each path and window
    for (metrics in list(box, pairs)) {
        g <- if (identical(metrics, box)) g_box else g_pairs
        for (window_size in c(5L, 15L)) {
            got <- size(gpu_memsize = g, window_size = window_size,
                metrics = metrics)
            expect_equal(
                got[["multicores"]],
                min(64L, floor(g / (need(block, window_size, metrics) +
                    worker)))
            )
            expect_lte(
                (need(got[["block"]], window_size, metrics) + worker) *
                    got[["multicores"]],
                g
            )
        }
    }
    # the window changes the box path only through the overlap; it changes
    # the pair path with its pairs
    box5 <- size(gpu_memsize = g_box, window_size = 5L, metrics = box)
    box15 <- size(gpu_memsize = g_box, window_size = 15L, metrics = box)
    expect_gt(box15[["multicores"]], 0.9 * box5[["multicores"]])
    pairs5 <- size(gpu_memsize = g_pairs, window_size = 5L, metrics = pairs)
    pairs15 <- size(gpu_memsize = g_pairs, window_size = 15L, metrics = pairs)
    expect_lt(pairs15[["multicores"]], pairs5[["multicores"]])
    # a smaller budget gives a smaller block, which still fits
    g_small <- worker + 3 * need(block, 5L, pairs)
    g_large <- worker + 30 * need(block, 5L, pairs)
    small <- size(gpu_memsize = g_small, window_size = 5L, metrics = pairs,
        multicores = 1L)
    large <- size(gpu_memsize = g_large, window_size = 5L, metrics = pairs,
        multicores = 1L)
    expect_lt(
        prod(small[["block"]][c("nrows", "ncols")]),
        prod(large[["block"]][c("nrows", "ncols")])
    )
    expect_lte(need(small[["block"]], 5L, pairs) + worker, g_small)
    expect_lte(need(large[["block"]], 5L, pairs) + worker, g_large)
    # RAM limits when memsize is the smaller budget; the call takes what it
    # adds to the main process and its merges before the workers: gpu7q1-3
    # on the server (2026-11-24), main 0.86 and merge 0.37 GB at most, 2
    # merges at once, worker 1.49 GB at most
    expect_equal(ram_worker, 1.5)
    expect_equal(.conf(c("texture_options", "gpu", "ram_main_memsize")), 0.9)
    expect_equal(.conf(c("texture_options", "gpu", "ram_merge_memsize")), 0.4)
    ram_call <- 0.9 + 2 * 0.4
    m <- ram_call + 10.5 * (ram_need(block, 5L) + ram_worker)
    ram <- size(gpu_memsize = 1000, window_size = 5L, metrics = box,
        memsize = m)
    expect_equal(ram[["multicores"]], 10L)
    expect_lte(
        (ram_need(ram[["block"]], 5L) + ram_worker) * ram[["multicores"]] +
            ram_call,
        m + 1e-9
    )
    # one block that does not fit the GPU names the GPU budget
    expect_error(
        size(gpu_memsize = 0.5 * need(block, 15L, pairs), window_size = 15L,
            metrics = pairs),
        "gpu_memsize"
    )
    # a block that fits only without the CUDA context does not fit either
    expect_error(
        size(gpu_memsize = need(block, 15L, pairs) + 0.5 * worker,
            window_size = 15L, metrics = pairs),
        "gpu_memsize"
    )
})

test_that(".conf_backend reads the key of a backend or its default", {
    texture_drop_backend("cpu")
    sits_env[["config"]][["texture_options"]][["cpu"]] <- list(ram_bloat = 7)
    expect_equal(.conf_backend("texture_options", "cpu", "ram_bloat", 1), 7)
    # a missing key or backend gives the default
    expect_equal(
        .conf_backend("texture_options", "cpu", "ram_worker_memsize", 0), 0
    )
    expect_equal(.conf_backend("texture_options", "tpu", "ram_bloat", 3), 3)
    # a string default is a value, not the name of a key
    expect_equal(
        .conf_backend("texture_options", "tpu", "device", "colors"),
        "colors"
    )
})

test_that("the backends fall back to the general bloat of sits", {
    pixels <- (512 + 2 * 3)^2
    # CPU without its options: processing_bloat_cpu and no fixed memory
    texture_drop_backend("cpu")
    need <- pixels * 2 * 8 * .conf("processing_bloat_cpu") * 1e-9
    m <- 10.5 * need
    got <- texture_size(gpu = FALSE, memsize = m)
    expect_equal(got[["multicores"]], floor(m / need))
    # GPU without its options: processing_bloat_gpu for every base and no
    # fixed memory, in RAM and on the GPU
    texture_drop_backend("gpu")
    bloat <- .conf("processing_bloat_gpu")
    box <- pixels * 8 * bloat * 1e-9
    got <- texture_size(gpu = TRUE, gpu_memsize = 10.5 * box)
    expect_equal(got[["multicores"]], 10L)
    pairs <- pixels * 2 * 7 * 6 * 8 * bloat * 1e-9
    got <- texture_size(gpu = TRUE, metrics = "glcm_asm",
        gpu_memsize = 10.5 * pairs)
    expect_equal(got[["multicores"]], 10L)
    ram <- pixels * 2 * 8 * bloat * 1e-9
    got <- texture_size(gpu = TRUE, memsize = 10.5 * ram)
    expect_equal(got[["multicores"]], 10L)
})

test_that("the CPU backend sizes by the keys of phase 2h", {
    # c2g series and cpp64q1-3 on the server (2026-11-24): bloat 3 of the
    # fit 2.9; worker 0.31, the 0.316 of cpp64q2 less its block; the call
    # adds 1.05 GB to the main process and 0.5 GB to each merge, 2 merges at
    # once
    key <- function(k) .conf(c("texture_options", "cpu", k))
    expect_equal(key("ram_bloat"), 3)
    expect_equal(key("ram_worker_memsize"), 0.31)
    expect_equal(key("ram_main_memsize"), 1.05)
    expect_equal(key("ram_merge_memsize"), 0.5)
    expect_equal(.conf(c("texture_options", "merges")), 2)
    # a block of 518 x 518 with its overlap: 268324 x 2 x 8 x 3 = 0.01288 GB;
    # one worker 0.32288 GB; the call 1.05 + 2 x 0.5 = 2.05 GB
    # memsize 4: floor(1.95 / 0.32288) = 6 workers; 0.09 / 6 = 0.015 GB per
    # worker, 1 base block, so the base block
    got <- texture_size(gpu = FALSE, memsize = 4, multicores = 64L)
    expect_equal(got[["multicores"]], 6L)
    expect_equal(unname(got[["block"]][c("nrows", "ncols")]), c(512L, 512L))
    # memsize 16, multicores 8: 8 workers; 11.47 / 8 = 1.434 GB per worker,
    # 111 base blocks, 6 lines of 16: 3 segments of 6 lines
    got <- texture_size(gpu = FALSE, memsize = 16, multicores = 8L)
    expect_equal(got[["multicores"]], 8L)
    expect_equal(unname(got[["block"]][c("nrows", "ncols")]), c(3072L, 8192L))
    # one worker asked has one merge at a time: 1.05 + 0.5 = 1.55 GB, and
    # 1.55 + 0.32288 fits in memsize 2
    got <- texture_size(gpu = FALSE, memsize = 2, multicores = 1L)
    expect_equal(got[["multicores"]], 1L)
    # memsize 1 does not hold the call of 1.55 GB: the error names memsize
    # and the GB of the call
    expect_error(texture_size(gpu = FALSE, memsize = 1, multicores = 1L),
        "1.55")
    # with no merges key the call counts no merge: 1.05 + 0.32288 fits in
    # memsize 1.4 with 2 workers asked
    opts <- sits_env[["config"]][["texture_options"]]
    on.exit(sits_env[["config"]][["texture_options"]] <- opts, add = TRUE)
    sits_env[["config"]][["texture_options"]][["merges"]] <- NULL
    got <- texture_size(gpu = FALSE, memsize = 1.4, multicores = 2L)
    expect_equal(got[["multicores"]], 1L)
})

test_that("sits_texture holds the call and some workers by default", {
    # 8 GB: the C++ call of 2.05 GB and 18 workers, the torch call of 1.7 GB
    # and 4 workers of 1.5 GB
    expect_equal(formals(sits_texture.raster_cube)[["memsize"]], 8L)
    # 5.95 / 0.32288 = 18 workers; 6.3 / (1.5 + 268324 x 2 x 8 x 9e-9) = 4
    expect_equal(texture_size(gpu = FALSE, memsize = 8)[["multicores"]], 18L)
    expect_equal(texture_size(gpu = TRUE, memsize = 8)[["multicores"]], 4L)
})

test_that("sits_texture checks gpu_memsize", {
    feature <- texture_cube()
    out_dir <- texture_dir("texture-gpu-memsize")
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    for (bad in list(0, "a")) {
        expect_error(
            sits_texture(
                feature,
                NDVICONTRAST = glcm_contrast(NDVI),
                window_size  = 5L,
                gpu_memsize  = bad,
                output_dir   = out_dir,
                progress     = FALSE
            ),
            "gpu_memsize"
        )
    }
})

test_that(".texture_use_gpu follows SITS_FORCE_CPU on a GPU machine", {
    skip_if_not_installed("torch")
    old_env <- Sys.getenv("SITS_FORCE_CPU", unset = NA_character_)
    on.exit(
        if (is.na(old_env)) {
            Sys.unsetenv("SITS_FORCE_CPU")
        } else {
            Sys.setenv(SITS_FORCE_CPU = old_env)
        }
    )
    # a GPU machine as torch sees it
    testthat::local_mocked_bindings(.torch_is_functional = function() TRUE)
    testthat::local_mocked_bindings(
        cuda_is_available = function() TRUE, .package = "torch"
    )
    Sys.setenv(SITS_FORCE_CPU = "FALSE")
    expect_true(.texture_use_gpu())
    Sys.setenv(SITS_FORCE_CPU = "TRUE")
    expect_false(.texture_use_gpu())
})

test_that("sits_texture torch and C++ backends produce close results on real data", {
    skip_on_cran()
    skip_if_not_installed("torch")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    class(feature) <- "data.frame"

    window_size <- 5L

    # Use normalizePath so the output dir and internally computed file paths
    # both use the same separator, avoiding a mixed-slash issue on Windows
    # where .file_eo_name normalises paths while file.path uses forward slashes.
    out_cpp   <- normalizePath(file.path(tempdir(), "texcpp"),   mustWork = FALSE)
    out_torch <- normalizePath(file.path(tempdir(), "textorch"), mustWork = FALSE)
    unlink(out_cpp,   recursive = TRUE)
    unlink(out_torch, recursive = TRUE)
    dir.create(out_cpp,   recursive = TRUE, showWarnings = FALSE)
    dir.create(out_torch, recursive = TRUE, showWarnings = FALSE)
    on.exit({
        unlink(out_cpp,   recursive = TRUE)
        unlink(out_torch, recursive = TRUE)
    }, add = TRUE)

    cpp_cube <- testthat::with_mocked_bindings(
        sits_texture(
            cube         = feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size  = window_size,
            multicores   = 1L,
            output_dir   = out_cpp,
            progress     = FALSE
        ),
        .texture_use_gpu = function() FALSE
    )

    torch_cube <- testthat::with_mocked_bindings(
        sits_texture(
            cube         = feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size  = window_size,
            multicores   = 1L,
            output_dir   = out_torch,
            progress     = FALSE
        ),
        .texture_use_gpu = function() TRUE
    )

    expect_true(all(sits_bands(cpp_cube)   %in% c("NDVI", "NDVICONTRAST")))
    expect_true(all(sits_bands(torch_cube) %in% c("NDVI", "NDVICONTRAST")))

    cpp_rast   <- .raster_open_rast(
        .fi_paths(.fi(sits_select(cpp_cube,   bands = "NDVICONTRAST")))
    )
    torch_rast <- .raster_open_rast(
        .fi_paths(.fi(sits_select(torch_cube, bands = "NDVICONTRAST")))
    )

    cpp_vals   <- .raster_get_values(cpp_rast)
    torch_vals <- .raster_get_values(torch_rast)

    # same NA mask; values differ only by the INT2S rounding of each path
    expect_identical(is.na(cpp_vals), is.na(torch_vals))
    expect_lte(max(abs(cpp_vals - torch_vals), na.rm = TRUE), 1)
})

test_that("sits_texture torch backend produces all nine GLCM bands", {
    skip_on_cran()
    skip_if_not_installed("torch")

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    class(feature) <- "data.frame"

    out_dir <- normalizePath(file.path(tempdir(), "texbands"), mustWork = FALSE)
    unlink(out_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

    run <- function(texture_cube) {
        testthat::with_mocked_bindings(
            texture_cube,
            .texture_use_gpu = function() TRUE
        )
    }

    expect_true("NDVICONTRAST" %in% sits_bands(run(sits_texture(
        feature, NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIDISSIM" %in% sits_bands(run(sits_texture(
        feature, NDVIDISSIM = glcm_dissimilarity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIHOMOG" %in% sits_bands(run(sits_texture(
        feature, NDVIHOMOG = glcm_homogeneity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIENERGY" %in% sits_bands(run(sits_texture(
        feature, NDVIENERGY = glcm_energy(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIASM" %in% sits_bands(run(sits_texture(
        feature, NDVIASM = glcm_asm(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIMEAN" %in% sits_bands(run(sits_texture(
        feature, NDVIMEAN = glcm_mean(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIVAR" %in% sits_bands(run(sits_texture(
        feature, NDVIVAR = glcm_variance(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVISTD" %in% sits_bands(run(sits_texture(
        feature, NDVISTD = glcm_std(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVICORRELATION" %in% sits_bands(run(sits_texture(
        feature, NDVICORRELATION = glcm_correlation(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
})

test_that("sits_texture rejects windows without neighbour pairs", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    expect_error(
        sits_texture(
            cube,
            NDVIVAR = glcm_variance(NDVI),
            window_size = 1L,
            output_dir = tempdir(),
            progress = FALSE
        ),
        "window_size"
    )
})
