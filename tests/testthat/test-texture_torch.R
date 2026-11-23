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
    bloat <- .conf(c("texture_options", "gpu_bloat"))
    expect_true(is.numeric(bloat) && bloat > 0)
    # GB of the windows tensor of one block on the GPU: one float32 column
    # per neighbour of each pixel, the overlap included
    need <- function(block, window_size) {
        overlap <- ceiling(window_size / 2L) - 1L
        pixels <- (block[["nrows"]] + 2L * overlap) *
            (block[["ncols"]] + 2L * overlap)
        pixels * window_size^2 * 4 * bloat * 1e-9
    }
    block <- c(nrows = 512L, ncols = 512L)
    image <- c(nrows = 8192L, ncols = 8192L)
    # GB in RAM of one block on the GPU path: the input and output bands
    # in doubles, and the windows tensor as torch builds it
    ram_need <- function(block, window_size, npaths = 2L) {
        overlap <- ceiling(window_size / 2L) - 1L
        pixels <- (block[["nrows"]] + 2L * overlap) *
            (block[["ncols"]] + 2L * overlap)
        pixels * npaths * 8 * .conf("processing_bloat_gpu") * 1e-9 +
            pixels * window_size^2 * 4 *
                .conf("processing_bloat_smooth_torch") * 1e-9
    }
    # memsize is large by default, so only the GPU limits the workers
    size <- function(gpu_memsize, window_size, multicores = 64L,
                     memsize = 10000) {
        backend <- testthat::with_mocked_bindings(
            .texture_backend(gpu_memsize = gpu_memsize),
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
    for (window_size in c(5L, 9L)) {
        got <- size(gpu_memsize = 40, window_size = window_size)
        expect_equal(
            got[["multicores"]],
            min(64L, floor(40 / need(block, window_size)))
        )
        expect_lte(need(got[["block"]], window_size) * got[["multicores"]], 40)
    }
    # the window counts: 9 x 9 neighbours take more memory than 5 x 5
    expect_lt(
        size(gpu_memsize = 40, window_size = 9L)[["multicores"]],
        size(gpu_memsize = 40, window_size = 5L)[["multicores"]]
    )
    # a smaller budget gives a smaller block
    small <- size(gpu_memsize = 8, window_size = 5L, multicores = 1L)
    large <- size(gpu_memsize = 80, window_size = 5L, multicores = 1L)
    expect_lt(
        prod(small[["block"]][c("nrows", "ncols")]),
        prod(large[["block"]][c("nrows", "ncols")])
    )
    # a grown block still fits the budget of its worker
    expect_lte(need(small[["block"]], 5L), 8)
    expect_lte(need(large[["block"]], 5L), 80)
    # RAM limits when memsize is the smaller budget
    ram <- size(gpu_memsize = 1000, window_size = 5L, memsize = 5)
    expect_equal(
        ram[["multicores"]],
        min(64L, floor(5 / ram_need(block, 5L)))
    )
    expect_lte(ram_need(ram[["block"]], 5L) * ram[["multicores"]], 5)
    # one block that does not fit the GPU names the GPU budget
    expect_error(size(gpu_memsize = 0.5, window_size = 9L), "gpu_memsize")
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
