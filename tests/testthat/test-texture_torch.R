test_that("torch glcm_compute matches C++ for all metrics (single angle)", {
    skip_if_not_installed("torch")

    set.seed(7321)
    nrows       <- 20L
    ncols       <- 20L
    window_size <- 5L
    n_grey      <- 64L   # small so the batched ASM loop is fast and comparable
    leg         <- window_size %/% 2L
    angles      <- 0.0

    # Synthetic grey-level image; values in [0, n_grey - 1] (matching C++ check)
    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))

    # Interior pixel indices (1-based R vector index)
    # Boundary pixels are excluded because PyTorch "reflect" padding and the
    # C++ locus_neigh2 mirror differ at the edge of the block.
    row_ids  <- (leg + 1L):(nrows - leg)
    col_ids  <- (leg + 1L):(ncols - leg)
    interior <- as.vector(
        outer(row_ids - 1L, col_ids, FUN = function(r, c) r * ncols + c)
    )

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
        max_diff <- max(abs(cpp_result[interior] - torch_result[interior, 1L]))
        expect_lt(max_diff, 0.5,
            label = paste("interior max|diff| for", metric))
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
    leg         <- window_size %/% 2L
    angles      <- c(0.0, pi / 2)

    img <- as.integer(sample(0L:(n_grey - 1L), nrows * ncols, replace = TRUE))

    row_ids  <- (leg + 1L):(nrows - leg)
    col_ids  <- (leg + 1L):(ncols - leg)
    interior <- as.vector(
        outer(row_ids - 1L, col_ids, FUN = function(r, c) r * ncols + c)
    )

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

    max_diff <- max(abs(cpp_result[interior] - torch_result[interior, 1L]))
    expect_lt(max_diff, 0.5)
})

test_that(".texture_torch_available respects torch availability and env override", {
    old_env <- Sys.getenv("SITS_TEXTURE_FORCE_CPP", unset = NA_character_)
    on.exit(
        if (is.na(old_env)) {
            Sys.unsetenv("SITS_TEXTURE_FORCE_CPP")
        } else {
            Sys.setenv(SITS_TEXTURE_FORCE_CPP = old_env)
        }
    )

    available <- function(functional, force_cpp = "FALSE") {
        testthat::local_mocked_bindings(
            .torch_is_functional = function() functional
        )
        Sys.setenv(SITS_TEXTURE_FORCE_CPP = force_cpp)
        .texture_torch_available()
    }

    expect_true(available(functional = TRUE))
    expect_false(available(functional = FALSE))
    expect_false(available(functional = TRUE, force_cpp = "TRUE"))
})

test_that("sits_texture_torch and sits_texture produce close results on real data", {
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
        .texture_torch_available = function() FALSE
    )

    torch_cube <- testthat::with_mocked_bindings(
        sits_texture_torch(
            cube         = feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size  = window_size,
            multicores   = 1L,
            output_dir   = out_torch,
            progress     = FALSE
        ),
        .torch_gpu_available = function() FALSE
    )

    expect_true(all(sits_bands(cpp_cube)   %in% c("NDVI", "NDVICONTRAST")))
    expect_true(all(sits_bands(torch_cube) %in% c("NDVI", "NDVICONTRAST")))

    cpp_rast   <- .raster_open_rast(
        .fi_paths(.fi(sits_select(cpp_cube,   bands = "NDVICONTRAST")))
    )
    torch_rast <- .raster_open_rast(
        .fi_paths(.fi(sits_select(torch_cube, bands = "NDVICONTRAST")))
    )

    nrow_r <- .raster_nrows(cpp_rast)
    ncol_r <- .raster_ncols(cpp_rast)

    # Exclude outermost leg pixels where padding strategies differ
    leg      <- window_size %/% 2L
    row_ids  <- (leg + 1L):(nrow_r - leg)
    col_ids  <- (leg + 1L):(ncol_r - leg)
    interior <- as.vector(
        outer(row_ids - 1L, col_ids, FUN = function(r, c) r * ncol_r + c)
    )

    cpp_vals   <- .raster_get_values(cpp_rast)
    torch_vals <- .raster_get_values(torch_rast)

    valid <- is.finite(cpp_vals[interior, 1L]) & is.finite(torch_vals[interior, 1L])
    pix   <- interior[valid]

    max_diff <- max(abs(cpp_vals[pix, 1L] - torch_vals[pix, 1L]))
    # Tolerance of 2: covers INT2S rounding (±1 per implementation) and
    # float32-vs-double arithmetic differences in the interior
    expect_lt(max_diff, 2.0)
})

test_that("sits_texture_torch produces all nine GLCM bands", {
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
            .torch_gpu_available = function() FALSE
        )
    }

    expect_true("NDVICONTRAST" %in% sits_bands(run(sits_texture_torch(
        feature, NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIDISSIM" %in% sits_bands(run(sits_texture_torch(
        feature, NDVIDISSIM = glcm_dissimilarity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIHOMOG" %in% sits_bands(run(sits_texture_torch(
        feature, NDVIHOMOG = glcm_homogeneity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIENERGY" %in% sits_bands(run(sits_texture_torch(
        feature, NDVIENERGY = glcm_energy(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIASM" %in% sits_bands(run(sits_texture_torch(
        feature, NDVIASM = glcm_asm(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIMEAN" %in% sits_bands(run(sits_texture_torch(
        feature, NDVIMEAN = glcm_mean(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVIVAR" %in% sits_bands(run(sits_texture_torch(
        feature, NDVIVAR = glcm_variance(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVISTD" %in% sits_bands(run(sits_texture_torch(
        feature, NDVISTD = glcm_std(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
    expect_true("NDVICORRELATION" %in% sits_bands(run(sits_texture_torch(
        feature, NDVICORRELATION = glcm_correlation(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir, progress = FALSE
    ))))
})
