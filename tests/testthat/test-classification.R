test_that("Classify with random forest - single core and multicore", {
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))

    expect_type(rfor_model, "closure")
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    class_ndvi <- sits_classify(
        data = point_ndvi,
        ml_model = rfor_model,
        multicores = 2,
        progress = FALSE
    )

    expect_true(nrow(class_ndvi$predicted[[1]]) == 17)
    expect_true(all(class_ndvi$predicted[[1]]$class %in%
        sits_labels(samples_modis_ndvi)))
})

test_that("Classify a set of time series with svm and impute methods", {
    # train model
    svm_model <- sits_train(cerrado_2classes, sits_svm())
    # add NA
    classification_data <- sits_apply(cerrado_2classes,
                                      NDVI = ifelse(NDVI > 0.7, NA, NDVI))
    # define input methods
    input_fncs <- list(
        impute_linear(),
        impute_mean(),
        impute_median(),
        impute_mean_window(weighting = "simple"),
        impute_mean_window(weighting = "simple", k = 3),
        impute_mean_window(weighting = "linear"),
        impute_mean_window(weighting = "linear", k = 5),
        impute_mean_window(weighting = "exponential"),
        impute_mean_window(weighting = "exponential", k = 7),
        impute_mean_window(weighting = "exponential", k = 30)
    )
    # test classification with impute methods
    results <- purrr::map_lgl(seq(input_fncs), function(idx) {
        # get impute function
        impute_fnc <- input_fncs[[idx]]
        # classify
        class1 <- sits_classify(classification_data,
                                ml_model = svm_model,
                                multicores = 2,
                                progress = FALSE)
        # test values
        class1$predicted[[1]]$class %in% sits_labels(cerrado_2classes)
    })
    # check if all results are true
    expect_true(all(results))
})

test_that("Classify error bands 1", {
    model <- sits_train(samples_modis_ndvi, sits_svm())
    point <- sits_select(point_mt_6bands, bands = "EVI")

    expect_error(
        sits_classify(
            data = point,
            ml_model = model
        )
    )
})

test_that("Classify with NA values (using multiple impute methods)", {
    # load cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        multicores = 2,
        progress = FALSE
    )
    # preparation - create directory to save NA
    data_dir <- paste0(tempdir(), "/na-cube")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - insert NA in cube
    raster_cube <- sits_apply(
        data = raster_cube,
        NDVI_NA = ifelse(NDVI > 0.5, NA, NDVI),
        output_dir = data_dir
    )
    raster_cube <- sits_select(raster_cube, bands = "NDVI_NA")
    .fi(raster_cube) <- .fi(raster_cube) |>
        dplyr::mutate(band = "NDVI")
    # preparation - create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    # define input methods
    input_fncs <- list(
        impute_linear(),
        impute_mean(),
        impute_median(),
        impute_mean_window(weighting = "simple"),
        impute_mean_window(weighting = "simple", k = 3),
        impute_mean_window(weighting = "linear"),
        impute_mean_window(weighting = "linear", k = 5),
        impute_mean_window(weighting = "exponential"),
        impute_mean_window(weighting = "exponential", k = 7),
        impute_mean_window(weighting = "exponential", k = 30)
    )
    # test impute methods
    results <- purrr::map_lgl(seq(input_fncs), function(idx) {
        # get impute function
        impute_fnc <- input_fncs[[idx]]
        # test classification with NA
        class_map <- sits_classify(
            data       = raster_cube,
            ml_model   = rfor_model,
            output_dir = data_dir,
            progress   = FALSE,
            impute_fn  = impute_fnc
        )
        # get map file
        class_map_file <- class_map[["file_info"]][[1]][["path"]]
        # get file raster
        class_map_rst <- .raster_open_rast(class_map_file)
        # test
        result <- anyNA(class_map_rst[])
        # delete file
        unlink(class_map_file)
        # return
        result
    })
    # check if all results are true
    expect_true(all(results))
    # remove test files
    unlink(data_dir)
})

test_that("Classify with exclusion mask", {
    # load cube
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    raster_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        tiles = "012010",
        bands = "NDVI",
        start_date = "2013-09-14",
        end_date = "2014-08-29",
        multicores = 2,
        progress = FALSE
    )
    # preparation - create directory to save NA
    data_dir <- paste0(tempdir(), "/exclusion-mask-na")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    # preparation - create exclusion mask
    exclusion_mask <- sf::st_as_sfc(
        x = sf::st_bbox(
            c(
                xmin = -55.63478,
                ymin = -11.63328,
                xmax = -55.54080,
                ymax = -11.56978
            ),
            crs = "EPSG:4326"
        )
    )
    # transform object to cube crs
    exclusion_mask <- sf::st_transform(exclusion_mask, .cube_crs(raster_cube))
    # preparation - calculate centroid of the exclusion mask
    exclusion_mask_centroid <- sf::st_centroid(exclusion_mask)
    # preparation - create a random forest model
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 40))
    # test classification with NA
    probs_map <- suppressWarnings(
        sits_classify(
            data = raster_cube,
            ml_model = rfor_model,
            output_dir = data_dir,
            exclusion_mask = exclusion_mask,
            progress = FALSE
        )
    )
    # testing original data
    probs_map_rst <- .raster_open_rast(probs_map[["file_info"]][[1]][["path"]])
    expect_true(anyNA(probs_map_rst[]))
    # extract values
    probs_map_value <- .raster_extract(
        probs_map_rst,
        .raster_open_vect(exclusion_mask_centroid)
    )

    expect_true(any(is.na(probs_map_value)))
    # remove test files
    unlink(data_dir)
})

# Force the CPU or auto-detected orchestration branch of sits_classify()
# through the public API by toggling SITS_FORCE_CPU (read by
# .torch_gpu_classification()), restoring the variable afterwards. Both
# branches run on CPU tensors when no CUDA/MPS device is present, so this
# exercises both code paths on any machine.
classify_forcing <- function(force, cube, ml_model, output_dir) {
    old <- Sys.getenv("SITS_FORCE_CPU", unset = NA)
    Sys.setenv(SITS_FORCE_CPU = force)
    on.exit(
        if (is.na(old)) {
            Sys.unsetenv("SITS_FORCE_CPU")
        } else {
            Sys.setenv(SITS_FORCE_CPU = old)
        }
    )
    sits_classify(
        data = cube, ml_model = ml_model, memsize = 4L, multicores = 2L,
        output_dir = output_dir, progress = FALSE
    )
}

test_that("Classify a torch model returns the same probs across CPU/GPU", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    set.seed(2971)
    torch::torch_manual_seed(2971)
    # cheap torch model; the CPU/GPU branch selection is independent of it
    ml_model <- .try(
        sits_train(
            samples_modis_ndvi,
            sits_mlp(
                layers = c(64L, 64L),
                dropout_rates = c(0.2, 0.3),
                epochs = 2L,
                batch_size = 64L,
                verbose = FALSE
            )
        ),
        .default = NULL
    )
    skip_if(is.null(ml_model), "torch training failed")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )

    cpu_dir <- file.path(tempdir(), "classify_torch_cpu")
    gpu_dir <- file.path(tempdir(), "classify_torch_gpu")
    unlink(c(cpu_dir, gpu_dir), recursive = TRUE)
    dir.create(cpu_dir)
    dir.create(gpu_dir)
    on.exit(unlink(c(cpu_dir, gpu_dir), recursive = TRUE), add = TRUE)

    # Drive both pipelines through the public API by forcing each branch
    probs_cpu <- classify_forcing("YES", cube, ml_model, cpu_dir)
    probs_gpu <- classify_forcing("NO", cube, ml_model, gpu_dir)

    # Both branches yield the same probs_cube structure and labels
    expect_s3_class(probs_cpu, "probs_cube")
    expect_s3_class(probs_gpu, "probs_cube")
    expect_equal(nrow(probs_cpu), nrow(probs_gpu))
    expect_equal(.cube_labels(probs_cpu), .cube_labels(probs_gpu))

    # Both branches run on CPU tensors here, so probabilities must agree.
    # The CPU and GPU orchestrations partition the data differently, which
    # can shift the integer-scaled probabilities by at most one unit.
    probs_cpu_rst <- .raster_open_rast(.fi(probs_cpu)[["path"]])
    probs_gpu_rst <- .raster_open_rast(.fi(probs_gpu)[["path"]])
    expect_equal(dim(probs_cpu_rst[]), dim(probs_gpu_rst[]))
    expect_lte(max(abs(probs_cpu_rst[] - probs_gpu_rst[])), 1L)
})
