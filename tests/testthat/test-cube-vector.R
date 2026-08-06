# Segment the bundled MODIS cube into output_dir
segment_modis <- function(output_dir) {
    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    segs_cube <- sits_segment(
        cube = modis_cube,
        seg_fn = sits_snic(
            grid_seeding = "rectangular",
            spacing = 15,
            compactness = 0.4,
            padding = 2
        ),
        output_dir = output_dir,
        progress = FALSE
    )
    list(raster = modis_cube, segs = segs_cube)
}

# Segment and classify the bundled MODIS cube into output_dir
classify_modis <- function(output_dir) {
    cubes <- segment_modis(output_dir)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    cubes[["probs"]] <- sits_classify(
        data = cubes[["segs"]],
        ml_model = rfor_model,
        output_dir = output_dir,
        progress = FALSE
    )
    labels <- sits_labels(rfor_model)
    cubes[["labels"]] <- stats::setNames(labels, seq_along(labels))
    cubes
}

# Create an empty output directory
new_output_dir <- function(name) {
    output_dir <- file.path(tempdir(), name)
    unlink(output_dir, recursive = TRUE)
    dir.create(output_dir, showWarnings = FALSE)
    output_dir
}

test_that("Reload segments vector cube", {
    output_dir <- new_output_dir("vec_segs")
    on.exit(unlink(output_dir, recursive = TRUE))

    cubes <- segment_modis(output_dir)
    local_segs_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = cubes[["raster"]],
        vector_dir = output_dir,
        progress = FALSE
    )
    expect_s3_class(local_segs_cube, "segs_cube")
    expect_s3_class(local_segs_cube, "vector_cube")
    expect_true("vector_info" %in% colnames(local_segs_cube))
    expect_equal(nrow(local_segs_cube), nrow(cubes[["segs"]]))
    expect_equal(sits_bands(local_segs_cube), sits_bands(cubes[["segs"]]))
    expect_equal(sits_timeline(local_segs_cube), sits_timeline(cubes[["segs"]]))

    # vector_band is deprecated
    expect_warning(
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6.1",
            raster_cube = cubes[["raster"]],
            vector_dir = output_dir,
            vector_band = "segments",
            progress = FALSE
        ),
        regexp = "deprecated"
    )
})

test_that("Reload probs vector cube", {
    output_dir <- new_output_dir("vec_probs")
    on.exit(unlink(output_dir, recursive = TRUE))

    cubes <- classify_modis(output_dir)
    # recover the probs raster cube
    probs_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "probs",
        labels = cubes[["labels"]],
        progress = FALSE
    )
    # attach the segments to the probs cube
    local_probs_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = probs_cube,
        vector_dir = output_dir,
        progress = FALSE
    )
    expect_s3_class(local_probs_vector_cube, "probs_vector_cube")
    expect_s3_class(local_probs_vector_cube, "derived_vector_cube")
    expect_true("vector_info" %in% colnames(local_probs_vector_cube))
    expect_equal(sits_labels(local_probs_vector_cube), cubes[["labels"]])
    expect_equal(sits_bands(local_probs_vector_cube), "probs")

    # the reloaded cube must be usable
    class_vector_cube <- sits_label_classification(
        cube = local_probs_vector_cube,
        label_method = "mean",
        output_dir = output_dir,
        version = "v2",
        progress = FALSE
    )
    expect_s3_class(class_vector_cube, "class_vector_cube")
    expect_true("vector_info" %in% colnames(class_vector_cube))
})

test_that("Reload class vector cube", {
    output_dir <- new_output_dir("vec_class")
    on.exit(unlink(output_dir, recursive = TRUE))

    cubes <- classify_modis(output_dir)
    sits_label_classification(
        cube = cubes[["probs"]],
        label_method = "mean",
        output_dir = output_dir,
        progress = FALSE
    )
    # recover the class raster cube
    class_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "class",
        labels = cubes[["labels"]],
        progress = FALSE
    )
    # attach the segments to the class cube
    local_class_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = class_cube,
        vector_dir = output_dir,
        progress = FALSE
    )
    expect_s3_class(local_class_vector_cube, "class_vector_cube")
    expect_s3_class(local_class_vector_cube, "derived_vector_cube")
    expect_true("vector_info" %in% colnames(local_class_vector_cube))
    expect_equal(sits_labels(local_class_vector_cube), cubes[["labels"]])
    expect_equal(sits_bands(local_class_vector_cube), "class")
})

test_that("Reload uncertainty vector cube", {
    output_dir <- new_output_dir("vec_uncert")
    on.exit(unlink(output_dir, recursive = TRUE))

    cubes <- classify_modis(output_dir)
    uncert_vector_cube <- sits_uncertainty(
        cube = cubes[["probs"]],
        type = "entropy",
        output_dir = output_dir,
        progress = FALSE
    )
    expect_s3_class(uncert_vector_cube, "uncertainty_vector_cube")
    # recover the uncertainty raster cube
    uncert_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "entropy",
        progress = FALSE
    )
    # attach the segments to the uncertainty cube
    local_uncert_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = uncert_cube,
        vector_dir = output_dir,
        progress = FALSE
    )
    expect_s3_class(local_uncert_vector_cube, "uncertainty_vector_cube")
    expect_s3_class(local_uncert_vector_cube, "derived_vector_cube")
    expect_true("vector_info" %in% colnames(local_uncert_vector_cube))
    expect_equal(sits_bands(local_uncert_vector_cube), "entropy")
})

test_that("Reload variance vector cube", {
    output_dir <- new_output_dir("vec_var")
    on.exit(unlink(output_dir, recursive = TRUE))

    cubes <- classify_modis(output_dir)
    var_vector_cube <- sits_variance(
        cube = cubes[["probs"]],
        output_dir = output_dir,
        progress = FALSE
    )
    expect_s3_class(var_vector_cube, "variance_vector_cube")
    # recover the variance raster cube
    var_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        bands = "variance",
        labels = cubes[["labels"]],
        progress = FALSE
    )
    # attach the segments to the variance cube
    local_var_vector_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = var_cube,
        vector_dir = output_dir,
        progress = FALSE
    )
    expect_s3_class(local_var_vector_cube, "variance_vector_cube")
    expect_s3_class(local_var_vector_cube, "derived_vector_cube")
    expect_true("vector_info" %in% colnames(local_var_vector_cube))
    expect_equal(sits_bands(local_var_vector_cube), "variance")
})

test_that("Deprecated parameters on vector cube operations", {
    output_dir <- new_output_dir("vec_deprecated")
    on.exit(unlink(output_dir, recursive = TRUE))

    cubes <- segment_modis(output_dir)
    rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
    # n_sam_pol is deprecated
    expect_warning(
        probs_cube <- sits_classify(
            data = cubes[["segs"]],
            ml_model = rfor_model,
            output_dir = output_dir,
            n_sam_pol = 10,
            progress = FALSE
        ),
        regexp = "n_sam_pol.*deprecated"
    )
    # segment-based labeling without label_method is deprecated
    expect_warning(
        class_cube <- sits_label_classification(
            cube = probs_cube,
            output_dir = output_dir,
            progress = FALSE
        ),
        regexp = "deprecated"
    )
    expect_s3_class(class_cube, "class_vector_cube")
})
