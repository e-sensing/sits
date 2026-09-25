test_that("Creating a cube from a HuggingFace dataset", {
    # define cube
    cube <- .try(
        {
            sits_cube(
                source     = "HF:felipemcarlos",
                collection = "sits_mod13q1_sinop",
                tiles      = "012010",
                progress   = FALSE
            )
        },
        .default = NULL
    )

    # skip if cube is null
    testthat::skip_if(purrr::is_null(cube),
        message = "HuggingFace is not accessible"
    )

    # check cube
    expect_s3_class(cube, "hf_cube")
    expect_equal(.tile_name(cube), "012010")

    # sits names are upper case
    expect_equal(.cube_source(cube), "HF:FELIPEMCARLOS")
    expect_equal(.cube_collection(cube), "SITS_MOD13Q1_SINOP")
    expect_equal(sits_bands(cube), "NDVI")
    expect_equal(length(sits_timeline(cube)), 12L)

    # the collection was registered in the configuration
    expect_true("HF:FELIPEMCARLOS" %in% .sources())

    # the images are the same ones distributed with sits
    local_cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = system.file("extdata/raster/mod13q1", package = "sits"),
        progress   = FALSE
    )

    expect_equal(sits_timeline(cube), sits_timeline(local_cube))
    expect_equal(sits_bbox(cube), sits_bbox(local_cube))

    # get file_info
    fi <- .fi(cube)

    # check file_info
    expect_equal(nrow(fi), 12L)
    expect_true(all(fi[["nrows"]] == 147L & fi[["ncols"]] == 255L))

    # the path must open and have one band
    rast <- .raster_open_rast(fi[["path"]][[1L]])
    expect_equal(.raster_nlayers(rast), 1L)

    # images are plotted without informing bands (no color composites)
    p <- plot(cube)
    expect_equal(nrow(p[[1L]]$shp), 147L)

    # a cube restored in a new session registers its collection when used,
    # including by operations that only check the source (e.g., regularize)
    sits_env[["config"]][["sources"]][["HF:FELIPEMCARLOS"]] <- NULL
    sits_env[["sources_session"]] <- NULL

    expect_true("hf_cube" %in% .cube_s3class(cube))
    expect_true("HF:FELIPEMCARLOS" %in% .sources())

    sits_env[["config"]][["sources"]][["HF:FELIPEMCARLOS"]] <- NULL
    sits_env[["sources_session"]] <- NULL
    point <- data.frame(
        longitude = -55.5, latitude = -11.6, start_date = "2013-09-14",
        end_date = "2014-08-29", label = "x"
    )

    samples <- sits_get_data(cube, samples = point, progress = FALSE)
    expect_equal(nrow(samples[["time_series"]][[1L]]), 12L)
    expect_true("HF:FELIPEMCARLOS" %in% .sources())

    # copied images are loaded again using the sits names
    output_dir <- file.path(tempdir(), "hf_cube_copy")
    dir.create(output_dir, showWarnings = FALSE)

    sits_cube_copy(cube, output_dir = output_dir, progress = FALSE)
    copy_cube <- sits_cube(
        source     = "HF:FELIPEMCARLOS",
        collection = "SITS_MOD13Q1_SINOP",
        data_dir   = output_dir,
        progress   = FALSE
    )

    expect_equal(sits_timeline(copy_cube), sits_timeline(cube))
    expect_equal(sits_bands(copy_cube), "NDVI")

    unlink(output_dir, recursive = TRUE)
})

test_that("Selecting images of a HuggingFace dataset", {
    cube <- .try(
        {
            sits_cube(
                source     = "HF:FELIPEMCARLOS",
                collection = "Sits_Mod13q1_Sinop",
                tiles      = "012010",
                start_date = "2013-09-14",
                end_date   = "2013-12-19",
                progress   = FALSE
            )
        },
        .default = NULL
    )

    # skip if cube is null
    testthat::skip_if(purrr::is_null(cube),
        message = "HuggingFace is not accessible"
    )

    # check cube
    expect_equal(.cube_collection(cube), "SITS_MOD13Q1_SINOP")
    expect_equal(length(sits_timeline(cube)), 4L)

    # tiles must exist in the dataset
    expect_error(
        sits_cube(
            source     = "HF:FELIPEMCARLOS",
            collection = "SITS_MOD13Q1_SINOP",
            tiles      = "000000",
            progress   = FALSE
        )
    )

    # selection by roi, with a grid system unknown to sits ("STG")
    roi <- c(
        lon_min = -55.6,
        lat_min = -11.7,
        lon_max = -55.5,
        lat_max = -11.6
    )

    roi_cube <- sits_cube(
        source     = "HF:FELIPEMCARLOS",
        collection = "SITS_MOD13Q1_SINOP",
        roi        = roi,
        progress   = FALSE
    )

    expect_equal(.tile_name(roi_cube), "012010")

    roi[c("lon_min", "lon_max")] <- c(-40.1, -40.0)
    expect_error(
        sits_cube(
            source     = "HF:FELIPEMCARLOS",
            collection = "SITS_MOD13Q1_SINOP",
            roi        = roi,
            progress   = FALSE
        )
    )

    # datasets without a collection definition are not supported
    expect_error(
        sits_cube(
            source     = "HF:HUGGINGFACEFW",
            collection = "FINEWEB",
            tiles      = "000000",
            progress   = FALSE
        )
    )
})

test_that("Creating classified maps and embeddings from HuggingFace", {
    # classified map (class_cube: true)
    class_cube <- .try(
        {
            sits_cube(
                source     = "HF:felipemcarlos",
                collection = "sits_mod13q1_sinop_class",
                tiles      = "012010",
                progress   = FALSE
            )
        },
        .default = NULL
    )

    # embeddings (bands EMB00, EMB01, ...)
    emb_cube <- .try(
        {
            sits_cube(
                source     = "HF:felipemcarlos",
                collection = "sits_mod13q1_sinop_emb",
                tiles      = "012010",
                progress   = FALSE
            )
        },
        .default = NULL
    )

    # skip if cubes are null
    testthat::skip_if(
        purrr::is_null(class_cube) || purrr::is_null(emb_cube),
        message = "HuggingFace is not accessible"
    )

    # embeddings: bands are read with the sits definition of embeddings
    expect_s3_class(emb_cube, "embeddings_cube")
    expect_equal(sits_bands(emb_cube), paste0("EMB0", 0L:7L))
    expect_equal(
        .tile_band_conf(.tile(emb_cube), "EMB00"),
        .conf("embedding_values", "INT2S")
    )

    # classified map: type and labels come from the collection definition
    expect_s3_class(class_cube, "class_cube")
    expect_equal(.cube_collection(class_cube), "SITS_MOD13Q1_SINOP_CLASS")
    expect_equal(sits_bands(class_cube), "class")
    expect_equal(
        unname(sits_labels(class_cube)),
        c("Cerrado", "Forest", "Pasture", "Soy_Corn")
    )

    # copies of classified maps are loaded again with the default version
    output_dir <- file.path(tempdir(), "hf_class_copy")
    dir.create(output_dir, showWarnings = FALSE)
    sits_cube_copy(class_cube, output_dir = output_dir, progress = FALSE)
    copy_cube <- sits_cube(
        source     = "HF:felipemcarlos",
        collection = "sits_mod13q1_sinop_class",
        data_dir   = output_dir,
        bands      = "class",
        labels     = sits_labels(class_cube),
        progress   = FALSE
    )

    expect_s3_class(copy_cube, "class_cube")
    unlink(output_dir, recursive = TRUE)
})

test_that("Creating results produced by sits from HuggingFace", {
    # probabilities (labels from the collection definition)
    probs_cube <- .try(
        {
            sits_cube(
                source     = "HF:felipemcarlos",
                collection = "sits_mod13q1_sinop_results",
                tiles      = "012010",
                bands      = "probs",
                progress   = FALSE
            )
        },
        .default = NULL
    )

    # skip if cube is null
    testthat::skip_if(purrr::is_null(probs_cube),
        message = "HuggingFace is not accessible"
    )

    # results follow the sits definition of results cubes
    expect_s3_class(probs_cube, "probs_cube")
    expect_equal(
        unname(sits_labels(probs_cube)),
        c("Cerrado", "Forest", "Pasture", "Soy_Corn")
    )
    expect_equal(sits_bands(probs_cube), "probs")
    expect_equal(.tile_name(probs_cube), "012010")

    # dates are those of the classified period
    expect_equal(
        as.character(sits_timeline(probs_cube)),
        c("2013-09-14", "2014-08-29")
    )

    # the description of the images is read from the images themselves
    expect_equal(.tile_nrows(probs_cube), 147L)
    expect_equal(.tile_ncols(probs_cube), 255L)
    class_cube <- sits_cube(
        source     = "HF:felipemcarlos",
        collection = "sits_mod13q1_sinop_results",
        tiles      = "012010",
        bands      = "class",
        progress   = FALSE
    )

    expect_s3_class(class_cube, "class_cube")

    # results are processed as any results cube
    output_dir <- file.path(tempdir(), "hf_results")
    dir.create(output_dir, showWarnings = FALSE)
    label_cube <- sits_label_classification(
        probs_cube,
        output_dir = output_dir,
        progress = FALSE
    )

    expect_s3_class(label_cube, "class_cube")
    unlink(output_dir, recursive = TRUE)
})

test_that("HuggingFace collection definitions identify the cube type", {
    # embedding bands, as defined by sits
    emb <- .conf("embedding_values", "INT2S")

    # the satellite and the sensor are part of the names of the files
    collection <- list(satellite = "TERRA", sensor = "MODIS")
    expect_error(.hf_collection_check(
        list(satellite = "TERRA", bands = list(NDVI = list()))
    ))

    # accepted: images, embeddings, and classified maps
    expect_no_error(.hf_collection_check(c(collection, yaml::yaml.load(
        "bands: {NDVI: {data_type: INT2S, scale_factor: 0.0001}}"
    ))))
    expect_no_error(.hf_collection_check(
        c(collection, list(bands = list(EMB00 = emb, EMB01 = emb)))
    ))
    expect_no_error(.hf_collection_check(c(collection, yaml::yaml.load(
        "{class_cube: true, bands: {CLASS: {values: {1: Forest}}}}"
    ))))

    # accepted: results produced by sits, completed with the sits definition
    results <- c(collection, yaml::yaml.load(
        "bands: {PROBS: {resolution: 30}, CLASS: {resolution: 30}}"
    ))

    expect_no_error(.hf_collection_check(results))
    expect_true(.hf_collection_is_results(results))

    results <- .hf_collection_results(results)
    expect_equal(results[["bands"]][["PROBS"]][["scale_factor"]], 0.0001)
    expect_equal(results[["bands"]][["CLASS"]][["data_type"]], "INT1U")
    expect_equal(results[["bands"]][["CLASS"]][["band_name"]], "class")

    # a definition without bands is only accepted for embeddings, which sits
    # describes itself (the bands come from the names of the images)
    expect_false(.hf_collection_is_results(collection))
    expect_false(.hf_collection_is_embeddings(collection))

    # rejected: results mixed with other bands, or not defined by sits
    expect_error(.hf_collection_check(c(collection, yaml::yaml.load(
        "bands: {PROBS: {resolution: 30}, NDVI: {resolution: 30}}"
    ))))
    expect_error(.hf_collection_check(c(collection, yaml::yaml.load(
        "bands: {UNCERT: {resolution: 30}}"
    ))))

    # rejected: classified maps with other bands
    expect_error(.hf_collection_check(c(collection, yaml::yaml.load(
        "{class_cube: true, bands: {MAP: {values: {1: Forest}}}}"
    ))))

    # rejected: embeddings not stored as defined by sits
    emb[["scale_factor"]] <- 1
    expect_error(.hf_collection_check(
        c(collection, list(bands = list(EMB00 = emb)))
    ))
})
