test_that("Describing a collection of sits as a HuggingFace dataset", {
    collection_conf <- sits_config_to_hf("MPC", "SENTINEL-2-L2A")

    # the dataset describes the data, not how sits reaches a provider
    expect_equal(collection_conf[["satellite"]], "SENTINEL-2")
    expect_equal(collection_conf[["grid_system"]], "MGRS")
    expect_false(any(
        .conf("hf", "collection_conf_source_keys") %in% names(collection_conf)
    ))

    # images are named as the bands of sits, not as the assets of the
    # provider ("SCL", in the case of the cloud band of Sentinel-2)
    expect_equal(
        names(collection_conf[["bands"]]),
        unname(purrr::map_chr(collection_conf[["bands"]], "band_name"))
    )

    # a dataset describes only bands selected
    collection_conf <- sits_config_to_hf(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        bands = c("B02", "B8A", "CLOUD")
    )
    expect_equal(names(collection_conf[["bands"]]), c("B02", "B8A", "CLOUD"))
    expect_equal(collection_conf[["bands"]][["B02"]][["resolution"]], 10L)
    expect_equal(
        collection_conf[["bands"]][["CLOUD"]][["interp_values"]],
        c(0, 1, 2, 3, 8, 9, 10)
    )

    # classified maps not produced by sits keep their labels
    collection_conf <- sits_config_to_hf("TERRASCOPE", "WORLD-COVER-2021")

    expect_true(collection_conf[["class_cube"]])
    expect_equal(names(collection_conf[["bands"]]), "CLASS")
    expect_equal(
        collection_conf[["bands"]][["CLASS"]][["values"]][["10"]], "Tree_Cover"
    )

    # collections and bands sits does not know are rejected
    expect_error(sits_config_to_hf("MPC", "SENTINEL-2-INVALID"))
    expect_error(sits_config_to_hf("NOT-A-SOURCE", "SENTINEL-2-L2A"))
    expect_error(
        sits_config_to_hf("MPC", "SENTINEL-2-L2A", bands = "B13")
    )
})

test_that("Describing a data cube as a HuggingFace dataset", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    collection_conf <- sits_to_hf(cube)

    # base properties are correctly transformed
    expect_equal(collection_conf[["satellite"]], "TERRA")
    expect_equal(collection_conf[["sensor"]], "MODIS")
    expect_equal(collection_conf[["grid_system"]], "STG")
    expect_equal(collection_conf[["dates"]], "2013 to 2014")

    # a cube that shares images of multiple satellite / sensor is rejected
    expect_error(sits_to_hf(
        dplyr::bind_rows(cube, dplyr::mutate(cube, satellite = "AQUA"))
    ))

    # bands are converted correctly
    band_conf <- collection_conf[["bands"]][["NDVI"]]

    expect_equal(band_conf[["band_name"]], "NDVI")
    expect_equal(band_conf[["data_type"]], "INT2S")
    expect_equal(band_conf[["missing_value"]], -3000L)
    expect_equal(band_conf[["scale_factor"]], 0.0001)

    # the resolution of a band is read from the images
    expect_equal(
        band_conf[["resolution"]], .tile_xres(.tile(cube)), tolerance = 1.0e-06
    )

    # bands computed by sits (e.g., indices) are unknown to the collection,
    # and are described as sits reads them
    apply_dir <- file.path(tempdir(), "hf_apply")
    dir.create(apply_dir, showWarnings = FALSE)
    on.exit(unlink(apply_dir, recursive = TRUE), add = TRUE)

    index_cube <- sits_apply(
        cube,
        NDWI = 0.5 * NDVI,
        output_dir = apply_dir,
        progress = FALSE
    )
    index_conf <- sits_to_hf(index_cube)

    # custom bands are available
    expect_equal(names(index_conf[["bands"]]), c("NDVI", "NDWI"))

    # custom band is using the correct name
    expect_equal(index_conf[["bands"]][["NDWI"]][["band_name"]], "NDWI")

    # custom bands use default values
    band_values <- .conf("default_values", "INT2S")
    expect_equal(
        index_conf[["bands"]][["NDWI"]][names(band_values)], band_values
    )

    # collection with custom bands must be accepted as valid
    # collection config
    expect_no_error(.hf_conf_check(index_conf))

    # the definition written is the definition returned
    output_dir <- file.path(tempdir(), "hf_conf_cube")
    dir.create(output_dir, showWarnings = FALSE)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

    # transform!
    sits_to_hf(cube, output_dir = output_dir, repo = "user/dataset")

    # comparison must be valid
    expect_equal(
        yaml::yaml.load_file(file.path(output_dir, "sits", "sits.yml")),
        collection_conf
    )

    # and it is a definition sits is able to register
    expect_no_error(.hf_conf_check(collection_conf))
})

test_that("Describing results and embeddings as a HuggingFace dataset", {
    emb_dir <- file.path(tempdir(), "hf_emb")
    dir.create(emb_dir, showWarnings = FALSE)
    on.exit(unlink(emb_dir, recursive = TRUE), add = TRUE)

    # simulating "embeddings" by creating files named as sits embeddings
    images <- list.files(
        system.file("extdata/raster/mod13q1", package = "sits"),
        pattern = "[.]jp2$", full.names = TRUE
    )

    # "generate" embeddings
    purrr::iwalk(utils::head(images, 2L), function(image, index) {
        target_file <- paste0(
            "TERRA_MODIS_012010_EMB0", index - 1L, "_2014-01-01.jp2"
        )
        target_file <- file.path(emb_dir, target_file)

        file.copy(image, target_file, overwrite = TRUE)
    })

    # load embeddings cube
    emb_cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = emb_dir,
        progress   = FALSE
    )
    expect_s3_class(emb_cube, "embeddings_cube")

    # embeddings described must have a complete collection
    # with bands, dates and other properties
    collection_conf <- sits_to_hf(emb_cube)

    expect_null(collection_conf[["bands"]])
    expect_equal(collection_conf[["dates"]], "2014")
    expect_no_error(.hf_conf_check(collection_conf))

    # results produced by sits are described by their
    # resolution and labels
    labels <- c(
        "1" = "ClearCut_Fire",
        "2" = "ClearCut_BareSoil",
        "3" = "ClearCut_Veg",
        "4" = "Forest"
    )

    class_cube <- sits_cube(
        source     = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir   = system.file("extdata/raster/classif", package = "sits"),
        parse_info = .conf("results_parse_info_def"),
        bands      = "class",
        labels     = labels,
        progress   = FALSE
    )

    # classified maps produced by sits are named as the results of sits, so
    # they are described as results, and not as `class_cube`
    collection_conf <- sits_to_hf(class_cube)

    expect_null(collection_conf[["class_cube"]])
    expect_equal(collection_conf[["labels"]], as.list(labels))

    # results are described in upper case, which is how sits looks a band up,
    # while the images keep the name sits writes
    expect_equal(
        names(collection_conf[["bands"]][["CLASS"]]), "resolution"
    )
    expect_no_error(.hf_conf_check(collection_conf))

    # a definition written is a definition sits is able to read: the band is
    # looked up in upper case and names the images in lower case
    conf_reg <- utils::modifyList(
        .conf("hf", "collection_defaults"), collection_conf
    )

    conf_reg[["parse_info"]] <- .conf("results_parse_info_def")
    conf_reg[["collection_name"]] <- "user/dataset"

    # generate results collection
    conf_reg <- .hf_collection_results(conf_reg)

    # it is possible to register it
    .conf_add_source(
        source = "HF:TEST",
        source_conf = list(
            s3_class = .conf("hf", "s3_class"),
            url = .conf("hf", "url"),
            collections = list(DATASET = conf_reg)
        )
    )

    on.exit({
            sits_env[["config"]][["sources"]][["HF:TEST"]] <- NULL
            sits_env[["sources_session"]] <- NULL
        },
        add = TRUE
    )

    # collection must be registered and accessible via source api
    expect_equal(.source_bands("HF:TEST", "DATASET"), "CLASS")
    expect_equal(.source_bands_band_name("HF:TEST", "DATASET"), "class")

    # a dataset can share more than one result of the same classification
    res_dir <- file.path(tempdir(), "hf_results")
    dir.create(res_dir, showWarnings = FALSE)
    on.exit(unlink(res_dir, recursive = TRUE), add = TRUE)

    # get class files
    class_files <- list.files(
        system.file("extdata/raster/classif", package = "sits"),
        full.names = TRUE
    )

    # fake entropy files
    file.copy(
        class_files,
        file.path(res_dir, gsub("class", "entropy", basename(class_files))),
        overwrite = TRUE
    )

    # load entropy cube
    entropy_cube <- sits_cube(
        source     = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir   = res_dir,
        parse_info = .conf("results_parse_info_def"),
        bands      = "entropy",
        labels     = labels,
        progress   = FALSE
    )

    # we can convert a list of cubes describing the same cube / tile / sensor
    collection_conf <- sits_to_hf(list(class_cube, entropy_cube))

    expect_equal(names(collection_conf[["bands"]]), c("CLASS", "ENTROPY"))
    expect_equal(collection_conf[["labels"]], as.list(labels))
    expect_no_error(.hf_conf_check(collection_conf))

    # duplicated bands are rejected
    expect_error(sits_to_hf(list(class_cube, class_cube)))
})

test_that("Writing the files of a HuggingFace dataset", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )

    output_dir <- file.path(tempdir(), "hf_dataset")
    dir.create(output_dir, showWarnings = FALSE)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

    # files of sits are written in the "sits" directory, as they are read
    # in the repository
    sits_dir <- file.path(output_dir, "sits")
    conf_file <- file.path(sits_dir, "sits.yml")
    cache_file <- file.path(sits_dir, "cache.rds")

    # nothing is written when no directory is informed
    expect_no_error(sits_to_hf(cube))
    expect_false(dir.exists(sits_dir))

    # the cache refers to the images of a repository, which is required.
    # nothing is written when it is not available
    expect_error(sits_to_hf(cube, output_dir = output_dir))
    expect_false(dir.exists(sits_dir))

    # directory must exist
    expect_error(
        sits_to_hf(
            cube,
            output_dir = file.path(output_dir, "missing"),
            repo = "user/dataset"
        )
    )

    # the collection definition and the cache are written together
    sits_to_hf(cube, output_dir = output_dir, repo = "user/dataset")
    expect_setequal(list.files(sits_dir), c("sits.yml", "cache.rds"))

    # definitions are written as sits writes its own sources
    conf_lines <- readLines(conf_file)
    expect_true(any(grepl("^# sits collection definition", conf_lines)))
    expect_true(any(grepl("^satellite: TERRA$", conf_lines)))

    # the cache describes the cube, with images read from the repository
    cache <- readRDS(cache_file)

    expect_equal(names(cache), .conf("hf", "cache_keys"))
    expect_equal(
        cache[["sits_version"]],
        utils::packageDescription("sits")[["Version"]]
    )

    # the dataset is registered as sits registers it when reading the
    # dataset, using the collection definition written with the cache
    testthat::local_mocked_bindings(
        .hf_repo = function(source, collection) "user/dataset",
        .hf_collection_conf = function(repo) {
            yaml::yaml.load_file(file.path(output_dir, "sits", "sits.yml"))
        }
    )

    .hf_source_register("HF:USER", "DATASET")
    
    on.exit(
        {
            sits_env[["config"]][["sources"]][["HF:USER"]] <- NULL
            sits_env[["sources_session"]] <- NULL
        },
        add = TRUE
    )

    # it is described as the dataset, not as the provider of the images
    expect_equal(cache[["source"]], "HF:USER")
    expect_equal(cache[["collection"]], "DATASET")
    expect_equal(.cube_source(cache[["cube"]]), "HF:USER")
    expect_equal(.cube_collection(cache[["cube"]]), "DATASET")
    expect_equal(
        unlist(.cube_paths(cache[["cube"]])),
        paste0(
            "/vsicurl/https://huggingface.co/datasets/user/dataset/",
            "resolve/main/", basename(unlist(.cube_paths(cube)))
        )
    )
    expect_equal(.cube_timeline(cache[["cube"]]), .cube_timeline(cube))
    expect_equal(.cube_bands(cache[["cube"]]), .cube_bands(cube))

    # a cache describes a single cube: many cubes are shared only with their
    # collection definition
    unlink(sits_dir, recursive = TRUE)
    sits_to_hf(list(cube), output_dir = output_dir)
    expect_equal(list.files(sits_dir), "sits.yml")

    # collections of sits are written the same way
    unlink(sits_dir, recursive = TRUE)
    sits_config_to_hf("MPC", "SENTINEL-2-L2A", output_dir = output_dir)
    expect_equal(list.files(sits_dir), "sits.yml")

    # we only describe cubes
    expect_error(sits_to_hf(samples_modis_ndvi))
})
