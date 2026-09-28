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

test_that("Cache is used when available in a HuggingFace dataset", {
    repo <- "user/dataset"
    source <- "HF:USER"
    collection <- "DATASET"

    # the provider of a dataset shares the cube of the images it uploads
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = system.file("extdata/raster/mod13q1", package = "sits"),
        progress   = FALSE
    )

    # define cache file
    cache_dir <- file.path(tempdir(), "hf_cache")
    cache_file <- file.path(cache_dir, "cache.rds")

    # create directory and side effect
    dir.create(cache_dir, showWarnings = FALSE)
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

    # describe cube and create cache
    sits_to_hf(cube, cache = cache_file, repo = repo)

    # a cache describes the collection of the dataset it is shared in
    cache <- readRDS(cache_file)

    cache[["source"]] <- source
    cache[["collection"]] <- collection

    saveRDS(cache, cache_file)

    # complete cube object
    shared_cube <- cache[["cube"]]

    # get cube files
    files <- .file_remove_vsi(unlist(.cube_paths(shared_cube)))

    # mocking HuggingFace operations
    testthat::local_mocked_bindings(
        .hf_file_download = function(repo, file) cache_file,
        .hf_files = function(repo) files,
        .source_collection_name = function(source, collection) repo
    )

    # the cube shared by the dataset describes its images
    cached <- .hf_cache_cube(
        source = source,
        collection = collection,
        bands = NULL,
        tiles = NULL,
        start_date = NULL,
        end_date = NULL
    )

    expect_s3_class(cached, "raster_cube")
    expect_equal(.cube_timeline(cached), .cube_timeline(cube))
    expect_equal(.cube_bands(cached), .cube_bands(cube))
    expect_equal(unlist(.cube_paths(cached)), unlist(.cube_paths(shared_cube)))

    # images are selected from the cache, as they are from a dataset
    timeline <- .cube_timeline(cached)[[1L]]
    selected <- .hf_cache_cube(
        source = source,
        collection = collection,
        bands = "NDVI",
        tiles = .cube_tiles(cached),
        start_date = timeline[[1L]],
        end_date = timeline[[2L]]
    )

    expect_equal(.cube_bands(selected), "NDVI")
    expect_equal(length(.cube_timeline(selected)[[1L]]), 2L)

    # each date informed is a limit of the period selected
    from_date <- .hf_cache_cube(
        source = source,
        collection = collection,
        bands = NULL,
        tiles = NULL,
        start_date = timeline[[2L]],
        end_date = NULL
    )

    # from the start date to the end
    expect_equal(.cube_timeline(from_date)[[1L]], timeline[-1L])

    # from the beggining to the end date
    until_date <- .hf_cache_cube(
        source = source,
        collection = collection,
        bands = NULL,
        tiles = NULL,
        start_date = NULL,
        end_date = timeline[[1L]]
    )

    expect_equal(.cube_timeline(until_date)[[1L]], timeline[[1L]])

    # the images of the dataset are reloaded when the cache does not contains
    # the bands / tiles selected by the user
    expect_warning(
        expect_null(.hf_cache_cube(
            source = source,
            collection = collection,
            bands = "EVI",
            tiles = NULL,
            start_date = NULL,
            end_date = NULL
        ))
    )

    expect_warning(
        expect_null(.hf_cache_cube(
            source = source,
            collection = collection,
            bands = NULL,
            tiles = "000000",
            start_date = NULL,
            end_date = NULL
        ))
    )

    # a cache prepared targeting another dataset is not accepted
    expect_warning(
        expect_null(
            .hf_cache_cube(
                source = source,
                collection = "OTHER",
                bands = NULL,
                tiles = NULL,
                start_date = NULL,
                end_date = NULL
            )
        )
    )
})

test_that("Cache shared by a HuggingFace dataset is validated", {
    repo <- "user/dataset"
    source <- "HF:USER"
    collection <- "DATASET"

    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = system.file("extdata/raster/mod13q1", package = "sits"),
        progress   = FALSE
    )

    # define cache file
    cache_dir <- file.path(tempdir(), "hf_cache_check")
    cache_file <- file.path(cache_dir, "cache.rds")

    # create directory and side effect
    dir.create(cache_dir, showWarnings = FALSE)
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

    # describe cube and create cache
    sits_to_hf(cube, cache = cache_file, repo = repo)

    # a cache describes the collection of the dataset it is shared in
    cache <- readRDS(cache_file)

    cache[["source"]] <- source
    cache[["collection"]] <- collection

    saveRDS(cache, cache_file)

    # complete cube object
    shared_cube <- cache[["cube"]]
    files <- .file_remove_vsi(unlist(.cube_paths(shared_cube)))

    # a cache must be associated with one repository only
    expect_true(.hf_cache_check(cache, source, collection))
    expect_warning(expect_false(.hf_cache_check(list(), source, collection)))
    expect_warning(
        expect_false(.hf_cache_check(cache, source, "OTHER"))
    )
    expect_warning(
        expect_false(.hf_cache_check(cache[-5L], source, collection))
    )

    # a cube prepared with another version of sits is read as it is
    # but we keep the user informed about the issue this may cause
    cache_other <- cache
    cache_other[["sits_version"]] <- "0.0.0"

    expect_warning(
        expect_true(.hf_cache_check(cache_other, source, collection))
    )

    # the images described must be the images the dataset holds, and only them
    testthat::local_mocked_bindings(.hf_files = function(repo) files)
    expect_true(.hf_cache_files(shared_cube, repo))

    # also images of another repository are refused
    expect_warning(expect_false(.hf_cache_files(shared_cube, "other/dataset")))

    # and images the dataset does not hold anymore are reported
    testthat::local_mocked_bindings(.hf_files = function(repo) files[-1L])
    expect_warning(expect_false(.hf_cache_files(shared_cube, repo)))
})

test_that("Signing HuggingFace requests with the token of the user", {
    token_env <- .conf("hf", "token_env")[[1L]]
    gdal_env <- .conf("hf", "token_gdal_env")

    # define function to manage token
    token_set <- function(value) {
        if (is.na(value)) {
            Sys.unsetenv(token_env)
        } else {
            do.call(Sys.setenv, stats::setNames(list(value), token_env))
        }
    }

    # get any existing token
    token_old <- Sys.getenv(token_env, unset = NA)

    # a token defined by another test / env is not part of this one
    .hf_token_flush()

    # reload gdal env variable
    gdal_old <- Sys.getenv(gdal_env)

    # to avoid side effect to other tests, we restore the env at the end of
    # this test
    on.exit(token_set(token_old), add = TRUE)
    on.exit(.hf_token_flush(), add = TRUE)

    # datasets shared as open data can be read without any token, so
    # the auth operations must produce NULL to avoid behavior changes
    token_set(NA)
    expect_null(.hf_token())
    expect_null(.hf_headers())
    expect_null(.hf_token_persist())

    # the gdal configuration of the session is left as it is
    expect_equal(Sys.getenv(gdal_env), gdal_old)

    # a token is verified once in a session
    # > here, we mock the session to return a valid token
    testthat::local_mocked_bindings(
        .hf_token_validate = function(token) invisible(token)
    )

    token_set("hf_token_of_the_user")

    expect_equal(.hf_token(), "hf_token_of_the_user")
    expect_equal(
        .hf_headers(), list(Authorization = "Bearer hf_token_of_the_user")
    )

    # gdal signs its requests with the headers written in a file
    .hf_token_persist()
    header_file <- Sys.getenv(gdal_env)

    expect_true(nzchar(header_file))
    expect_equal(
        readLines(header_file), "Authorization: Bearer hf_token_of_the_user"
    )

    # the headers of a token are written once in a session
    .hf_token_flush()
    .hf_token_persist()
    expect_equal(Sys.getenv(gdal_env), header_file)

    # the configuration of the session is restored when the token is not
    # needed anymore, so it is not informed to other services
    .hf_token_flush()
    expect_equal(Sys.getenv(gdal_env), gdal_old)
})
