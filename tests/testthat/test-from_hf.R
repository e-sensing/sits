test_that("Reading samples and models from HuggingFace", {
    testthat::skip_if_not_installed("arrow")

    # files shared in the repository
    repo_dir <- file.path(tempdir(), "hf_repo")
    dir.create(repo_dir, showWarnings = FALSE)
    on.exit(unlink(repo_dir, recursive = TRUE), add = TRUE)

    # prepare model
    model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 20L))

    # save objects
    saveRDS(model, file.path(repo_dir, "model.rds"))
    saveRDS(samples_modis_ndvi, file.path(repo_dir, "samples.rds"))
    saveRDS(list(a = 1L), file.path(repo_dir, "other.rds"))

    # save samples as parquet
    sits_to_parquet(samples_modis_ndvi, file.path(repo_dir, "samples.parquet"))

    # repository of the user - mock it
    repo_files <- c("README.md", "model.rds")
    testthat::local_mocked_bindings(
        .hf_dataset = function(repo, type) {
            list(
                siblings = purrr::map(repo_files, function(file) {
                    list(rfilename = file)
                })
            )
        },
        .hf_file_download = function(repo, file, type) {
            # copy file to local
            file_local <- tempfile(fileext = paste0(".", .file_ext(file)))
            file.copy(file.path(repo_dir, file), file_local)

            # return local file
            file_local
        }
    )

    # repositories must have a file sits reads
    repo_files <- "README.md"
    expect_error(
        sits_from_hf("user/repo"),
        .conf("messages", ".hf_repo_file_empty"),
        fixed = TRUE
    )

    # a single file is read without informing it
    repo_files <- c("README.md", "model.rds")
    hf_model <- sits_from_hf("user/repo", type = "model")

    expect_s3_class(hf_model, "sits_model")
    expect_equal(sits_labels(hf_model), sits_labels(model))

    # when there are many files, users choose the one to be read
    repo_files <- c(repo_files, "samples.rds", "samples.parquet", "other.rds")
    expect_error(sits_from_hf("user/repo"), "samples.parquet")

    # read samples from rds file
    hf_samples <- sits_from_hf("user/repo", file = "samples.rds")
    expect_s3_class(hf_samples, "sits")
    expect_equal(nrow(hf_samples), nrow(samples_modis_ndvi))

    # read samples from parquet file
    hf_samples <- sits_from_hf("user/repo", file = "samples.parquet")
    expect_s3_class(hf_samples, "sits")
    expect_equal(sits_bands(hf_samples), sits_bands(samples_modis_ndvi))

    # only samples and models are read, and only files of the repository
    expect_error(sits_from_hf("user/repo", file = "other.rds"))
    expect_error(sits_from_hf("user/repo", file = "missing.rds"))
    expect_error(sits_from_hf("user/repo", file = "README.md"))

    # a repository describing a cube is read as a cube, which is only
    # shared in datasets
    repo_files <- c(repo_files, "sits/sits.yml", "sits/cache.rds")
    expect_error(
        sits_from_hf("user/repo", type = "model", output_dir = tempdir()),
        .conf("messages", ".hf_repo_read_cube"),
        fixed = TRUE
    )
})

test_that("Images are downloaded within the limits of HuggingFace", {
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = system.file("extdata/raster/mod13q1", package = "sits"),
        progress   = FALSE
    )

    output_dir <- file.path(tempdir(), "hf_download")
    dir.create(output_dir, showWarnings = FALSE)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

    # limits of the user: 5 requests, none, and then a new window
    batches <- 0L
    limits <- list(
        list(remaining = 5L, reset = 0L),
        list(remaining = 0L, reset = 0L),
        list(remaining = 3000L, reset = 300L)
    )

    testthat::local_mocked_bindings(
        .hf_download_limit = function(repo, file, type) {
            batches <<- batches + 1L
            limits[[min(batches, length(limits))]]
        }
    )

    # images are downloaded in batches fitting the requests available,
    # waiting for the next window when there is none
    expect_message(
        local_cube <- .hf_download_cube(
            cube = cube,
            repo = "user/repo",
            output_dir = output_dir,
            n_tries = 1L,
            multicores = 2L,
            progress = FALSE
        ),
        "HuggingFace"
    )

    expect_equal(batches, 3L)
    expect_equal(sits_timeline(local_cube), sits_timeline(cube))
    expect_equal(length(list.files(output_dir)), 12L)
})

test_that("Requests to HuggingFace are limited and signed", {
    # limits are read from the headers of HuggingFace
    testthat::local_mocked_bindings(
        .head_request = function(...) "response",
        .response_header = function(...) "\"resolvers\";r=2999;t=240"
    )

    # read limits from headers
    expect_equal(
        .hf_download_limit("user/repo", "sits/sits.yml"),
        list(remaining = 2999L, reset = 240L)
    )

    # only requests to HuggingFace are signed
    expect_null(.hf_url_headers("https://example.com/image.tif"))
    expect_null(.hf_url_headers("https://huggingface.co.example.com/a.tif"))
})
