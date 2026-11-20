gpu_tiles_model <- function(seed) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
    sits_train(
        samples_modis_ndvi,
        sits_mlp(
            layers = c(32L, 32L), dropout_rates = c(0.2, 0.2), epochs = 2L,
            verbose = FALSE
        )
    )
}

# Classify on the dataloader (gpu = TRUE) or CPU path in 64 x 64 blocks;
# returns the probs and the counts of dataloaders and of their blocks.
classify_gpu_tiles <- function(gpu, cube, ml_model, output_dir,
                               version = "v1", verbose = FALSE) {
    n_loaders <- 0L
    n_blocks <- 0L
    trace(
        "dataloader",
        tracer = function() {
            n_loaders <<- n_loaders + 1L
            n_blocks <<- n_blocks + length(get("dataset", parent.frame()))
        },
        where = asNamespace("torch"),
        print = FALSE
    )
    on.exit(untrace("dataloader", where = asNamespace("torch")), add = TRUE)
    probs <- testthat::with_mocked_bindings(
        sits_classify(
            data = cube, ml_model = ml_model, memsize = 4L, multicores = 1L,
            block_size = c(ncols = 64L, nrows = 64L),
            output_dir = output_dir, version = version, verbose = verbose,
            progress = FALSE
        ),
        .torch_gpu_classification = function() gpu
    )
    list(probs = probs, n_loaders = n_loaders, n_blocks = n_blocks)
}

tile_values <- function(probs, name) {
    .raster_open_rast(.fi(probs[probs[["tile"]] == name, ])[["path"]])[]
}

tile_extent <- function(probs, name) {
    as.vector(.raster_extent_rast(
        .raster_open_rast(.fi(probs[probs[["tile"]] == name, ])[["path"]])
    ))
}

test_that("GPU classification - one dataloader for all tiles", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    root <- file.path(tempdir(), "gpu_tiles")
    unlink(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cube <- gpu_tiles_cube(root)
    model <- gpu_tiles_model(2971L)
    dirs <- file.path(root, c("ref", "gpu"))
    for (d in dirs) dir.create(d)
    # the CPU path counts luz's dataloaders of each block, not checked here
    ref <- classify_gpu_tiles(FALSE, cube, model, dirs[[1L]])
    # record, when each tile reports its end, which probs files exist
    probs_file <- function(name) {
        .file_derived_name(
            tile = cube[cube[["tile"]] == name, ], band = "probs",
            version = "v1", output_dir = dirs[[2L]]
        )
    }
    at_end <- list()
    gpu <- withCallingHandlers(
        classify_gpu_tiles(TRUE, cube, model, dirs[[2L]], verbose = TRUE),
        message = function(m) {
            tile <- regmatches(
                conditionMessage(m),
                regexec("^Tile '(.*)' finished", conditionMessage(m))
            )[[1L]]
            if (length(tile) == 2L) {
                at_end[[tile[[2L]]]] <<- c(
                    A = file.exists(probs_file("A")),
                    B = file.exists(probs_file("B"))
                )
            }
            invokeRestart("muffleMessage")
        }
    )
    expect_equal(gpu[["n_loaders"]], 1L)
    expect_equal(gpu[["probs"]][["tile"]], c("A", "B"))
    # tile A is merged before tile B ends, tile B when it ends
    expect_equal(at_end[["A"]], c(A = TRUE, B = FALSE))
    expect_equal(at_end[["B"]], c(A = TRUE, B = TRUE))
    for (name in c("A", "B")) {
        # within one unit of the INT2U scale, as the CPU/GPU test
        expect_lte(
            max(abs(tile_values(gpu[["probs"]], name) -
                    tile_values(ref[["probs"]], name)), na.rm = TRUE),
            1L
        )
        expect_equal(
            is.na(tile_values(gpu[["probs"]], name)),
            is.na(tile_values(ref[["probs"]], name))
        )
        # the file keeps the extent of the CPU file
        expect_identical(
            tile_extent(gpu[["probs"]], name),
            tile_extent(ref[["probs"]], name)
        )
    }
    # no block file is left
    expect_length(
        list.files(dirs[[2L]], pattern = "block", recursive = TRUE,
                   all.files = TRUE),
        0L
    )
})

test_that("GPU classification - vector cube", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    skip_if_not_installed("snic")
    root <- file.path(tempdir(), "gpu_tiles_vector")
    unlink(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    dirs <- file.path(root, c("segments", "ref", "gpu"))
    purrr::walk(dirs, dir.create, recursive = TRUE)
    segments <- sits_segment(
        cube = gpu_tiles_cube(root), seg_fn = sits_snic(spacing = 50L),
        output_dir = dirs[[1L]], memsize = 4L, multicores = 1L,
        progress = FALSE
    )
    model <- gpu_tiles_model(2971L)
    ref <- classify_gpu_tiles(FALSE, segments, model, dirs[[2L]])
    gpu <- classify_gpu_tiles(TRUE, segments, model, dirs[[3L]])
    expect_equal(gpu[["n_loaders"]], 1L)
    expect_identical(class(gpu[["probs"]]), class(ref[["probs"]]))
    expect_s3_class(gpu[["probs"]], "probs_vector_cube")
    expect_identical(gpu[["probs"]][["vector_info"]], segments[["vector_info"]])
    purrr::walk(c("A", "B"), function(name) {
        # within one unit of the INT2U scale, as the CPU/GPU test
        expect_lte(
            max(abs(tile_values(gpu[["probs"]], name) -
                    tile_values(ref[["probs"]], name)), na.rm = TRUE),
            1L
        )
    })
})

test_that("GPU classification - done tiles and recovered blocks", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    root <- file.path(tempdir(), "gpu_tiles_recovery")
    unlink(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cube <- gpu_tiles_cube(root)
    model <- gpu_tiles_model(2971L)
    dir <- file.path(root, "gpu")
    dir.create(dir)
    first <- classify_gpu_tiles(TRUE, cube, model, dir)
    path_a <- .fi(first[["probs"]][1L, ])[["path"]]
    path_b <- .fi(first[["probs"]][2L, ])[["path"]]
    values_b <- tile_values(first[["probs"]], "B")
    mtime_a <- file.mtime(path_a)
    Sys.sleep(1.1)
    # a done tile is not classified again
    unlink(path_b)
    again <- classify_gpu_tiles(TRUE, cube, model, dir)
    expect_equal(again[["n_loaders"]], 1L)
    expect_equal(file.mtime(path_a), mtime_a)
    expect_true(file.exists(path_b))
    expect_lte(max(abs(tile_values(again[["probs"]], "B") - values_b),
                   na.rm = TRUE), 1L)
    # a dir in place of its VRT makes the merge of tile B fail after all its
    # blocks are written: they are kept
    unlink(path_b)
    vrt_b <- sub("tif$", "vrt", path_b)
    dir.create(vrt_b)
    # GDAL also warns that it cannot write the VRT
    expect_error(suppressWarnings(classify_gpu_tiles(TRUE, cube, model, dir)))
    expect_false(file.exists(path_b))
    expect_gt(length(list.files(dir, pattern = "block", recursive = TRUE,
                                all.files = TRUE)), 0L)
    unlink(vrt_b, recursive = TRUE)
    # every block of tile B is recovered, so no dataloader is created
    last <- classify_gpu_tiles(TRUE, cube, model, dir)
    expect_equal(last[["n_loaders"]], 0L)
    expect_equal(file.mtime(path_a), mtime_a)
    expect_lte(max(abs(tile_values(last[["probs"]], "B") - values_b),
                   na.rm = TRUE), 1L)
})

test_that("GPU classification - tiles in two CRSs and partial recovery", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    root <- file.path(tempdir(), "gpu_tiles_crs")
    unlink(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    cube <- gpu_tiles_cube_two_crs(root)
    model <- gpu_tiles_model(2971L)
    dirs <- file.path(root, c("ref", "gpu"))
    for (d in dirs) dir.create(d)
    ref <- classify_gpu_tiles(FALSE, cube, model, dirs[[1L]])
    probs_file <- function(i) {
        .file_derived_name(
            tile = cube[i, ], band = "probs", version = "v1",
            output_dir = dirs[[2L]]
        )
    }
    # a dir in place of its VRT makes the merge of tile B fail
    vrt_b <- sub("tif$", "vrt", probs_file(2L))
    dir.create(vrt_b)
    # GDAL also warns that it cannot write the VRT
    expect_error(suppressWarnings(
        classify_gpu_tiles(TRUE, cube, model, dirs[[2L]])
    ))
    blocks <- list.files(dirs[[2L]], pattern = "block", recursive = TRUE,
                         all.files = TRUE, full.names = TRUE)
    # 127 x 147 pixels in blocks of 64 x 64
    expect_length(blocks, 6L)
    unlink(c(blocks[1:2], vrt_b), recursive = TRUE)
    # tile A again, so both CRSs share the dataloader
    unlink(probs_file(1L))
    msgs <- character(0L)
    gpu <- withCallingHandlers(
        classify_gpu_tiles(TRUE, cube, model, dirs[[2L]], verbose = TRUE),
        message = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
        }
    )
    # tile A and the two deleted blocks of tile B are read
    expect_equal(gpu[["n_blocks"]], 6L + 2L)
    expect_equal(sum(startsWith(msgs, "Elapsed time of")), 2L)
    for (i in 1:2) {
        name <- cube[["tile"]][[i]]
        out <- .raster_open_rast(probs_file(i))
        expect_true(terra::same.crs(out, .raster_open_rast(.tile_path(
            cube[i, ]))))
        expect_lte(
            max(abs(out[] - tile_values(ref[["probs"]], name)),
                na.rm = TRUE),
            1L
        )
        expect_equal(is.na(out[]), is.na(tile_values(ref[["probs"]], name)))
    }
    expect_length(
        list.files(dirs[[2L]], pattern = "block", recursive = TRUE,
                   all.files = TRUE),
        0L
    )
})
