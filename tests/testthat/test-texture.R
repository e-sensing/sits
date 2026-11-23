test_that("Testing texture generation", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    # Create a MODIS cube
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    dir_images <- paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }
    unlink(list.files(dir_images,
        pattern = "\\.tif$",
        full.names = TRUE
    ))
    # Compute the NDVI variance
    class(feature) <- "data.frame"
    texture <- sits_texture(
        cube = feature,
        NDVIVAR = glcm_variance(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIVAR")))

    # Compute the NDVI mean
    texture <- sits_texture(
        cube = feature,
        NDVIMEAN = glcm_mean(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIMEAN")))

    # Compute the NDVI contrast
    texture <- sits_texture(
        cube = feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVICONTRAST")))

    # Compute the NDVI dissimilarity
    texture <- sits_texture(
        cube = feature,
        NDVIDISSIMILARITY = glcm_dissimilarity(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIDISSIMILARITY")))

    # Compute the NDVI homogeneity
    texture <- sits_texture(
        cube = feature,
        NDVIHOMOGEINEITY = glcm_homogeneity(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIHOMOGEINEITY")))

    # Compute the NDVI energy
    texture <- sits_texture(
        cube = feature,
        NDVIENERGY = glcm_energy(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIENERGY")))

    # Compute the NDVI asm
    texture <- sits_texture(
        cube = feature,
        NDVIASM = glcm_asm(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVIASM")))

    # Compute the NDVI std
    texture <- sits_texture(
        cube = feature,
        NDVISTD = glcm_std(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVISTD")))

    # Compute the NDVI correlation
    texture <- sits_texture(
        cube = feature,
        NDVICORRELATION = glcm_correlation(NDVI),
        window_size = 5,
        multicores = 1,
        output_dir = dir_images
    )
    expect_true(all(sits_bands(texture) %in% c("NDVI", "NDVICORRELATION")))

    unlink(dir_images, recursive = TRUE)
})
test_that("Texture - four angles", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    out_dir <- file.path(tempdir(), "texture-four-angles")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    texture <- sits_texture(
        cube = cube,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 3,
        angles = c(0, pi / 4, pi / 2, 3 * pi / 4),
        multicores = 1,
        output_dir = out_dir,
        progress = FALSE
    )
    expect_true("NDVICONTRAST" %in% sits_bands(texture))
    unlink(out_dir, recursive = TRUE)
})
test_that("Texture - logs the start and end of each step", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    out_dir <- file.path(tempdir(), "texture-debug-log")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    .debug(flag = TRUE, output_dir = out_dir)
    on.exit(.debug(flag = FALSE), add = TRUE)
    sits_texture(
        cube = cube,
        NDVIVAR = glcm_variance(NDVI),
        window_size = 3,
        multicores = 1,
        output_dir = out_dir,
        progress = FALSE
    )
    events <- utils::read.csv(
        .file_log_name(out_dir),
        strip.white = TRUE
    )[["event"]]
    steps <- c(
        "call", "tile", "block_data_read", "block_kernel",
        "block_write", "tile_merge"
    )
    purrr::walk(steps, function(step) {
        n_start <- sum(events == paste0("start_", step))
        expect_gt(n_start, 0L)
        expect_equal(sum(events == paste0("end_", step)), n_start)
    })
    unlink(out_dir, recursive = TRUE)
})
test_that("Texture - multicores limits an open cluster", {
    skip_on_cran()
    suppressMessages(sits_parallel(workers = 4L))
    on.exit(suppressMessages(sits_parallel(workers = 0L)), add = TRUE)
    feature <- texture_cube()
    out_dir <- texture_dir("texture-limit-cluster")
    marks <- texture_dir("texture-limit-cluster-marks")
    on.exit(unlink(c(out_dir, marks), recursive = TRUE), add = TRUE)
    texture_small_blocks()
    testthat::local_mocked_bindings(
        .texture_use_gpu = function() FALSE,
        .texture_functions = texture_marker(.texture_functions, marks)
    )
    # a limit the caller asks for is not a cut by memory
    expect_no_warning(sits_texture(
        feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L,
        multicores = 2L,
        output_dir = out_dir,
        progress = FALSE
    ))
    # 40 blocks on 2 of the 4 nodes
    pids <- texture_marker_pids(marks)
    expect_length(pids, 2L)
    expect_false(Sys.getpid() %in% pids)
    # the cluster stays as the caller opened it, and the limit goes
    expect_length(sits_env[["cluster"]], 4L)
    expect_null(sits_env[["forced_multicores"]])
})
test_that("Texture - multicores opens a cluster when none is open", {
    skip_on_cran()
    suppressMessages(sits_parallel(workers = 0L))
    feature <- texture_cube()
    out_dir <- texture_dir("texture-open-cluster")
    marks <- texture_dir("texture-open-cluster-marks")
    on.exit(unlink(c(out_dir, marks), recursive = TRUE), add = TRUE)
    texture_small_blocks()
    testthat::local_mocked_bindings(
        .texture_use_gpu = function() FALSE,
        .texture_functions = texture_marker(.texture_functions, marks)
    )
    sits_texture(
        feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L,
        multicores = 2L,
        output_dir = out_dir,
        progress = FALSE
    )
    pids <- texture_marker_pids(marks)
    expect_length(pids, 2L)
    expect_false(Sys.getpid() %in% pids)
    # the call closes the cluster it opened
    expect_false(.parallel_is_open())
})
test_that("Texture - merge in a worker while the next date runs", {
    skip_on_cran()
    suppressMessages(sits_parallel(workers = 0L))
    cube <- texture_cube(dates = 2L)
    dates <- as.character(sits_timeline(cube))
    texture_small_blocks()
    run <- function(out_dir, multicores) {
        .debug(flag = TRUE, output_dir = out_dir)
        on.exit(.debug(flag = FALSE), add = TRUE)
        sits_texture(
            cube,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            multicores = multicores,
            output_dir = out_dir,
            progress = FALSE
        )
    }
    out_two <- texture_dir("texture-queue-two")
    out_one <- texture_dir("texture-queue-one")
    on.exit(unlink(c(out_two, out_one), recursive = TRUE), add = TRUE)
    two <- run(out_two, 2L)
    ev <- texture_events(out_two)
    merges <- ev[ev[["event"]] == "start_tile_merge", ]
    expect_equal(nrow(merges), 2L)
    # the merge runs in a worker, not in this process
    expect_true(all(merges[["pid"]] != Sys.getpid()))
    # the blocks of the second date start before the merge of the first
    blocks_start <- ev[ev[["event"]] == "start_tile_blocks", ]
    expect_equal(nrow(blocks_start), 2L)
    expect_lt(max(blocks_start[["timestamp"]]), min(merges[["timestamp"]]))
    # each tile event once per date, in order
    steps <- c(
        "start_tile", "start_tile_blocks", "end_tile_blocks",
        "start_tile_merge", "end_tile_merge", "end_tile"
    )
    for (date in dates) {
        of_date <- ev[grepl(date, ev[["value"]], fixed = TRUE) &
            ev[["event"]] %in% steps, ]
        of_date <- of_date[order(of_date[["timestamp"]]), ]
        expect_equal(of_date[["event"]], steps)
        # 40 blocks of 32 x 32 in a tile of 147 x 255
        expect_equal(
            sum(ev[["event"]] == "start_block" &
                grepl(date, ev[["value"]], fixed = TRUE)),
            ceiling(147 / 32) * ceiling(255 / 32)
        )
    }
    # the same values as one core
    one <- run(out_one, 1L)
    for (date in dates) {
        a <- sits_select(one, bands = "NDVICONTRAST", dates = date)
        b <- sits_select(two, bands = "NDVICONTRAST", dates = date)
        expect_equal(
            .raster_get_values(.raster_open_rast(.tile_path(a))),
            .raster_get_values(.raster_open_rast(.tile_path(b)))
        )
    }
    # a second call runs no block and starts each tile again
    since <- as.numeric(Sys.time())
    run(out_two, 2L)
    again <- texture_events(out_two)
    again <- again[again[["timestamp"]] >= since, ]
    expect_equal(sum(again[["event"]] == "start_block"), 0L)
    expect_equal(sum(again[["event"]] == "start_tile"), 2L)
    # with the file of the second date removed, only its blocks run again
    second <- sits_select(two, bands = "NDVICONTRAST", dates = dates[[2L]])
    unlink(.tile_path(second))
    since <- as.numeric(Sys.time())
    run(out_two, 2L)
    third <- texture_events(out_two)
    third <- third[third[["timestamp"]] >= since &
        third[["event"]] == "start_block", ]
    expect_gt(nrow(third), 0L)
    expect_true(all(grepl(dates[[2L]], third[["value"]], fixed = TRUE)))
})
test_that("Texture - resume skips the blocks left by a failed merge", {
    skip_on_cran()
    suppressMessages(sits_parallel(workers = 0L))
    feature <- texture_cube()
    out_dir <- texture_dir("texture-resume-blocks")
    ref_dir <- texture_dir("texture-resume-blocks-ref")
    on.exit(unlink(c(out_dir, ref_dir), recursive = TRUE), add = TRUE)
    texture_small_blocks()
    run <- function(dir) {
        sits_texture(
            feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            multicores = 1L,
            output_dir = dir,
            progress = FALSE
        )
    }
    reference <- run(ref_dir)
    # the merge fails once, so the block files stay
    real_merge <- .texture_merge
    failed <- FALSE
    testthat::local_mocked_bindings(.texture_merge = function(...) {
        if (!failed) {
            failed <<- TRUE
            stop("merge fails on purpose")
        }
        real_merge(...)
    })
    expect_error(run(out_dir), "merge fails on purpose")
    expect_gt(length(list.files(
        file.path(out_dir, ".sits"), pattern = "[.]tif$"
    )), 0L)
    .debug(flag = TRUE, output_dir = out_dir)
    on.exit(.debug(flag = FALSE), add = TRUE)
    resumed <- run(out_dir)
    .debug(flag = FALSE)
    ev <- texture_events(out_dir)
    # every block is read back from its file, no kernel runs again
    expect_equal(sum(ev[["event"]] == "start_block"), 40L)
    expect_equal(sum(ev[["event"]] == "start_block_kernel"), 0L)
    a <- sits_select(reference, bands = "NDVICONTRAST")
    b <- sits_select(resumed, bands = "NDVICONTRAST")
    expect_equal(
        .raster_get_values(.raster_open_rast(.tile_path(a))),
        .raster_get_values(.raster_open_rast(.tile_path(b)))
    )
})
test_that("Texture - worker dies in a merge, merge resent", {
    skip_on_cran()
    skip_on_os("windows")
    suppressMessages(sits_parallel(workers = 0L))
    cube <- texture_cube(dates = 2L)
    root <- texture_dir("texture-merge-death")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    texture_small_blocks()
    texture <- function(name, multicores) {
        out_dir <- file.path(root, name)
        dir.create(out_dir)
        sits_texture(
            cube,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            multicores = multicores,
            output_dir = out_dir,
            progress = FALSE
        )
    }
    reference <- texture("sequential", 1L)
    suppressMessages(sits_parallel(workers = 2L))
    on.exit(suppressMessages(sits_parallel(workers = 0L)), add = TRUE)
    # the first merge in a worker writes its file, then kills the worker;
    # the flag keeps the other traced worker from killing itself
    flag <- file.path(root, "killed")
    kill_after_merge <- function(flag) {
        trace(
            ".raster_merge_blocks",
            exit = bquote(if (!file.exists(.(flag))) {
                file.create(.(flag))
                tools::pskill(Sys.getpid(), tools::SIGKILL)
            }),
            where = asNamespace("sits"), print = FALSE
        )
        NULL
    }
    environment(kill_after_merge) <- globalenv()
    parallel::clusterCall(sits_env[["cluster"]], kill_after_merge, flag)
    texture_queue <- suppressMessages(texture("queue", 2L))
    expect_true(file.exists(flag))
    # no block or partial merge is left in .sits
    expect_length(list.files(
        file.path(root, "queue", ".sits"), pattern = "[.]tif$"
    ), 0L)
    for (date in as.character(sits_timeline(cube))) {
        a <- sits_select(reference, bands = "NDVICONTRAST", dates = date)
        b <- sits_select(texture_queue, bands = "NDVICONTRAST", dates = date)
        expect_equal(
            .raster_get_values(.raster_open_rast(.tile_path(a))),
            .raster_get_values(.raster_open_rast(.tile_path(b)))
        )
    }
})
test_that("Texture - workers asked and the warning when memory cuts them", {
    skip_on_cran()
    feature <- texture_cube()
    on.exit(suppressMessages(sits_parallel(workers = 0L)), add = TRUE)
    # one call: the pids of the workers and the warnings it gave; the memory
    # cut to one worker is keyed on budgets no default uses: memsize 5 for
    # the RAM rule, gpu_memsize 7.25 for the GPU rule
    run <- function(name, cluster = 0L, cut_ram = FALSE, cut_gpu = FALSE,
                    gpu = FALSE, ...) {
        suppressMessages(sits_parallel(workers = 0L))
        if (cluster > 0L) {
            suppressMessages(sits_parallel(workers = cluster))
        }
        out_dir <- texture_dir(name)
        marks <- texture_dir(paste0(name, "-marks"))
        on.exit(unlink(c(out_dir, marks), recursive = TRUE), add = TRUE)
        texture_small_blocks()
        real_max <- .jobs_max_multicores
        testthat::local_mocked_bindings(
            .texture_use_gpu = function() gpu,
            .texture_functions = texture_marker(.texture_functions, marks),
            .texture_torch_functions = texture_marker(
                .texture_torch_functions, marks
            ),
            .jobs_max_multicores = function(job_block_memsize, memsize,
                                            multicores) {
                if ((cut_ram && memsize == 5) || (cut_gpu && memsize == 7.25)) {
                    return(min(multicores, 1L))
                }
                real_max(job_block_memsize, memsize, multicores)
            }
        )
        warnings <- testthat::capture_warnings(sits_texture(
            feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            output_dir = out_dir,
            progress = FALSE,
            ...
        ))
        list(pids = texture_marker_pids(marks), warnings = warnings)
    }
    # the budget words: memsize alone, and gpu_memsize
    names_memsize <- function(w) isTRUE(grepl("(^|[^_])memsize", w))
    names_gpu <- function(w) isTRUE(grepl("gpu_memsize", w, fixed = TRUE))
    has_numbers <- function(w, got, asked) {
        isTRUE(grepl(paste0("\\b", got, "\\b"), w)) &&
            isTRUE(grepl(paste0("\\b", asked, "\\b"), w))
    }
    # 1. the open cluster is the request; memory cuts it to 1
    one <- run("texture-ask-cluster-cut", cluster = 4L, cut_ram = TRUE,
        memsize = 5L)
    expect_length(one[["pids"]], 1L)
    expect_length(one[["warnings"]], 1L)
    expect_true(names_memsize(one[["warnings"]][1L]))
    expect_false(names_gpu(one[["warnings"]][1L]))
    expect_true(has_numbers(one[["warnings"]][1L], 1L, 4L))
    # 2. the caller asks for 1: no warning
    two <- run("texture-ask-one", cluster = 4L, cut_ram = TRUE, memsize = 5L,
        multicores = 1L)
    expect_length(two[["pids"]], 1L)
    expect_length(two[["warnings"]], 0L)
    # 3. no cluster, 4 asked, memory allows 1
    three <- run("texture-ask-four-cut", cut_ram = TRUE, memsize = 5L,
        multicores = 4L)
    expect_length(three[["warnings"]], 1L)
    expect_true(names_memsize(three[["warnings"]][1L]))
    expect_true(has_numbers(three[["warnings"]][1L], 1L, 4L))
    # 4. the open cluster is the request and memory allows it all
    four <- run("texture-ask-cluster", cluster = 4L)
    expect_length(four[["pids"]], 4L)
    expect_length(four[["warnings"]], 0L)
    # 5. no cluster and no request: the default of 2
    five <- run("texture-ask-default")
    expect_length(five[["pids"]], 2L)
    expect_length(five[["warnings"]], 0L)
    # 6. an open cluster smaller than the request is not enlarged: it runs
    # with its 2 nodes and says so
    six <- run("texture-ask-above-cluster", cluster = 2L, multicores = 4L)
    expect_length(six[["pids"]], 2L)
    expect_length(six[["warnings"]], 1L)
    expect_true(isTRUE(grepl("cluster", six[["warnings"]][1L], fixed = TRUE)))
    expect_true(has_numbers(six[["warnings"]][1L], 2L, 4L))
    skip_if_not_installed("torch")
    # 7. on the GPU, only the GPU budget cuts
    seven <- run("texture-ask-gpu-cut", gpu = TRUE, cut_gpu = TRUE,
        multicores = 2L, gpu_memsize = 7.25)
    expect_length(seven[["pids"]], 1L)
    expect_length(seven[["warnings"]], 1L)
    expect_true(names_gpu(seven[["warnings"]][1L]))
    expect_false(names_memsize(seven[["warnings"]][1L]))
    expect_true(has_numbers(seven[["warnings"]][1L], 1L, 2L))
    # 8. on the GPU, only the RAM budget cuts
    eight <- run("texture-ask-gpu-ram-cut", gpu = TRUE, cut_ram = TRUE,
        multicores = 2L, memsize = 5L, gpu_memsize = 7.25)
    expect_length(eight[["pids"]], 1L)
    expect_length(eight[["warnings"]], 1L)
    expect_true(names_memsize(eight[["warnings"]][1L]))
    expect_false(names_gpu(eight[["warnings"]][1L]))
    # 9. on the GPU, both budgets cut: both are named
    nine <- run("texture-ask-gpu-both-cut", gpu = TRUE, cut_ram = TRUE,
        cut_gpu = TRUE, multicores = 2L, memsize = 5L, gpu_memsize = 7.25)
    expect_length(nine[["warnings"]], 1L)
    expect_true(names_memsize(nine[["warnings"]][1L]))
    expect_true(names_gpu(nine[["warnings"]][1L]))
})
