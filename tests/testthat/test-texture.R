# Every debug event of every process that wrote to the log of out_dir
texture_events <- function(out_dir) {
    files <- list.files(
        file.path(out_dir, ".sits"), pattern = "[.]log$", full.names = TRUE
    )
    do.call(rbind, lapply(files, utils::read.csv, strip.white = TRUE))
}
# size() of the backend of one device for a block of 512 x 512 in an image
# of 8192 x 8192, two paths
texture_size <- function(gpu, metrics = "glcm_contrast", window_size = 7L,
                         memsize = 10000, gpu_memsize = 1000,
                         multicores = 64L) {
    backend <- testthat::with_mocked_bindings(
        .texture_backend(gpu_memsize = gpu_memsize, metrics = metrics),
        .texture_use_gpu = function() gpu
    )
    backend[["size"]](
        block = c(nrows = 512L, ncols = 512L),
        overlap = ceiling(window_size / 2L) - 1L,
        npaths = 2L,
        window_size = window_size,
        image_size = c(nrows = 8192L, ncols = 8192L),
        memsize = memsize,
        multicores = multicores
    )
}
# the config without the options of one backend, restored at the exit of
# the caller (withr is not a dependency of sits)
texture_drop_backend <- function(backend, env = parent.frame()) {
    opts <- sits_env[["config"]][["texture_options"]]
    do.call(on.exit, list(
        bquote(sits_env[["config"]][["texture_options"]] <- .(opts)),
        add = TRUE, after = FALSE
    ), envir = env)
    sits_env[["config"]][["texture_options"]][["memory"]][[backend]] <- NULL
}
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
    out_dir <- normalizePath(tempfile("texture-four-angles"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
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
})
test_that("Texture - logs the start and end of each step", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    out_dir <- normalizePath(tempfile("texture-debug-log"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
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
})
test_that("Texture - multicores limits an open cluster", {
    skip_on_cran()
    out_dir <- normalizePath(
        tempfile("texture-limit-cluster"), mustWork = FALSE
    )
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    # the workers of the cluster write their pids to the debug log
    suppressMessages(
        sits_parallel(workers = 4L, log = TRUE, output_dir = out_dir)
    )
    on.exit(suppressMessages(sits_parallel(workers = 0L)), add = TRUE)
    on.exit(.debug(flag = FALSE), add = TRUE)
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    testthat::local_mocked_bindings(.texture_use_gpu = function() FALSE)
    # a limit the caller asks for is not a cut by memory
    expect_no_warning(sits_texture(
        feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L,
        block_size = c(nrows = 32L, ncols = 32L),
        multicores = 2L,
        output_dir = out_dir,
        progress = FALSE
    ))
    # 40 blocks on 2 of the 4 nodes
    ev <- texture_events(out_dir)
    pids <- unique(ev[["pid"]][ev[["event"]] == "start_block_kernel"])
    expect_length(pids, 2L)
    expect_false(Sys.getpid() %in% pids)
    # the cluster stays as the caller opened it, and the limit goes
    expect_length(sits_env[["cluster"]], 4L)
    expect_null(sits_env[["forced_multicores"]])
})
test_that("Texture - multicores opens a cluster when none is open", {
    skip_on_cran()
    suppressMessages(sits_parallel(workers = 0L))
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-open-cluster"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    # the call starts its workers with the debug log of the main process
    .debug(flag = TRUE, output_dir = out_dir)
    on.exit(.debug(flag = FALSE), add = TRUE)
    testthat::local_mocked_bindings(.texture_use_gpu = function() FALSE)
    sits_texture(
        feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L,
        block_size = c(nrows = 32L, ncols = 32L),
        multicores = 2L,
        output_dir = out_dir,
        progress = FALSE
    )
    ev <- texture_events(out_dir)
    pids <- unique(ev[["pid"]][ev[["event"]] == "start_block_kernel"])
    expect_length(pids, 2L)
    expect_false(Sys.getpid() %in% pids)
    # the call closes the cluster it opened
    expect_false(.parallel_is_open())
})
test_that("Texture - merge in a worker while the next date runs", {
    skip_on_cran()
    suppressMessages(sits_parallel(workers = 0L))
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    cube <- sits_select(
        cube, bands = "NDVI", dates = c("2013-09-14", "2013-10-16")
    )
    dates <- as.character(sits_timeline(cube))
    run <- function(out_dir, multicores) {
        .debug(flag = TRUE, output_dir = out_dir)
        on.exit(.debug(flag = FALSE), add = TRUE)
        sits_texture(
            cube,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            block_size = c(nrows = 32L, ncols = 32L),
            multicores = multicores,
            output_dir = out_dir,
            progress = FALSE
        )
    }
    out_two <- normalizePath(tempfile("texture-queue-two"), mustWork = FALSE)
    dir.create(out_two)
    out_one <- normalizePath(tempfile("texture-queue-one"), mustWork = FALSE)
    dir.create(out_one)
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
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(
        tempfile("texture-resume-blocks"), mustWork = FALSE
    )
    dir.create(out_dir)
    ref_dir <- normalizePath(
        tempfile("texture-resume-blocks-ref"), mustWork = FALSE
    )
    dir.create(ref_dir)
    on.exit(unlink(c(out_dir, ref_dir), recursive = TRUE), add = TRUE)
    run <- function(dir) {
        sits_texture(
            feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            block_size = c(nrows = 32L, ncols = 32L),
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
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    cube <- sits_select(
        cube, bands = "NDVI", dates = c("2013-09-14", "2013-10-16")
    )
    root <- normalizePath(tempfile("texture-merge-death"), mustWork = FALSE)
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    texture <- function(name, multicores) {
        out_dir <- file.path(root, name)
        dir.create(out_dir)
        sits_texture(
            cube,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            block_size = c(nrows = 32L, ncols = 32L),
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
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    on.exit(suppressMessages(sits_parallel(workers = 0L)), add = TRUE)
    # one call: the pids of the workers and the warnings it gave; the memory
    # cut to one worker is keyed on budgets no default uses: memsize 5 for
    # the RAM rule, gpu_memsize 7.25 for the GPU rule
    run <- function(name, cluster = 0L, cut_ram = FALSE, cut_gpu = FALSE,
                    gpu = FALSE, ...) {
        suppressMessages(sits_parallel(workers = 0L))
        out_dir <- normalizePath(tempfile(name), mustWork = FALSE)
        dir.create(out_dir)
        on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
        # every process of the call writes its pid to the debug log
        .debug(flag = TRUE, output_dir = out_dir)
        on.exit(.debug(flag = FALSE), add = TRUE)
        if (cluster > 0L) {
            suppressMessages(sits_parallel(
                workers = cluster, log = TRUE, output_dir = out_dir
            ))
        }
        real_budget <- .jobs_budget
        testthat::local_mocked_bindings(
            .texture_use_gpu = function() gpu,
            # a block of two thirds of what the call leaves gives one worker
            .jobs_budget = function(name, memsize, block_memsize,
                                    worker_memsize = 0, call_memsize = 0) {
                if ((cut_ram && memsize == 5) || (cut_gpu && memsize == 7.25)) {
                    block_memsize <- (memsize - call_memsize) / 1.5 -
                        worker_memsize
                }
                real_budget(name, memsize, block_memsize, worker_memsize,
                    call_memsize)
            }
        )
        warnings <- testthat::capture_warnings(sits_texture(
            feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            block_size = c(nrows = 32L, ncols = 32L),
            output_dir = out_dir,
            progress = FALSE,
            ...
        ))
        ev <- texture_events(out_dir)
        pids <- unique(ev[["pid"]][ev[["event"]] == "start_block_kernel"])
        list(pids = pids, warnings = warnings)
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
    # 7. on the GPU, only the GPU budget cuts; memsize 20 keeps the RAM
    # budget from cutting
    seven <- run("texture-ask-gpu-cut", gpu = TRUE, cut_gpu = TRUE,
        multicores = 2L, memsize = 20L, gpu_memsize = 7.25)
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
test_that("Texture - block_size sets the blocks of the call", {
    skip_on_cran()
    suppressMessages(sits_parallel(workers = 0L))
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-block-size"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    .debug(flag = TRUE, output_dir = out_dir)
    on.exit(.debug(flag = FALSE), add = TRUE)
    sits_texture(
        feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L,
        block_size = c(nrows = 32L, ncols = 32L),
        multicores = 2L,
        output_dir = out_dir,
        progress = FALSE
    )
    ev <- texture_events(out_dir)
    # 40 blocks of 32 x 32 in a tile of 147 x 255
    expect_equal(sum(ev[["event"]] == "start_block"), 40L)
})
test_that("Texture - block_size is checked", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(
        tempfile("texture-block-size-check"), mustWork = FALSE
    )
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    for (bad in list(c(nrows = 1L, ncols = 32L), c(rows = 32L, cols = 32L),
                     c(nrows = 32L, ncols = 10000L))) {
        expect_error(sits_texture(
            feature,
            NDVICONTRAST = glcm_contrast(NDVI),
            window_size = 5L,
            block_size = bad,
            output_dir = out_dir,
            progress = FALSE
        ), "block_size")
    }
})

test_that("sits_texture gives the backend the GLCM functions it calls", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-metrics"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    seen <- new.env()
    real <- .texture_backend
    testthat::local_mocked_bindings(
        .texture_backend = function(gpu_memsize, metrics) {
            seen[["metrics"]] <- metrics
            real(gpu_memsize = gpu_memsize, metrics = metrics)
        },
        .texture_use_gpu = function() FALSE
    )
    suppressMessages(sits_texture(
        feature, NDVIASM = glcm_asm(NDVI), window_size = 3L,
        block_size = c(nrows = 32L, ncols = 32L),
        multicores = 1L, output_dir = out_dir, progress = FALSE
    ))
    # the GPU rule counts the pairs of ASM only if it sees glcm_asm
    expect_true("glcm_asm" %in% seen[["metrics"]])
})

test_that("the texture runs torch only on CUDA", {
    # MPS was never measured, so torch runs only on CUDA
    gpu <- function(any, cuda) testthat::with_mocked_bindings(
        .texture_use_gpu(),
        .torch_gpu_available = function() any,
        .torch_cuda_enabled = function() cuda
    )
    expect_true(gpu(TRUE, TRUE))
    expect_false(gpu(TRUE, FALSE))
    expect_false(gpu(FALSE, FALSE))
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
    expect_identical(cpu[["fns"]], .texture_cpp_functions)
})

test_that("sits_texture runs torch on the GPU in multicores workers", {
    skip_on_cran()
    skip_if_not_installed("torch")
    suppressMessages(sits_parallel(workers = 0L))
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-gpu-workers"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    # the call starts its workers with the debug log of the main process
    .debug(flag = TRUE, output_dir = out_dir)
    on.exit(.debug(flag = FALSE), add = TRUE)
    # the kernel still picks its device from the hardware, so it runs on
    # the CPU here
    testthat::local_mocked_bindings(.texture_use_gpu = function() TRUE)
    sits_texture(
        feature,
        NDVICONTRAST = glcm_contrast(NDVI),
        window_size  = 5L,
        block_size   = c(nrows = 32L, ncols = 32L),
        multicores   = 2L,
        output_dir   = out_dir,
        progress     = FALSE
    )
    ev <- texture_events(out_dir)
    pids <- unique(ev[["pid"]][ev[["event"]] == "start_block_kernel"])
    expect_length(pids, 2L)
    expect_false(Sys.getpid() %in% pids)
})

test_that("the GPU backend fits its workers and block in gpu_memsize", {
    # the formula of the rule is written again here from the contract, with
    # the key values typed; the test cannot see whether the keys hold for
    # another GPU, tile or metric, which only a run on the server shows
    bloat_box <- .conf(c("texture_options", "memory", "gpu", "gpu_bloat_box"))
    # server series of 2026-11-24: 0.689 GB + 19.2 copies, R squared 0.999;
    # a run of 7 tiles with blocks of 7.26 Mpx needed 19.6, so 20
    expect_equal(bloat_box, 20)
    bloat_pairs <- .conf(
        c("texture_options", "memory", "gpu", "gpu_bloat_pairs")
    )
    # each worker holds a CUDA context besides its block
    worker <- .conf(c("texture_options", "memory", "gpu", "gpu_worker_memsize"))
    ram_bloat <- .conf(c("texture_options", "memory", "gpu", "ram_bloat"))
    # and the R process with torch in RAM besides its bands
    ram_worker <- .conf(
        c("texture_options", "memory", "gpu", "ram_worker_memsize")
    )
    expect_true(is.numeric(bloat_box) && bloat_box > 0)
    expect_true(is.numeric(bloat_pairs) && bloat_pairs > 0)
    expect_true(is.numeric(worker) && worker > 0)
    pixels <- function(block, window_size) {
        overlap <- ceiling(window_size / 2L) - 1L
        (block[["nrows"]] + 2L * overlap) * (block[["ncols"]] + 2L * overlap)
    }
    # GB on the GPU of one block: the box metrics hold float64 images of
    # the block; ASM and energy hold long codes of the pairs of each window
    need <- function(block, window_size, metrics) {
        if (any(c("glcm_asm", "glcm_energy") %in% metrics)) {
            pairs <- 2 * window_size * (window_size - 1)
            pixels(block, window_size) * pairs * 8 * bloat_pairs * 1e-9
        } else {
            pixels(block, window_size) * 8 * bloat_box * 1e-9
        }
    }
    # GB in RAM of one block on the GPU path: the bands in doubles
    ram_need <- function(block, window_size, npaths = 2L) {
        pixels(block, window_size) * npaths * 8 * ram_bloat * 1e-9
    }
    block <- c(nrows = 512L, ncols = 512L)
    image <- c(nrows = 8192L, ncols = 8192L)
    # memsize is large by default, so only the GPU limits the workers
    size <- function(gpu_memsize, window_size, metrics, multicores = 64L,
                     memsize = 10000) {
        backend <- testthat::with_mocked_bindings(
            .texture_backend(gpu_memsize = gpu_memsize, metrics = metrics),
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
    box <- "glcm_contrast"
    pairs <- c("glcm_contrast", "glcm_asm")
    # budgets in blocks of window 5, so no case reaches the 64 workers
    g_box <- 20 * (need(block, 5L, box) + worker)
    g_pairs <- 20 * (need(block, 5L, pairs) + worker)
    # the workers the budget allows, for each path and window
    for (metrics in list(box, pairs)) {
        g <- if (identical(metrics, box)) g_box else g_pairs
        for (window_size in c(5L, 15L)) {
            got <- size(gpu_memsize = g, window_size = window_size,
                metrics = metrics)
            expect_equal(
                got[["multicores"]],
                min(64L, floor(g / (need(block, window_size, metrics) +
                    worker)))
            )
            expect_lte(
                (need(got[["block"]], window_size, metrics) + worker) *
                    got[["multicores"]],
                g
            )
        }
    }
    # the window changes the box path only through the overlap; it changes
    # the pair path with its pairs
    box5 <- size(gpu_memsize = g_box, window_size = 5L, metrics = box)
    box15 <- size(gpu_memsize = g_box, window_size = 15L, metrics = box)
    expect_gt(box15[["multicores"]], 0.9 * box5[["multicores"]])
    pairs5 <- size(gpu_memsize = g_pairs, window_size = 5L, metrics = pairs)
    pairs15 <- size(gpu_memsize = g_pairs, window_size = 15L, metrics = pairs)
    expect_lt(pairs15[["multicores"]], pairs5[["multicores"]])
    # a smaller budget gives a smaller block, which still fits
    g_small <- worker + 3 * need(block, 5L, pairs)
    g_large <- worker + 30 * need(block, 5L, pairs)
    small <- size(gpu_memsize = g_small, window_size = 5L, metrics = pairs,
        multicores = 1L)
    large <- size(gpu_memsize = g_large, window_size = 5L, metrics = pairs,
        multicores = 1L)
    expect_lt(
        prod(small[["block"]][c("nrows", "ncols")]),
        prod(large[["block"]][c("nrows", "ncols")])
    )
    expect_lte(need(small[["block"]], 5L, pairs) + worker, g_small)
    expect_lte(need(large[["block"]], 5L, pairs) + worker, g_large)
    # RAM limits when memsize is the smaller budget; the call takes what it
    # adds to the main process and its merges before the workers: server
    # series of 2026-11-24, 53 bytes per pixel; worker 1.26 GB at most in
    # runs of 7 tiles less its block; main 0.88 and merge 0.45 GB at most,
    # 2 merges at once
    expect_equal(ram_worker, 1.3)
    expect_equal(ram_bloat, 3.5)
    gpu_key <- function(k) .conf(c("texture_options", "memory", "gpu", k))
    expect_equal(gpu_key("ram_main_memsize"), 0.9)
    expect_equal(gpu_key("ram_merge_memsize"), 0.5)
    ram_call <- 0.9 + 2 * 0.5
    m <- ram_call + 10.5 * (ram_need(block, 5L) + ram_worker)
    ram <- size(gpu_memsize = 1000, window_size = 5L, metrics = box,
        memsize = m)
    expect_equal(ram[["multicores"]], 10L)
    expect_lte(
        (ram_need(ram[["block"]], 5L) + ram_worker) * ram[["multicores"]] +
            ram_call,
        m + 1e-9
    )
    # one block that does not fit the GPU names the GPU budget
    expect_error(
        size(gpu_memsize = 0.5 * need(block, 15L, pairs), window_size = 15L,
            metrics = pairs),
        "gpu_memsize"
    )
    # a block that fits only without the CUDA context does not fit either
    expect_error(
        size(gpu_memsize = need(block, 15L, pairs) + 0.5 * worker,
            window_size = 15L, metrics = pairs),
        "gpu_memsize"
    )
})

test_that(".conf_backend reads the key of a backend or its default", {
    texture_drop_backend("cpu")
    mem <- c("texture_options", "memory")
    sits_env[["config"]][[c(mem, "cpu")]] <- list(ram_bloat = 7)
    expect_equal(.conf_backend(mem, "cpu", "ram_bloat", 1), 7)
    # a missing key or backend gives the default
    expect_equal(
        .conf_backend(mem, "cpu", "ram_worker_memsize", 0), 0
    )
    expect_equal(.conf_backend(mem, "tpu", "ram_bloat", 3), 3)
    # a string default is a value, not the name of a key
    expect_equal(
        .conf_backend(mem, "tpu", "device", "colors"),
        "colors"
    )
})

test_that("the backends fall back to the general bloat of sits", {
    pixels <- (512 + 2 * 3)^2
    # CPU without its options: processing_bloat_cpu and no fixed memory
    texture_drop_backend("cpu")
    need <- pixels * 2 * 8 * .conf("processing_bloat_cpu") * 1e-9
    m <- 10.5 * need
    got <- texture_size(gpu = FALSE, memsize = m)
    expect_equal(got[["multicores"]], floor(m / need))
    # GPU without its options: processing_bloat_gpu for every base and no
    # fixed memory, in RAM and on the GPU
    texture_drop_backend("gpu")
    bloat <- .conf("processing_bloat_gpu")
    box <- pixels * 8 * bloat * 1e-9
    got <- texture_size(gpu = TRUE, gpu_memsize = 10.5 * box)
    expect_equal(got[["multicores"]], 10L)
    pairs <- pixels * 2 * 7 * 6 * 8 * bloat * 1e-9
    got <- texture_size(gpu = TRUE, metrics = "glcm_asm",
        gpu_memsize = 10.5 * pairs)
    expect_equal(got[["multicores"]], 10L)
    ram <- pixels * 2 * 8 * bloat * 1e-9
    got <- texture_size(gpu = TRUE, memsize = 10.5 * ram)
    expect_equal(got[["multicores"]], 10L)
})

test_that("the CPU backend sizes by its measured keys", {
    # server series of 2026-11-24 on one tile and on 7 tiles: bloat 3 of the
    # fit 2.9; worker 0.31, the 0.316 of a 7-tile run less its block; the call
    # adds 1.05 GB to the main process and 0.5 GB to each merge, 2 merges at
    # once
    key <- function(k) .conf(c("texture_options", "memory", "cpu", k))
    expect_equal(key("ram_bloat"), 3)
    expect_equal(key("ram_worker_memsize"), 0.31)
    expect_equal(key("ram_main_memsize"), 1.05)
    expect_equal(key("ram_merge_memsize"), 0.5)
    expect_equal(.conf(c("texture_options", "memory", "merges")), 2)
    # a block of 518 x 518 with its overlap: 268324 x 2 x 8 x 3 = 0.01288 GB;
    # one worker 0.32288 GB; the call 1.05 + 2 x 0.5 = 2.05 GB
    # memsize 4: floor(1.95 / 0.32288) = 6 workers; 0.09 / 6 = 0.015 GB per
    # worker, 1 base block, so the base block
    got <- texture_size(gpu = FALSE, memsize = 4, multicores = 64L)
    expect_equal(got[["multicores"]], 6L)
    expect_equal(unname(got[["block"]][c("nrows", "ncols")]), c(512L, 512L))
    # memsize 16, multicores 8: 8 workers; 11.47 / 8 = 1.434 GB per worker,
    # 111 base blocks, 6 lines of 16: 3 segments of 6 lines
    got <- texture_size(gpu = FALSE, memsize = 16, multicores = 8L)
    expect_equal(got[["multicores"]], 8L)
    expect_equal(unname(got[["block"]][c("nrows", "ncols")]), c(3072L, 8192L))
    # one worker asked has one merge at a time: 1.05 + 0.5 = 1.55 GB, and
    # 1.55 + 0.32288 fits in memsize 2
    got <- texture_size(gpu = FALSE, memsize = 2, multicores = 1L)
    expect_equal(got[["multicores"]], 1L)
    # memsize 1 does not hold the call of 1.55 GB: the error names memsize
    # and the GB of the call
    expect_error(texture_size(gpu = FALSE, memsize = 1, multicores = 1L),
        "1.55")
    # with no merges key the call counts no merge: 1.05 + 0.32288 fits in
    # memsize 1.4 with 2 workers asked
    opts <- sits_env[["config"]][["texture_options"]]
    on.exit(sits_env[["config"]][["texture_options"]] <- opts, add = TRUE)
    sits_env[["config"]][["texture_options"]][["memory"]][["merges"]] <- NULL
    got <- texture_size(gpu = FALSE, memsize = 1.4, multicores = 2L)
    expect_equal(got[["multicores"]], 1L)
})

test_that("sits_texture holds the call and some workers by default", {
    # 8 GB: the C++ call of 2.05 GB and 18 workers, the torch call of 1.9 GB
    # and 4 workers of 1.3 GB
    expect_equal(formals(sits_texture.raster_cube)[["memsize"]], 8L)
    # 5.95 / 0.32288 = 18 workers; 6.1 / (1.3 + 268324 x 2 x 8 x 3.5e-9) = 4
    expect_equal(texture_size(gpu = FALSE, memsize = 8)[["multicores"]], 18L)
    expect_equal(texture_size(gpu = TRUE, memsize = 8)[["multicores"]], 4L)
})

test_that("sits_texture checks gpu_memsize", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-gpu-memsize"), mustWork = FALSE)
    dir.create(out_dir)
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

test_that("sits_texture torch and C++ give close values on real data", {
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
    out_cpp   <- normalizePath(tempfile("texcpp"),   mustWork = FALSE)
    out_torch <- normalizePath(tempfile("textorch"), mustWork = FALSE)
    dir.create(out_cpp)
    dir.create(out_torch)
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

    out_dir <- normalizePath(tempfile("texbands"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

    run <- function(texture_cube) {
        testthat::with_mocked_bindings(
            texture_cube,
            .texture_use_gpu = function() TRUE
        )
    }

    expect_true("NDVICONTRAST" %in% sits_bands(run(sits_texture(
        feature, NDVICONTRAST = glcm_contrast(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVIDISSIM" %in% sits_bands(run(sits_texture(
        feature, NDVIDISSIM = glcm_dissimilarity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVIHOMOG" %in% sits_bands(run(sits_texture(
        feature, NDVIHOMOG = glcm_homogeneity(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVIENERGY" %in% sits_bands(run(sits_texture(
        feature, NDVIENERGY = glcm_energy(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVIASM" %in% sits_bands(run(sits_texture(
        feature, NDVIASM = glcm_asm(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVIMEAN" %in% sits_bands(run(sits_texture(
        feature, NDVIMEAN = glcm_mean(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVIVAR" %in% sits_bands(run(sits_texture(
        feature, NDVIVAR = glcm_variance(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVISTD" %in% sits_bands(run(sits_texture(
        feature, NDVISTD = glcm_std(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
    ))))
    expect_true("NDVICORRELATION" %in% sits_bands(run(sits_texture(
        feature, NDVICORRELATION = glcm_correlation(NDVI),
        window_size = 5L, multicores = 1L, output_dir = out_dir,
        progress = FALSE
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

test_that("Texture - metrics are written as FLT4S and read back as computed", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-flt4s"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    # the C++ kernel, whatever the machine, so the oracle can be exact
    testthat::local_mocked_bindings(.texture_use_gpu = function() FALSE)
    # the oracle: grey levels of the physical value of the expression between
    # min_value -1 and max_value 1 of sits_texture(), with the C++ kernel on
    # the whole tile as one block
    ndvi <- as.vector(.tile_read_block(
        tile = feature, band = "NDVI", block = NULL
    ))
    ndvi[is.na(ndvi)] <- 0
    nrows <- .tile_nrows(feature)
    ncols <- .tile_ncols(feature)
    cases <- list(
        CORRELATION = list(m = "correlation", x = ndvi),
        CONTRAST = list(m = "contrast", x = ndvi),
        # the expression is evaluated on physical values before the binning
        HALFMEAN = list(m = "mean", x = ndvi * 0.5)
    )
    got_all <- list()
    for (band in names(cases)) {
        m <- cases[[band]][["m"]]
        arg <- if (band == "HALFMEAN") quote(NDVI * 0.5) else quote(NDVI)
        expr <- list(call(paste0("glcm_", m), arg))
        names(expr) <- band
        tex <- do.call(sits_texture, c(list(cube = feature), expr, list(
            window_size = 5L, block_size = c(nrows = nrows, ncols = ncols),
            multicores = 1L, output_dir = out_dir, progress = FALSE
        )))
        fi <- .fi(.tile(tex))
        path <- fi[["path"]][fi[["band"]] == band]
        expect_equal(terra::datatype(terra::rast(path)), "FLT4S")
        got <- as.vector(.tile_read_block(
            tile = .tile(tex), band = band, block = NULL
        ))
        # the top level falls in the level below, as before this change
        grey <- pmin((pmin(pmax(cases[[band]][["x"]], -1), 1) + 1) / 2 * 1000,
            999)
        want <- as.vector(get(paste0("C_glcm_", m), asNamespace("sits"))(
            x = grey, angles = 0, nrows = nrows, ncols = ncols,
            n_grey = 1000L, window_size = 5L
        ))
        expect_false(anyNA(got))
        # the file holds float32
        expect_lt(max(abs(want - got) / (1 + abs(want))), 1e-6, label = band)
        got_all[[band]] <- got
    }
    # more than the 0 and 1 of an integer
    expect_gt(length(unique(round(got_all[["CORRELATION"]], 3L))), 2L)
})

test_that("Texture - values are binned between min_value and max_value", {
    got <- .texture_grey_levels(
        values = c(-1, 1, 0, 2, -3, NA), min_value = -1, max_value = 1,
        n_grey = 1000L
    )
    # the top level falls in the level below; values outside are cut
    expect_equal(got, c(0, 999, 500, 999, 0, NA))
})

test_that("Texture - min_value must be below max_value", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    expect_error(
        sits_texture(
            feature, NDVIMEAN = glcm_mean(NDVI), min_value = 1,
            max_value = 1, output_dir = tempdir(), progress = FALSE
        ),
        "max_value"
    )
})

test_that("Texture - an output band named as a band of the collection", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    expect_error(
        sits_texture(
            feature, EVI = glcm_mean(NDVI), window_size = 3L,
            output_dir = tempdir(), progress = FALSE
        ),
        "EVI"
    )
})

test_that("Texture - a new cube of the output folder reads the metric back", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-reread"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    tex <- sits_texture(
        feature, CONTRAST = glcm_contrast(NDVI), window_size = 5L,
        multicores = 1L, output_dir = out_dir, progress = FALSE
    )
    # a later session finds the band by the type of its file
    again <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = out_dir,
        bands = "CONTRAST",
        progress = FALSE
    )
    first <- .tile_read_block(tile = .tile(tex), band = "CONTRAST",
        block = NULL)
    second <- .tile_read_block(tile = .tile(again), band = "CONTRAST",
        block = NULL)
    expect_equal(as.vector(second), as.vector(first))
    expect_gt(max(second, na.rm = TRUE), 1)
})

test_that("Texture - a file of the old INT2S output is computed again", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-old-int2s"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    run <- function() {
        sits_texture(
            feature, CONTRAST = glcm_contrast(NDVI), window_size = 5L,
            multicores = 1L, output_dir = out_dir, progress = FALSE
        )
    }
    tex <- run()
    fi <- .fi(.tile(tex))
    path <- fi[["path"]][fi[["band"]] == "CONTRAST"]
    want <- terra::values(terra::rast(path), mat = FALSE)
    # the file a run of the code before FLT4S left
    old <- terra::rast(path) * 1
    terra::writeRaster(old, paste0(path, ".old.tif"), datatype = "INT2S")
    file.rename(paste0(path, ".old.tif"), path)
    expect_equal(terra::datatype(terra::rast(path)), "INT2S")
    run()
    expect_equal(terra::datatype(terra::rast(path)), "FLT4S")
    expect_equal(terra::values(terra::rast(path), mat = FALSE), want)
})

test_that("Texture - a texture band of an embeddings cube keeps its type", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    feature <- sits_select(cube, bands = "NDVI", dates = "2013-09-14")
    out_dir <- normalizePath(tempfile("texture-emb"), mustWork = FALSE)
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    tex <- sits_texture(
        feature, CONTRAST = glcm_contrast(NDVI), window_size = 3L,
        multicores = 1L, output_dir = out_dir, progress = FALSE
    )
    tile <- .tile(tex)
    class(tile) <- c("embeddings_cube", class(tile))
    # an embedding band keeps the embedding settings, any other band those
    # of the type of its file
    expect_equal(.scale(.tile_band_conf(tile = tile, band = "EMB01")), 0.001)
    expect_equal(.scale(.tile_band_conf(tile = tile, band = "CONTRAST")), 1)
})
