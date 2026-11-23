# The NDVI of the first dates of the mod13q1 cube of the package: one
# tile of 147 x 255 pixels, one feature per date
texture_cube <- function(dates = 1L) {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    cube <- sits_cube(
        source     = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir   = data_dir,
        progress   = FALSE
    )
    sits_select(
        cube, bands = "NDVI", dates = sits_timeline(cube)[seq_len(dates)]
    )
}
# A factory of GLCM functions that also leaves one file per process that
# called it, named by its pid; it runs in workers, so it carries its own
# environment instead of the test frame
texture_marker <- function(real, dir) {
    env <- new.env(parent = globalenv())
    env[["real"]] <- real
    env[["dir"]] <- dir
    fn <- function(...) {
        file.create(file.path(dir, as.character(Sys.getpid())))
        real(...)
    }
    environment(fn) <- env
    fn
}
# The pids that called a marker
texture_marker_pids <- function(dir) {
    as.integer(list.files(dir))
}
# Blocks of 32 x 32 pixels whatever the memory, so a tile has 40 blocks
texture_small_blocks <- function(env = parent.frame()) {
    testthat::local_mocked_bindings(
        .texture_blocksize = function(cube) c(nrows = 32L, ncols = 32L),
        .jobs_optimal_block = function(job_block_memsize, block, ...) block,
        .env = env
    )
}
# A new empty folder under the session temporary folder
texture_dir <- function(name) {
    dir <- normalizePath(file.path(tempdir(), name), mustWork = FALSE)
    unlink(dir, recursive = TRUE)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir
}
# Every debug event of every process that wrote to the log of out_dir
texture_events <- function(out_dir) {
    files <- list.files(
        file.path(out_dir, ".sits"), pattern = "[.]log$", full.names = TRUE
    )
    do.call(rbind, lapply(files, utils::read.csv, strip.white = TRUE))
}
# Peak RSS above the base of one torch call, in a new R process: an earlier
# call in the same process leaves freed memory that the allocator reuses
texture_peak_worker <- function(what, side, window_size, metric, n_grey) {
    status <- function(field) {
        line <- grep(paste0("^", field, ":"), readLines("/proc/self/status"),
            value = TRUE)
        as.numeric(gsub("[^0-9]", "", line)) * 1024
    }
    glcm <- get(".texture_torch_glcm_compute", asNamespace("sits"))
    # loads torch and starts its threads before the base is read
    invisible(glcm(m = rep(0L, 64L), metric = "asm", window_size = 3L,
        angles = 0, img_nrow = 8L, img_ncol = 8L, n_grey = 64L))
    set.seed(43L)
    img <- as.integer(sample(0L:(n_grey - 1L), side * side, replace = TRUE))
    invisible(gc())
    base <- status("VmRSS")
    writeLines("5", "/proc/self/clear_refs")
    if (what == "known") {
        # ones, not zeros, so every page is written
        x <- torch::torch_ones(side * side, dtype = torch::torch_float64())
        rm(x)
    } else {
        invisible(glcm(m = img, metric = metric, window_size = window_size,
            angles = c(0, pi / 4, pi / 2, 3 * pi / 4), img_nrow = side,
            img_ncol = side, n_grey = n_grey))
    }
    status("VmHWM") - base
}
environment(texture_peak_worker) <- globalenv()
# one sits worker per measure, set up by sits as in a real call
texture_fresh_peak <- function(what, ...) {
    started <- .parallel_start(workers = 1L, force = TRUE)
    on.exit(.parallel_stop(started), add = TRUE)
    if (!started) {
        stop("a sits cluster is open, so the worker would not be new")
    }
    .parallel_map(list(what), texture_peak_worker, ...)[[1L]]
}
# the peak RSS of a process can be reset only on Linux
texture_skip_no_peak <- function() {
    testthat::skip_on_os(c("windows", "mac", "solaris"))
    ok <- tryCatch({
        writeLines("5", "/proc/self/clear_refs")
        TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) testthat::skip("the peak RSS of the process cannot be reset here")
}
