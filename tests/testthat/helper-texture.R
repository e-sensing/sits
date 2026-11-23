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
