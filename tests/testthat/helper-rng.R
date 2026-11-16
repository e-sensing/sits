rng_get_state <- function() get(".Random.seed", globalenv(), inherits = FALSE)
# sets the Mersenne-Twister generator to `seed` and returns its state
rng_seed_mt <- function(seed = 1L) {
    set.seed(seed, kind = "Mersenne-Twister")
    rng_get_state()
}
# runs fn() when the test that called the helper ends
rng_defer <- function(fn, env) {
    do.call(on.exit, list(as.call(list(fn)), add = TRUE), envir = env)
}
# a new directory, removed when the calling test ends
rng_local_dir <- function(env = parent.frame()) {
    dir <- tempfile("rng")
    dir.create(dir)
    rng_defer(function() unlink(dir, recursive = TRUE), env)
    dir
}
# writes a size x size raster in blocks of 256 as band `band` of a local
# cube, removed when the calling test ends; `seed` draws the values
rng_raster_cube <- function(band, size, nlyr = 1L, vals = NULL, seed = NULL,
                            labels = NULL, type = "INT2U",
                            env = parent.frame()) {
    data_dir <- rng_local_dir(env)
    r <- terra::rast(nrows = size, ncols = size, nlyrs = nlyr, xmin = 0,
                     xmax = size * 30, ymin = 0, ymax = size * 30,
                     crs = "EPSG:32723")
    if (is.null(vals)) {
        set.seed(seed)
        vals <- sample.int(10000L, terra::ncell(r) * nlyr, TRUE)
    }
    terra::values(r) <- vals
    file <- paste0("LANDSAT_OLI_000000_2020-01-01_2020-12-31_", band, "_v1")
    gdal <- c("TILED=YES", "BLOCKXSIZE=256", "BLOCKYSIZE=256")
    terra::writeRaster(r, file.path(data_dir, paste0(file, ".tif")),
                       datatype = type, overwrite = TRUE, gdal = gdal)
    if (!is.null(labels)) names(labels) <- seq_along(labels)
    sits_cube(source = "BDC", collection = "LANDSAT-OLI-16D",
              data_dir = data_dir, bands = band, version = "v1",
              labels = labels, progress = FALSE)
}
# run(...) gives for each argument set the result of the first one
rng_expect_same <- function(run, ...) {
    args <- list(...)
    a <- do.call(run, as.list(args[[1L]]))
    purrr::walk(args[-1L], function(x) {
        expect_identical(do.call(run, as.list(x)), a)
    })
    invisible(a)
}
# run(seed) repeats under another global state, follows the seed, and
# leaves the global generator as received, kind included
rng_expect_seed <- function(run) {
    before <- rng_seed_mt()
    a <- run(10L)
    expect_identical(rng_get_state(), before)
    set.seed(2L)
    expect_identical(run(10L), a)
    expect_false(identical(run(11L), a))
    invisible(a)
}
