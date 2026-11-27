# writes a size x size raster of `band` as a local cube in tempdir
.helper_rng_raster_cube <- function(band, size, nlyr = 1L, vals = NULL,
    seed = NULL,
                            labels = NULL, type = "INT2U") {
    data_dir <- tempfile("rng")
    dir.create(data_dir)
    r <- .raster_new_rast(
        nrows = size, ncols = size, xmin = 0, xmax = size * 30, ymin = 0,
        ymax = size * 30, nlayers = nlyr, crs = "EPSG:32723"
    )
    if (is.null(vals)) {
        set.seed(seed)
        vals <- sample.int(10000L, size * size * nlyr, TRUE)
    }
    r <- .raster_set_values(r, matrix(vals, nrow = size * size, ncol = nlyr))
    file <- paste0("LANDSAT_OLI_000000_2020-01-01_2020-12-31_", band, "_v1")
    .raster_write_rast(
        r, file.path(data_dir, paste0(file, ".tif")), data_type = type,
        overwrite = TRUE
    )
    if (!is.null(labels)) names(labels) <- seq_along(labels)
    sits_cube(source = "BDC", collection = "LANDSAT-OLI-16D",
              data_dir = data_dir, bands = band, version = "v1",
              labels = labels, progress = FALSE)
}
