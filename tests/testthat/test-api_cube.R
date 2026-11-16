test_that(".cube_jobs_create binds the jobs of each tile under its name", {
    cube <- sits_cube(
        source = "BDC", collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    cube <- cube[c(1L, 1L, 1L), ]
    cube[["tile"]] <- c("t1", "t2", "t3")
    # tile tk has k jobs; a column named tile must not hide the tile
    fn <- function(tile, extra) {
        k <- match(.tile_name(tile), c("t1", "t2", "t3")) + extra
        tibble::tibble(part = seq_len(k), tile = list(tile))
    }
    jobs <- .cube_jobs_create(cube, fn, extra = 0L)
    expect_equal(jobs[["group"]], c("t1", "t2", "t2", "t3", "t3", "t3"))
    expect_equal(jobs[["part"]], c(1L, 1L, 2L, 1L, 2L, 3L))
})
