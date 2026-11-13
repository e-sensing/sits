test_that("chunks have the overlap on both sides of each block", {
    block <- c(ncols = 512L, nrows = 512L)
    image_size <- c(ncols = 1536L, nrows = 1200L)
    image_bbox <- list(
        xmin = 0, xmax = 46080, ymin = 0, ymax = 36000, crs = "EPSG:32722"
    )
    chunks <- .chunks_create(block, 4L, image_size, image_bbox)
    first_row <- chunks[chunks[["row"]] == 1L, ]
    expect_equal(first_row[["col"]], c(1L, 509L, 1021L))
    expect_equal(first_row[["ncols"]], c(516L, 520L, 516L))
    first_col <- chunks[chunks[["col"]] == 1L, ]
    expect_equal(first_col[["row"]], c(1L, 509L, 1021L))
    expect_equal(first_col[["nrows"]], c(516L, 520L, 180L))
    # the cropped blocks start after the left and top overlap
    cropped <- .chunks_no_overlap(first_row)
    expect_equal(cropped[["col"]], c(1L, 5L, 5L))
    expect_equal(cropped[["ncols"]], c(512L, 512L, 512L))
    cropped <- .chunks_no_overlap(first_col)
    expect_equal(cropped[["row"]], c(1L, 5L, 5L))
    expect_equal(cropped[["nrows"]], c(512L, 512L, 176L))
})
test_that("cropped chunks partition the image and keep their neighbors", {
    overlap <- 4L
    image_size <- c(ncols = 1300L, nrows = 700L)
    image_bbox <- list(
        xmin = 0, xmax = 39000, ymin = 0, ymax = 21000, crs = "EPSG:32722"
    )
    chunks <- .chunks_create(
        c(ncols = 256L, nrows = 128L), overlap, image_size, image_bbox
    )
    cropped <- .chunks_no_overlap(chunks)
    covered <- matrix(0L, nrow = 700L, ncol = 1300L)
    for (i in seq_len(nrow(chunks))) {
        first_col <- chunks[["col"]][[i]] + cropped[["col"]][[i]] - 1L
        last_col <- first_col + cropped[["ncols"]][[i]] - 1L
        first_row <- chunks[["row"]][[i]] + cropped[["row"]][[i]] - 1L
        last_row <- first_row + cropped[["nrows"]][[i]] - 1L
        covered[first_row:last_row, first_col:last_col] <-
            covered[first_row:last_row, first_col:last_col] + 1L
        chunk_last_col <- chunks[["col"]][[i]] + chunks[["ncols"]][[i]] - 1L
        chunk_last_row <- chunks[["row"]][[i]] + chunks[["nrows"]][[i]] - 1L
        # a window of radius overlap stays in the chunk or meets the image edge
        expect_gte(first_col - chunks[["col"]][[i]], min(overlap, first_col - 1L))
        expect_gte(first_row - chunks[["row"]][[i]], min(overlap, first_row - 1L))
        expect_gte(chunk_last_col - last_col, min(overlap, 1300L - last_col))
        expect_gte(chunk_last_row - last_row, min(overlap, 700L - last_row))
    }
    expect_true(all(covered == 1L))
})
test_that("chunks without overlap keep the size of the block", {
    chunks <- .chunks_create(
        c(ncols = 512L, nrows = 512L), 0L,
        c(ncols = 1024L, nrows = 512L),
        list(xmin = 0, xmax = 30720, ymin = 0, ymax = 15360, crs = "EPSG:32722")
    )
    expect_equal(chunks[["col"]], c(1L, 513L))
    expect_equal(chunks[["ncols"]], c(512L, 512L))
})
test_that(".chunks_jobs_create names the block file of each chunk", {
    block <- c(ncols = 512L, nrows = 512L)
    image_size <- c(ncols = 1536L, nrows = 1200L)
    image_bbox <- list(
        xmin = 0, xmax = 46080, ymin = 0, ymax = 36000, crs = "EPSG:32722"
    )
    chunks <- .chunks_create(block, 4L, image_size, image_bbox)
    out_dir <- withr::local_tempdir()
    out_file <- file.path(out_dir, "T1_probs_v1.tif")
    jobs <- .chunks_jobs_create(chunks, out_file, out_dir)
    expect_equal(nrow(jobs), nrow(chunks))
    expect_equal(jobs[["col"]], chunks[["col"]])
    expect_true(all(jobs[["role"]] == "block"))
    expect_true(all(jobs[["out_file"]] == out_file))
    # the names come from the first pixel of each chunk, with its overlap
    first_row <- jobs[jobs[["row"]] == 1L, ]
    expect_equal(
        basename(first_row[["block_file"]]),
        c(
            "T1_probs_v1_block_1_1.tif", "T1_probs_v1_block_1_509.tif",
            "T1_probs_v1_block_1_1021.tif"
        )
    )
    expect_equal(
        unique(dirname(jobs[["block_file"]])), file.path(out_dir, ".sits")
    )
})
