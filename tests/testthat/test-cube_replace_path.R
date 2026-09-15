test_that("sits_cube_replace_path rewrites paths and keeps a valid cube", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- .try(
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6.1",
            data_dir = data_dir,
            progress = FALSE
        ),
        default = NULL
    )
    testthat::skip_if(purrr::is_null(cube), "BDC is not accessible")

    new_cube <- sits_cube_replace_path(
        cube        = cube,
        pattern     = data_dir,
        replacement = "/new/root"
    )

    # cube classes and structure are preserved
    expect_identical(class(new_cube), class(cube))
    expect_equal(nrow(new_cube), nrow(cube))

    # every path had its root rewritten
    old_paths <- .fi(cube)[["path"]]
    new_paths <- .fi(new_cube)[["path"]]
    expect_equal(
        new_paths,
        gsub(data_dir, "/new/root", old_paths, fixed = TRUE)
    )
    expect_true(all(startsWith(new_paths, "/new/root")))

    # regex mode also works (replace the file extension, whatever it is)
    regex_cube <- sits_cube_replace_path(
        cube        = cube,
        pattern     = "\\.[^.]+$",
        replacement = ".tiff",
        fixed       = FALSE
    )
    expect_true(all(endsWith(.fi(regex_cube)[["path"]], ".tiff")))
})

test_that("sits_cube_replace_path validates its inputs", {
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    cube <- .try(
        sits_cube(
            source = "BDC",
            collection = "MOD13Q1-6.1",
            data_dir = data_dir,
            progress = FALSE
        ),
        default = NULL
    )
    testthat::skip_if(purrr::is_null(cube), "BDC is not accessible")

    # not a raster cube
    expect_error(sits_cube_replace_path(list(1), "a", "b"))
    # pattern must be a single string
    expect_error(sits_cube_replace_path(cube, c("a", "b"), "b"))
    # replacement must be a single string
    expect_error(sits_cube_replace_path(cube, "a", c("b", "c")))
    # fixed must be logical
    expect_error(sits_cube_replace_path(cube, "a", "b", fixed = "yes"))
    # column must exist in file_info
    expect_error(sits_cube_replace_path(cube, "a", "b", column = "nope"))
})
