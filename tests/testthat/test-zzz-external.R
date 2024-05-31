test_that("FilePath", {
    file <- tools::file_path_sans_ext(
        system.file("extdata/shapefiles/cerrado/cerrado_forested.shp",
            package = "sits"
        )
    )
    expect_false("shp" %in% file)
})

test_that("Unique", {
    labels <- unique(cerrado_2classes[["label"]])
    expect_true("Cerrado" %in% labels)
})
