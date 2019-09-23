context("CSV")

test_that("Data to CSV",{
    expect_true(sits_data_to_csv(cerrado_2classes, file = "./cerrado_2classes.csv"))
})

test_that("Metadata to shp",{
    data(cerrado_2classes)
    sits_metadata_to_csv(cerrado_2classes, file = "cerrado_2classes.csv")

    expect_true(file.remove("cerrado_2classes.csv"))
})
