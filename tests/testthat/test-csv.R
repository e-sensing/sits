context("CSV")

test_that("Data to CSV",{
    csv_file <- paste0(tempdir(),"/cerrado_2classes.csv")
    expect_true(sits_data_to_csv(cerrado_2classes,
                                 file = csv_file))
    expect_true(file.remove(csv_file))
})

test_that("Data to CSV - error",{
    data(cerrado_2classes)
    expect_error(sits_data_to_csv(cerrado_2classes,
                    file = "/non-existent-directory/cerrado_2classes.csv"),
                 "sits_data_to_csv - file is not writable")
})

test_that("Metadata to CSV",{
    data(cerrado_2classes)
    csv_file <- paste0(tempdir(),"/cerrado_2classes.csv")
    sits_metadata_to_csv(cerrado_2classes, csv_file)

    expect_true(file.remove(csv_file))
})

test_that("Metadata to CSV - error",{
    data(cerrado_2classes)
    expect_error(sits_metadata_to_csv(cerrado_2classes,
                                      file = "/non-existent-directory/cerrado_2classes.csv"),
                 "sits_metadata_to_csv - file is not writable")
})

test_that("Reading a CSV file from WTSS", {
    skip_on_cran()
    csv_file <- system.file("extdata/samples/samples_matogrosso.csv",
                            package = "sits")
    cube_wtss <- sits_cube(type = "WTSS",
                           URL = "http://www.esensing.dpi.inpe.br/wtss/",
                           name = "MOD13Q1")

    if(purrr::is_null(cube_wtss))
        skip("WTSS is not accessible")

    points.tb <- sits_get_data(cube = cube_wtss,
                               file = csv_file,
                               bands = c("ndvi"))

    expect_true(all(unique(points.tb$label) %in% c("Pasture", "Cerrado")))

})
