context("CSV")

test_that("Data to CSV",{
    expect_true(sits_data_to_csv(cerrado_2classes,
                                 file = "./cerrado_2classes.csv"))
    expect_true(file.remove("./cerrado_2classes.csv"))
})

test_that("Data to CSV - error",{
    data(cerrado_2classes)
    expect_error(sits_data_to_csv(cerrado_2classes,
                    file = "/non-existent-directory/cerrado_2classes.csv"),
                 "sits_data_to_csv - file is not writable")
})

test_that("Metadata to CSV",{
    data(cerrado_2classes)
    sits_metadata_to_csv(cerrado_2classes, file = "cerrado_2classes.csv")

    expect_true(file.remove("cerrado_2classes.csv"))
})

test_that("Metadata to CSV - error",{
    data(cerrado_2classes)
    expect_error(sits_metadata_to_csv(cerrado_2classes,
                                      file = "/non-existent-directory/cerrado_2classes.csv"),
                 "sits_metadata_to_csv - file is not writable")
})

test_that("Reading a CSV file from WTSS", {
    #skip_on_cran()
    csv_file <- system.file("extdata/samples/samples_matogrosso.csv",
                            package = "sits")
    cube_wtss <- sits_cube(type = "WTSS",
                           URL = "http://www.esensing.dpi.inpe.br/wtss/",
                           name = "MOD13Q1")

    points.tb <- sits_get_data(cube = cube_wtss,
                               file = csv_file,
                               bands = c("ndvi"))

    expect_true(all(unique(points.tb$label) %in% c("Pasture", "Cerrado")))
})

test_that("Error Reading a CSV file from WTSS", {
    #skip_on_cran()
    test_csv <- cerrado_2classes[1:10,]
    # put a wrong latitude value
    test_csv[1:4,]$latitude <- 14.0

    sits_metadata_to_csv(test_csv, file = "test.csv")

    csv_file <- c("test.csv")

    cube_wtss <- sits_cube(type = "WTSS",
                           URL = "http://www.esensing.dpi.inpe.br/wtss/",
                           name = "MOD13Q1")

    points.tb <- sits_get_data(cube = cube_wtss,
                               file = csv_file,
                               bands = c("ndvi"))

    expect_true(nrow(points.tb) == 6)

    expect_true(file.remove("test.csv"))
})
