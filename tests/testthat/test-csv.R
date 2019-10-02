context("CSV")

test_that("Data to CSV",{
    expect_true(sits_data_to_csv(cerrado_2classes,
                                 file = "./cerrado_2classes.csv"))
    expect_true(file.remove("./cerrado_2classes.csv"))
})

test_that("Metadata to shp",{
    data(cerrado_2classes)
    sits_metadata_to_csv(cerrado_2classes, file = "cerrado_2classes.csv")

    expect_true(file.remove("cerrado_2classes.csv"))
})

test_that("Reading a CSV file from WTSS", {
    #skip_on_cran()
    csv_file <- system.file("extdata/samples/samples_matogrosso.csv",
                            package = "sits")
    cube_wtss <- sits_cube(service = "WTSS", name = "MOD13Q1")

    points.tb <- sits:::.sits_from_csv(csv_file = csv_file,
                                       cube = cube_wtss,
                                       bands = c("ndvi"),
                                       .prefilter   = "1",
                                       .n_start_csv = 1,
                                       .n_max_csv   = 3,
                                       .n_save      = 0)

    expect_true(all(unique(points.tb$label) == c("Pasture")))
})

test_that("Error Reading a CSV file from WTSS", {
    #skip_on_cran()
    test_csv <- cerrado_2classes[1:10,]
    # put a wrong latitude value
    test_csv[1:4,]$latitude <- 14.0

    sits_metadata_to_csv(test_csv, file = "test.csv")

    csv_file <- c("test.csv")

    cube_wtss <- sits_cube(service = "WTSS", name = "MOD13Q1")

    points.tb <- sits:::.sits_from_csv(csv_file = csv_file,
                                       cube = cube_wtss,
                                       bands = c("ndvi"),
                                       .prefilter   = "1",
                                       .n_start_csv = 1,
                                       .n_max_csv   = 10,
                                       .n_save      = 0)

    expect_true(nrow(points.tb) == 6)

    err <- sits_log_show_errors()
    err_wtss <- dplyr::filter(err, grepl("WTSS", error))
    expect_true(nrow(err_wtss) > 0)
    expect_true(file.remove("test.csv"))
})
