context("CSV")

test_that("Data to CSV",{
    expect_true(sits_data_to_csv(cerrado_2classes, file = "./cerrado_2classes.csv"))
})

test_that("Data to shp",{
    data("timeline_2000_2017")
    start_date <- lubridate::ymd("2002-08-29")
    end_date   <- lubridate::ymd("2013-08-13")
    shpfile <- system.file ("extdata/shapefiles/cerrado_forested.shp", package = "sits")
    csvfile <- paste0("cerrado_forested.csv")
    label <- "Cerrado_Forested"
    result <- sits_shp_to_csv(shpfile, csvfile, label, timeline_2000_2017, start_date, end_date)

    expect_true(result)
    expect_true(file.remove("cerrado_forested.csv"))
})

test_that("Metadata to shp",{
    data(cerrado_2classes)
    sits_metadata_to_csv (cerrado_2classes, file = "cerrado_2classes.csv")

    expect_true(file.remove("cerrado_2classes.csv"))
})
