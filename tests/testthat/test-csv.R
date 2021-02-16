context("CSV")

test_that("Data to CSV", {
    csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
    expect_true(sits_data_to_csv(cerrado_2classes,
        file = csv_file
    ))
    expect_true(file.remove(csv_file))
})

test_that("Data to CSV - error", {
    data(cerrado_2classes)
    expect_error(
        sits_data_to_csv(cerrado_2classes,
            file = "/non-existent-directory/cerrado_2classes.csv"
        ),
        "sits_data_to_csv - file is not writable"
    )
})

test_that("Metadata to CSV", {
    data(cerrado_2classes)
    csv_file <- paste0(tempdir(), "/cerrado_2classes.csv")
    sits_metadata_to_csv(cerrado_2classes, csv_file)

    expect_true(file.remove(csv_file))
})

test_that("Metadata to CSV - error", {
    data(cerrado_2classes)
    expect_error(
        sits_metadata_to_csv(cerrado_2classes,
            file = "/non-existent-directory/cerrado_2classes.csv"
        ),
        "sits_metadata_to_csv - file is not writable"
    )
})


