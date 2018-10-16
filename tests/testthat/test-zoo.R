context("WTSS")

test_that("Info", {
    data(cerrado_2classes)
    zoo.lst <- sits_toZOO (cerrado_2classes[1:5,])

    expect_equal(length(zoo.lst), 5)
    expect_equal(dim(zoo.lst[[1]])[1], 23)
    expect_equal(dim(zoo.lst[[1]])[2], 2)
})

