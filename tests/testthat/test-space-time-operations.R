test_that("All", {
  reproj <- .sits_proj_from_latlong(-10, -20, 4326)

  expect_equal(as.numeric(reproj[1, 1]), -10)
  expect_equal(as.numeric(reproj[1, 2]), -20)

  reproj <- .sits_proj_to_latlong(-10, -20, 4326)

  expect_equal(as.numeric(reproj[1, 1]), -10)
  expect_equal(as.numeric(reproj[1, 2]), -20)
})
test_that("Time Series Dates", {
  times <- sits_timeline(cerrado_2classes)
  expect_true(length(times) == 23)
})
test_that("Timeline date", {
  timeline <- sits_timeline(cerrado_2classes)
  expect_true(sits:::.sits_timeline_valid_date(
    as.Date("2000-09-12"),
    timeline
  ))
})
