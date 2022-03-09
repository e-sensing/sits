test_that("Writing a zoo time series", {
  data(cerrado_2classes)
  zoo_lst <- sits_to_zoo(cerrado_2classes[1:5, ])

  expect_equal(length(zoo_lst), 5)
  expect_equal(dim(zoo_lst[[1]])[1], 23)
  expect_equal(dim(zoo_lst[[1]])[2], 2)
})
