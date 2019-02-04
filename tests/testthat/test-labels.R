context("Labels")
test_that("Labels", {
    #skip_on_cran()

    labels <- sits_labels(samples_mt_9classes)

    expect_equal(sum(labels$count), 2115)
    expect_equal(labels$label[1], "Cerrado")
    expect_equal(sum(labels$prop), 1) # update to prop
})
