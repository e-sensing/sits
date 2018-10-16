context("Labels")
test_that("Labels", {
    #skip_on_cran()

    labels <- sits_labels(samples_MT_9classes)

    expect_equal(sum(labels$count), 2115)
    expect_equal(labels$label[1], "Cerrado")
    expect_equal(sum(labels$freq), 1) # update to prop
})
