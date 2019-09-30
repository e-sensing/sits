context("Labels")
test_that("Labels", {
    #skip_on_cran()

    labels <- sits_labels(samples_mt_6bands)

    expect_equal(sum(labels$count), 425)
    expect_equal(labels$label[1], "Cerrado")
    expect_equal(sum(labels$prop), 1) # update to prop
})

test_that("Relabel", {
    #skip_on_cran()
    data("prodes_226_064")

    conv.lst <-  list(Deforestation_2014 = "NonForest",
                    Deforestation_2015 = "NonForest",
                    Pasture = "NonForest")

    new_data  <- sits_relabel(prodes_226_064, conv.lst)

    expect_true("NonForest" %in% sits_labels(new_data)$label)
})
