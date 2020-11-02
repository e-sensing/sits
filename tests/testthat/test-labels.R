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
    data("samples_mt_4bands")

    conv.lst = list(Soy_Corn = "Cropland",
                    Soy_Cotton  = "Cropland",
                    Soy_Fallow  = "Cropland",
                    Soy_Millet  = "Cropland",
                    Soy_Sunflower  = "Cropland",
                    Fallow_Cotton  = "Cropland")

    new_data  <- sits_relabel(samples_mt_4bands, conv.lst)

    labels <- sits_labels(new_data)

    expect_true("Cropland" %in% sits_labels(new_data)$label)
    expect_equal(length(labels$label), 4)
    expect_equal(labels$label[1], "Cerrado")
    expect_equal(sum(labels$prop), 1)
})
