context("zzz")

test_that("onLoad", {
    sits:::.onLoad()
    expect(TRUE, "onLoad failed to load")
})
