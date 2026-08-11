test_that("STAC query uses the configured source version", {
    query <- .stac_create_items_query(
        source = "CDSE",
        collection = "SENTINEL-2-L2A",
        start_date = "2023-05-01",
        end_date = "2023-09-01",
        limit = 1L
    )

    expect_s3_class(query, "rstac_query")
    expect_equal(query$version, "1.0.0")
})

