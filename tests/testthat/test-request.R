test_that("Request to MPC using httr2", {
    # get the URL
    url <- .source_url(source = "MPC")
    resp <- .try(
        {
            .get_request(url)
        },
        .default = NULL
    )
    testthat::expect_s3_class(
        resp, "httr2_response"
    )
    testthat::expect_equal(
        .response_is_error(resp), FALSE
    )
    testthat::expect_equal(
        .response_status(resp), 200
    )
    testthat::expect_s3_class(
        .response_check_status(resp), "httr2_response"
    )
    testthat::expect_equal(
        class(.response_content(resp)), "list"
    )
    testthat::expect_equal(
        .response_content_type(resp), "application/json"
    )
})

test_that("Request with RETRY to MPC using httr2", {
    # get the URL
    url <- .source_url(source = "MPC")
    resp <- .try(
        {
            .retry_request(url)
        },
        .default = NULL
    )
    testthat::expect_s3_class(
        resp, "httr2_response"
    )
    testthat::expect_equal(
        .response_is_error(resp), FALSE
    )
    testthat::expect_equal(
        .response_status(resp), 200
    )
    testthat::expect_s3_class(
        .response_check_status(resp), "httr2_response"
    )

    testthat::expect_error(
        .retry_request(paste0(url, "/error"))
    )
})

test_that("URL utilities", {
    # get the URL
    url <- .source_url(source = "MPC")
    parsed_url <- .url_parse(url)

    testthat::expect_s3_class(
        parsed_url, "httr2_url"
    )

    parsed_url$query <- list(a = 1, b = 2)
    url_new <- .url_build(parsed_url)
    testthat::expect_equal(
        class(url_new), "character"
    )
    testthat::expect_equal(
        grepl("?a=1&b=2", url_new), TRUE
    )
})
