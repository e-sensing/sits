#test-sits_tibble_functions
testthat::context("sits_tibble_functions")

testthat::test_that("Computing time series normalization", {
    set.seed(666)
    my_st <- sits_random_tibble(n_samples = 10, n_vi = 3)
    # compute statistics
    res <- sits_normalize_ts(data.tb = my_st)
    computed_vals <- dplyr::bind_rows(res[[1]]$time_series)
    computed_stats <- res[[2]]
    # compute expected statistics by hand
    all_ts <- dplyr::bind_rows(my_st$time_series)
    all_ts <- all_ts[,2:ncol(all_ts)]
    vi_means <- colMeans(all_ts)
    vi_sds <- apply(all_ts, MARGIN = 2, sd)
    expected_stats <- dplyr::bind_cols(stats = c("mean", "sd"), dplyr::bind_rows(vi_means, vi_sds))
    expected_vals <- dplyr::as_tibble(scale(all_ts, center = vi_means, scale = vi_sds))
    # compare
    testthat::expect_true(all.equal(expected_stats, computed_stats))
    testthat::expect_true(sum(expected_vals - computed_vals[,2:ncol(computed_vals)]) == 0)
})

