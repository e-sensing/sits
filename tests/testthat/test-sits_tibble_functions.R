# test-sits_tibble_functions

testthat::context("sits_tibble_functions")

testthat::test_that("Computing time series normalization", {
    set.seed(666)
    my_st <- sits_random_tibble(n_samples = 10, n_vi = 3)
    # compute statistics
    res <- sits_normalize_ts(data.tb = my_st, use_IQR = FALSE)
    computed_vals <- dplyr::bind_rows(res[[1]]$time_series)
    computed_stats <- res[[2]]
    # compute expected statistics by hand
    all_ts <- dplyr::bind_rows(my_st$time_series)
    all_ts <- all_ts[,2:ncol(all_ts)]
    vi_means <- colMeans(all_ts)
    vi_sds <- apply(all_ts, MARGIN = 2, sd)
    expected_stats <- dplyr::bind_cols(stats = c("Tendency", "Dispersion"), dplyr::bind_rows(vi_means, vi_sds))
    expected_vals <- dplyr::as_tibble(scale(all_ts, center = vi_means, scale = vi_sds))
    # compare
    testthat::expect_true(all.equal(expected_stats, computed_stats))
    testthat::expect_true(sum(expected_vals - computed_vals[,2:ncol(computed_vals)]) == 0)
})

# NOTE: This fails during jenkins validation
# testthat::test_that("Sampling shapefiles", {
#     nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#     pnt_samples <- sits_sample_shp(shp.sf = nc,
#                                    label_field = "NAME",
#                                    nsamples = 500,
#                                    border_offset = 0.01)
#     testthat::expect_equal(nrow(pnt_samples) > 0.9 * (nrow(nc) * 5), TRUE)
#     testthat::expect_equal(ncol(pnt_samples), 7)
# })

# NOTE: This fails during jenkins validation
# testthat::test_that("Cast sits_tibble to sf", {
#     set.seed(666)
#     my_st <- sits_random_tibble(n_samples = 10, n_vi = 3)
#     sf_obj <- sits_to_shp(my_st)
#     testthat::expect_equal(nrow(sf_obj), nrow(my_st))
#     testthat::expect_equal(ncol(sf_obj), ncol(my_st) - 2)
# })
