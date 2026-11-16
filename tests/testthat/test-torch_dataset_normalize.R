# The block of .torch_chunks_dataset is normalized by
# .pred_features_normalize(), the function of the training. The dataset
# reads a known matrix from a stand-in read_fn; the expected values come
# from the formula in plain R, not from the package code.
normalize_fixture <- function() {
    # 3 pixels, 2 bands by 3 dates, band-major as the predictors
    values <- matrix(
        c(0.01, 0.348, 0.1201, 0.3107, 0.2446, 0.2017, 0.2778, 0.0671, 0.2499,
          0.4682, 0.5254, 0.7557, 0.2429, 0.8126, 0.7843, 0.2052, 0.9, 0.4114),
        nrow = 3L
    )
    list(
        values = values,
        stats = list(
            q02 = rep(c(0.0123, 0.1457), each = 3L),
            q98 = rep(c(0.3789, 0.8211), each = 3L)
        )
    )
}

normalize_item <- function(values, stats) {
    testthat::local_mocked_bindings(
        .tile_timeline = function(tile) {
            as.Date(c("2020-01-01", "2020-01-17", "2020-02-02"))
        }
    )
    dataset <- .torch_chunks_dataset(
        chunks = tibble::tibble(
            col = 1L, row = 1L, ncols = 3L, nrows = 1L,
            xmin = 0, xmax = 3, ymin = 0, ymax = 1
        ),
        tile = tibble::tibble(tile = "A"),
        read_fn = function(...) values,
        bands = c("B1", "B2"),
        stats = stats,
        ml_temporal_model = TRUE
    )
    list(dataset = dataset, item = dataset$.getitem(1L))
}

f32 <- function(x) torch::torch_tensor(x, dtype = torch::torch_float())

test_that("Torch dataset - the block is normalized as the training", {
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    fix <- normalize_fixture()
    q02 <- fix[["stats"]][["q02"]]
    q98 <- fix[["stats"]][["q98"]]
    # the formula of the training, in double precision
    expected <- sweep(sweep(fix[["values"]], 2L, q02), 2L, 1 / (q98 - q02), `*`)
    expected[expected < 0.0001] <- 0.0001
    expected[expected > 1] <- 1
    # the fixture tells the double formula from a float32 division
    in_float <- torch::torch_clamp(
        (f32(fix[["values"]]) - q02) / (q98 - q02), min = 0.0001, max = 1.0
    )
    expect_gt(as.integer(torch::torch_sum(in_float != f32(expected))), 0L)
    # below q02 and above q98
    expect_equal(expected[1L, 1L], 0.0001)
    expect_equal(expected[2L, 6L], 1)
    got <- normalize_item(fix[["values"]], fix[["stats"]])
    # the layout of a temporal model: batch, dates, bands
    expect_true(torch::torch_equal(
        got[["item"]][["input"]],
        f32(array(expected, dim = c(3L, 3L, 2L)))
    ))
    # the dataset keeps no normalization of its own
    expect_null(got[["dataset"]][[".normalize_data"]])
    expect_null(got[["dataset"]][["min"]])
    expect_null(got[["dataset"]][["range"]])
})

test_that("Torch dataset - no statistics, no normalization", {
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    fix <- normalize_fixture()
    got <- normalize_item(fix[["values"]], NULL)
    expect_true(torch::torch_equal(
        got[["item"]][["input"]],
        f32(array(fix[["values"]], dim = c(3L, 3L, 2L)))
    ))
})

test_that("Torch dataset - statistics of another size are refused", {
    skip_if_not_installed("torch")
    skip_if_not(torch::torch_is_installed())
    fix <- normalize_fixture()
    # statistics of 2 bands by 2 dates for a block of 2 bands by 3 dates
    short <- lapply(fix[["stats"]], function(x) x[c(1L, 2L, 4L, 5L)])
    expect_error(normalize_item(fix[["values"]], short))
})
