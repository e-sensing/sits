test_that("sits_parallel lifecycle and checks", {
    # make sure we start clean
    sits_parallel(workers = 0)

    # ---- Getter ----
    expect_null(sits_parallel())

    # ---- Parameter checks ----
    expect_error(sits_parallel(workers = -1))
    expect_error(sits_parallel(workers = "2"))
    expect_error(sits_parallel(workers = 2, log = "yes"))

    # output_dir is only required when log = TRUE
    expect_error(sits_parallel(workers = 2, log = TRUE))
    expect_error(sits_parallel(workers = 2, log = TRUE, output_dir = ""))

    # ---- Start cluster ----
    # Keep workers small for CRAN / CI stability
    sits_parallel(workers = 2)
    cl <- sits_parallel()
    expect_true(inherits(cl, "cluster"))
    expect_true(length(cl) >= 1)

    # ---- Restart cluster (strong check using worker PIDs) ----
    old_pids <- parallel::clusterCall(cl, Sys.getpid)

    # Restart cluster
    sits_parallel(workers = 2)
    cl2 <- sits_parallel()
    expect_true(inherits(cl2, "cluster"))
    expect_true(length(cl2) >= 1)

    new_pids <- parallel::clusterCall(cl2, Sys.getpid)

    # A restart must recreate workers, so PIDs should change.
    expect_false(identical(old_pids, new_pids))

    # ---- Stop cluster: workers = 0 ----
    sits_parallel(workers = 0)
    expect_null(sits_parallel())

    # ---- Stop cluster: workers = 1 ----
    sits_parallel(workers = 2)
    expect_true(inherits(sits_parallel(), "cluster"))
    sits_parallel(workers = 1)
    expect_null(sits_parallel())

    # ---- Cleanup ----
    sits_parallel(workers = 0)
})
