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

test_that("a dead worker is replaced with the cluster setup and its job is sent again", {
    # sits_classify() exports ml_model once with .parallel_start(); a worker
    # that dies is replaced by .parallel_reset_node(). Oracle: 42 + 1:12.
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    flag <- tempfile()
    ml_model <- 42
    # job 5 kills its own worker once; every job reads the exported ml_model
    fn <- function(i) {
        if (i == 5 && !file.exists(flag)) {
            file.create(flag)
            tools::pskill(Sys.getpid(), tools::SIGKILL)
        }
        Sys.sleep(0.2)
        ml_model + i
    }
    # in the global environment, fn finds ml_model and flag only where they
    # were exported, as the block functions of sits do
    environment(fn) <- globalenv()
    started <- .parallel_start(workers = 3, export_vars = c("ml_model", "flag"))
    on.exit(.parallel_stop(started, cleanup_vars = c("ml_model", "flag")), add = TRUE)
    values <- suppressMessages(.parallel_map(1:12, fn))
    expect_true(file.exists(flag))
    expect_equal(unlist(values), 42 + 1:12)
})
