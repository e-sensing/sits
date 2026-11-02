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

# Kills one worker of the open sits cluster through .parallel_map() and
# returns the PIDs of the workers before and after its replacement.
.test_kill_one_worker <- function() {
    flag <- tempfile()
    on.exit(unlink(flag), add = TRUE)
    # job 1 kills its own worker once; flag reaches the worker as argument
    fn <- function(i, flag) {
        if (i == 1 && !file.exists(flag)) {
            file.create(flag)
            tools::pskill(Sys.getpid(), tools::SIGKILL)
        }
        Sys.sleep(0.1)
        i
    }
    environment(fn) <- globalenv()
    before <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    values <- suppressMessages(.parallel_map(1:4, fn, flag = flag))
    after <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    list(before = before, after = after, values = unlist(values))
}
# Reads, on a worker, every item of the setup of .parallel_setup_nodes().
.test_worker_setup <- function(env_var, torch_check) {
    list(
        ml_model = get0("ml_model", envir = globalenv(), inherits = FALSE),
        env_var = Sys.getenv(env_var, unset = NA_character_),
        lib_paths = .libPaths(),
        torch_threads = if (torch_check) torch::torch_get_num_threads(),
        debug_flag = get("sits_env", envir = asNamespace("sits"))[[
            "debug_flag"
        ]]
    )
}
environment(.test_worker_setup) <- globalenv()

test_that("a recreated worker gets the whole setup of the other workers", {
    # Oracle: values set by this test (exported value, env var, lib path,
    # debug flag) and 1 torch thread, read back on each worker.
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    env_var <- "SITS_TEST_NODE_RESET"
    old_libs <- .libPaths()
    old_debug <- .debug()
    test_lib <- tempfile("lib")
    dir.create(test_lib)
    on.exit(
        {
            Sys.unsetenv(env_var)
            .libPaths(old_libs)
            .debug(flag = old_debug)
            unlink(test_lib, recursive = TRUE)
        },
        add = TRUE
    )
    Sys.setenv(SITS_TEST_NODE_RESET = "start-value")
    .libPaths(c(test_lib, old_libs))
    expected_libs <- .libPaths()
    log_dir <- tempfile("log")
    dir.create(log_dir)
    on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)
    ml_model <- 42
    started <- .parallel_start(
        workers = 2, export_vars = "ml_model",
        log = TRUE, output_dir = log_dir
    )
    on.exit(.parallel_stop(started, cleanup_vars = "ml_model"),
        add = TRUE, after = FALSE
    )
    killed <- .test_kill_one_worker()
    # exactly one worker was replaced, and every job returned its value
    expect_equal(killed[["values"]], 1:4)
    expect_equal(sum(killed[["before"]] != killed[["after"]]), 1L)
    torch_check <- .torch_is_functional()
    setups <- parallel::clusterCall(
        sits_env[["cluster"]], .test_worker_setup,
        env_var = env_var, torch_check = torch_check
    )
    new_node <- which(killed[["before"]] != killed[["after"]])
    new_setup <- setups[[new_node]]
    expect_equal(new_setup[["ml_model"]], 42)
    expect_equal(new_setup[["env_var"]], "start-value")
    expect_equal(new_setup[["lib_paths"]][[1L]], normalizePath(test_lib))
    expect_equal(new_setup[["lib_paths"]], expected_libs)
    expect_true(new_setup[["debug_flag"]])
    if (torch_check) {
        expect_equal(new_setup[["torch_threads"]], 1L)
    }
    # the recreated worker equals the workers made at cluster start
    for (setup in setups) {
        expect_identical(setup, new_setup)
    }
})

test_that("a recreated worker gets the env vars and lib paths of cluster start", {
    # Oracle: the env var value and lib path set by this test before
    # .parallel_start(); both change before a worker is killed.
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    env_var <- "SITS_TEST_NODE_RESET"
    old_libs <- .libPaths()
    test_lib <- tempfile("lib")
    dir.create(test_lib)
    on.exit(
        {
            Sys.unsetenv(env_var)
            .libPaths(old_libs)
            unlink(test_lib, recursive = TRUE)
        },
        add = TRUE
    )
    Sys.setenv(SITS_TEST_NODE_RESET = "start-value")
    .libPaths(c(test_lib, old_libs))
    expected_libs <- .libPaths()
    started <- .parallel_start(workers = 2)
    on.exit(.parallel_stop(started), add = TRUE, after = FALSE)
    # the main session changes after cluster start
    Sys.setenv(SITS_TEST_NODE_RESET = "changed-value")
    .libPaths(old_libs)
    killed <- .test_kill_one_worker()
    expect_equal(sum(killed[["before"]] != killed[["after"]]), 1L)
    setups <- parallel::clusterCall(
        sits_env[["cluster"]], .test_worker_setup,
        env_var = env_var, torch_check = FALSE
    )
    new_setup <- setups[[which(killed[["before"]] != killed[["after"]])]]
    expect_equal(new_setup[["env_var"]], "start-value")
    expect_equal(new_setup[["lib_paths"]], expected_libs)
})

test_that("a worker that dies while idle loses no result of the next maps", {
    # Oracle: values set by this test; each map returns its own values.
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    started <- .parallel_start(workers = 3)
    on.exit(.parallel_stop(started), add = TRUE)
    before <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    # worker 3 dies before the map, so it runs no job when it dies
    tools::pskill(before[[3L]], tools::SIGKILL)
    for (k in seq_len(50L)) {
        if (!tools::pskill(before[[3L]], 0L)) break
        Sys.sleep(0.1)
    }
    # jobs wait, so the end-of-file of worker 3 arrives before any result
    fn <- function(i) {
        Sys.sleep(0.2)
        i
    }
    environment(fn) <- globalenv()
    first <- suppressMessages(.parallel_map(c(10, 20), fn))
    second <- suppressMessages(.parallel_map(c(101, 102, 103), fn))
    after <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    expect_equal(sum(before != after), 1L)
    expect_identical(first, list(10, 20))
    expect_identical(second, list(101, 102, 103))
})

test_that("a worker that dies after its last result loses no result", {
    # Oracle: values set by this test; each map returns its own values.
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    flag <- tempfile()
    on.exit(unlink(flag), add = TRUE)
    started <- .parallel_start(workers = 3)
    on.exit(.parallel_stop(started), add = TRUE, after = FALSE)
    before <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    # job 1 returns at once and its worker dies 0.5 s later, while jobs 2
    # and 3 still run, so the dead worker has no job left in this map
    fn <- function(i, flag) {
        if (i == 1 && !file.exists(flag)) {
            file.create(flag)
            # system() appends "&"; "&&" sends the whole list to the background
            cmd <- sprintf("sleep 0.5 && kill -9 %d", Sys.getpid())
            system(cmd, wait = FALSE)
            return(i)
        }
        if (i < 100) Sys.sleep(1)
        i
    }
    environment(fn) <- globalenv()
    first <- suppressMessages(.parallel_map(c(1, 2, 3), fn, flag = flag))
    second <- suppressMessages(.parallel_map(c(101, 102, 103), fn, flag = flag))
    after <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    expect_true(file.exists(flag))
    expect_identical(first, list(1, 2, 3))
    expect_identical(second, list(101, 102, 103))
    expect_equal(sum(before != after), 1L)
})
