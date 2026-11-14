test_that("Parallel - dead worker replaced and job resent", {
    # ml_model is exported once at cluster start. Oracle: 42 + 1:12.
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    flag <- tempfile()
    ml_model <- 42
    # job 5 kills its worker once; each job reads ml_model
    fn <- function(i) {
        if (i == 5 && !file.exists(flag)) {
            file.create(flag)
            tools::pskill(Sys.getpid(), tools::SIGKILL)
        }
        Sys.sleep(0.2)
        ml_model + i
    }
    # fn finds ml_model and flag only in the worker globals, as sits does
    environment(fn) <- globalenv()
    started <- .parallel_start(workers = 3, export_vars = c("ml_model", "flag"))
    on.exit(
        .parallel_stop(started, cleanup_vars = c("ml_model", "flag")),
        add = TRUE
    )
    values <- suppressMessages(.parallel_map(1:12, fn))
    expect_true(file.exists(flag))
    expect_equal(unlist(values), 42 + 1:12)
})

# Kills one worker in .parallel_map(); returns the PIDs before and after
.test_kill_one_worker <- function() {
    flag <- tempfile()
    on.exit(unlink(flag), add = TRUE)
    # job 1 kills its worker once; flag comes as argument
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
# Reads on a worker each item set by .parallel_setup_nodes()
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

test_that("Parallel - new worker with the cluster setup", {
    # Oracle: values set here and 1 torch thread, read on each worker
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
    # one worker replaced; every job returned its value
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
    # the new worker equals those made at start
    for (setup in setups) {
        expect_identical(setup, new_setup)
    }
})

test_that("Parallel - new worker with start env vars and lib paths", {
    # Oracle: env var and lib path set before start, changed before the kill
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
    # the session changes after start
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

test_that("Parallel - idle worker dies", {
    # Oracle: each map returns the values set here
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    started <- .parallel_start(workers = 3)
    on.exit(.parallel_stop(started), add = TRUE)
    before <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    # worker 3 dies before the map, with no job
    tools::pskill(before[[3L]], tools::SIGKILL)
    for (k in seq_len(50L)) {
        if (!tools::pskill(before[[3L]], 0L)) break
        Sys.sleep(0.1)
    }
    # jobs wait, so the end-of-file of worker 3 comes first
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

test_that("Parallel - worker dies after its last result", {
    # Oracle: each map returns the values set here
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    flag <- tempfile()
    on.exit(unlink(flag), add = TRUE)
    started <- .parallel_start(workers = 3)
    on.exit(.parallel_stop(started), add = TRUE, after = FALSE)
    before <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    # the worker of job 1 dies 0.5 s after it returns, while jobs 2 and 3 run
    fn <- function(i, flag) {
        if (i == 1 && !file.exists(flag)) {
            file.create(flag)
            # "&&", so the "&" of system() covers the whole list
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

test_that("Parallel - resent job keeps its substream", {
    # Oracle: the same seeded map with no dead worker
    skip_on_cran()
    skip_on_os("windows")
    sits_parallel(workers = 0)
    old_kind <- RNGkind()
    on.exit(RNGkind(old_kind[[1L]], old_kind[[2L]], old_kind[[3L]]),
        add = TRUE
    )
    started <- .parallel_start(workers = 3)
    on.exit(.parallel_stop(started), add = TRUE, after = FALSE)
    # job 3 kills its worker once, when flag is absent
    fn <- function(i, flag) {
        if (i == 3 && !file.exists(flag)) {
            file.create(flag)
            tools::pskill(Sys.getpid(), tools::SIGKILL)
        }
        Sys.sleep(0.1)
        c(i, stats::runif(3))
    }
    environment(fn) <- globalenv()
    # flag exists: no worker dies
    no_kill <- tempfile()
    file.create(no_kill)
    kill <- tempfile()
    on.exit(unlink(c(no_kill, kill)), add = TRUE)
    set.seed(123, kind = "Mersenne-Twister")
    expected <- .parallel_map(1:6, fn, flag = no_kill)
    before <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    set.seed(123, kind = "Mersenne-Twister")
    values <- suppressMessages(.parallel_map(1:6, fn, flag = kill))
    after <- unlist(parallel::clusterCall(sits_env[["cluster"]], Sys.getpid))
    expect_true(file.exists(kill))
    expect_equal(sum(before != after), 1L)
    expect_identical(values, expected)
})
