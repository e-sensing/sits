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

# Write one file of a fake cgroup tree, creating its directories
write_cgroup_file <- function(path, text) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, path)
}

# Point the cgroup keys of the config to a fake tree
local_cgroup <- function(root, proc, env = parent.frame()) {
    conf <- .conf
    local_mocked_bindings(.conf = function(...) {
        key <- c(...)
        if (identical(key, "cgroup_root")) return(root)
        if (identical(key, "cgroup_proc_file")) return(proc)
        conf(...)
    }, .env = env)
}

# Fix the cores so expected values do not depend on this machine
local_cores <- function(n, env = parent.frame()) {
    local_mocked_bindings(
        detectCores = function(...) n, .package = "parallel", .env = env
    )
}

test_that(".parallel_cores reads the cgroup v2 limit of a container", {
    local_cores(8L)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    write_cgroup_file(proc, "0::/")
    write_cgroup_file(file.path(root, "cpu.max"), "200000 100000")
    expect_identical(.parallel_cores(), 2L)
})

test_that(".parallel_cores takes the lowest v2 limit of the parents", {
    local_cores(8L)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    write_cgroup_file(proc, "0::/a/b")
    write_cgroup_file(file.path(root, "a", "cpu.max"), "300000 100000")
    write_cgroup_file(file.path(root, "a", "b", "cpu.max"), "max 100000")
    # the limit of the parent applies when the process level has none
    expect_identical(.parallel_cores(), 3L)
    # the process level is lower, so the nearest level must not stop it
    write_cgroup_file(file.path(root, "a", "b", "cpu.max"), "100000 100000")
    expect_identical(.parallel_cores(), 1L)
    # a lower parent wins over the nearest level
    write_cgroup_file(file.path(root, "a", "cpu.max"), "100000 100000")
    write_cgroup_file(file.path(root, "a", "b", "cpu.max"), "400000 100000")
    expect_identical(.parallel_cores(), 1L)
})

test_that(".parallel_cores reads the cgroup v1 cpu controller", {
    local_cores(8L)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    # cpuset and memory lines point elsewhere; only the cpu line counts
    write_cgroup_file(proc, c(
        "5:cpuset:/other", "4:cpu,cpuacct:/a", "3:memory:/other"
    ))
    v1_dir <- file.path(root, "cpu", "a")
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_quota_us"), "250000")
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_period_us"), "100000")
    expect_identical(.parallel_cores(), 2L)
    # -1 means no limit at a level
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_quota_us"), "-1")
    write_cgroup_file(file.path(root, "cpu", "cpu.cfs_quota_us"), "-1")
    write_cgroup_file(file.path(root, "cpu", "cpu.cfs_period_us"), "100000")
    expect_identical(.parallel_cores(), 8L)
    # the kernel may list the controllers in either order
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_quota_us"), "300000")
    write_cgroup_file(proc, "4:cpuacct,cpu:/a")
    expect_identical(.parallel_cores(), 3L)
    # cpuset after the cpu line catches a substring match that keeps the last
    write_cgroup_file(proc, c("4:cpu,cpuacct:/a", "5:cpuset:/other"))
    expect_identical(.parallel_cores(), 3L)
})

test_that(".parallel_cores walks the v1 parents of the process", {
    local_cores(8L)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    write_cgroup_file(proc, "4:cpu,cpuacct:/a/b")
    v1_dir <- file.path(root, "cpu")
    write_cgroup_file(file.path(v1_dir, "a", "cpu.cfs_quota_us"), "200000")
    write_cgroup_file(file.path(v1_dir, "a", "cpu.cfs_period_us"), "100000")
    # no files at the process level, so only the parent limit can apply
    expect_identical(.parallel_cores(), 2L)
    # -1 at the process level is no limit, so the parent still applies
    b_dir <- file.path(v1_dir, "a", "b")
    write_cgroup_file(file.path(b_dir, "cpu.cfs_quota_us"), "-1")
    write_cgroup_file(file.path(b_dir, "cpu.cfs_period_us"), "100000")
    expect_identical(.parallel_cores(), 2L)
    # a limit only at the v1 root also applies
    unlink(file.path(v1_dir, "a", "cpu.cfs_quota_us"))
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_quota_us"), "300000")
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_period_us"), "100000")
    expect_identical(.parallel_cores(), 3L)
})

test_that(".parallel_cores takes the lowest of v2 and v1 on a hybrid host", {
    local_cores(8L)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    write_cgroup_file(proc, c("4:cpu,cpuacct:/a", "0::/x"))
    v1_dir <- file.path(root, "cpu", "a")
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_quota_us"), "300000")
    write_cgroup_file(file.path(v1_dir, "cpu.cfs_period_us"), "100000")
    write_cgroup_file(file.path(root, "x", "cpu.max"), "200000 100000")
    expect_identical(.parallel_cores(), 2L)
    # the other side lower, and the other line order
    write_cgroup_file(file.path(root, "x", "cpu.max"), "500000 100000")
    write_cgroup_file(proc, c("0::/x", "4:cpu,cpuacct:/a"))
    expect_identical(.parallel_cores(), 3L)
})

test_that(".parallel_cores with config defaults gives one valid count", {
    # weak: this machine's limit is unknown, so only the range is checked
    n <- .parallel_cores()
    expect_type(n, "integer")
    expect_length(n, 1L)
    expect_gte(n, 1L)
    cores <- parallel::detectCores()
    skip_if(is.na(cores), "detectCores is NA")
    expect_lte(n, cores)
})

test_that(".parallel_cores rounds down and stays within 1 and the cores", {
    local_cores(8L)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    write_cgroup_file(proc, "0::/")
    # half a CPU of time still needs one thread
    write_cgroup_file(file.path(root, "cpu.max"), "50000 100000")
    expect_identical(.parallel_cores(), 1L)
    write_cgroup_file(file.path(root, "cpu.max"), "250000 100000")
    expect_identical(.parallel_cores(), 2L)
    # 3.5 tells floor from round, which would give 4
    write_cgroup_file(file.path(root, "cpu.max"), "350000 100000")
    expect_identical(.parallel_cores(), 3L)
    # a quota above the cores cannot give more threads than cores
    write_cgroup_file(file.path(root, "cpu.max"), "2000000 100000")
    expect_identical(.parallel_cores(), 8L)
})

test_that(".parallel_cores treats bad cgroup files as no limit", {
    local_cores(8L)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    # missing proc cgroup file
    expect_identical(.parallel_cores(), 8L)
    # missing cgroup root
    write_cgroup_file(proc, "0::/a")
    local_cgroup(file.path(root, "none"), proc)
    expect_identical(.parallel_cores(), 8L)
    local_cgroup(root, proc)
    # empty cpu.max
    write_cgroup_file(proc, "0::/")
    file.create(file.path(root, "cpu.max"))
    expect_identical(.parallel_cores(), 8L)
    # malformed cpu.max
    write_cgroup_file(file.path(root, "cpu.max"), "abc")
    expect_identical(.parallel_cores(), 8L)
    # malformed proc cgroup line
    unlink(file.path(root, "cpu.max"))
    write_cgroup_file(proc, "garbage")
    expect_identical(.parallel_cores(), 8L)
})

test_that(".parallel_cores gives 1 when detectCores is NA", {
    local_cores(NA_integer_)
    root <- tempfile("cgroup-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    proc <- file.path(root, "proc-cgroup")
    local_cgroup(root, proc)
    write_cgroup_file(proc, "0::/")
    expect_identical(.parallel_cores(), 1L)
})
