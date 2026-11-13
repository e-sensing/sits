test_that(".jobs_map_queue closes each group after its parts", {
    dir <- withr::local_tempdir()
    # group a: two parts and the last; b: only the last; c: one part
    jobs <- tibble::tibble(
        group = c("a", "a", "a", "b", "c", "c"),
        id = 1:6,
        wait = c(0.5, 0, 0, 0, 0, 0)
    )
    # the last job of a group counts the files of the parts done before it
    fn <- function(job, dir) {
        Sys.sleep(job[["wait"]])
        group <- job[["group"]]
        file.create(file.path(dir, paste0(group, "_", job[["id"]])))
        n <- length(list.files(dir, pattern = paste0("^", group, "_")))
        list(group = group, n = n, pid = Sys.getpid())
    }
    run <- function() {
        unlink(list.files(dir, full.names = TRUE))
        hooks <- list(
            start = character(), last = character(), end = character()
        )
        values <- .jobs_map_queue(
            jobs = jobs, fn = fn, dir = dir,
            on_start = function(job) {
                hooks[["start"]] <<- c(hooks[["start"]], job[["group"]])
            },
            on_last = function(job) {
                hooks[["last"]] <<- c(hooks[["last"]], job[["group"]])
            },
            on_end = function(job, value) {
                hooks[["end"]] <<- c(hooks[["end"]], value[["group"]])
            }
        )
        list(values = values, hooks = hooks)
    }
    check <- function(res) {
        # one value per group, from its last job, after all its parts
        expect_equal(purrr::map_chr(res[["values"]], "group"), c("a", "b", "c"))
        expect_equal(purrr::map_int(res[["values"]], "n"), c(3L, 1L, 2L))
        expect_equal(sort(res[["hooks"]][["start"]]), c("a", "b", "c"))
        expect_equal(sort(res[["hooks"]][["last"]]), c("a", "b", "c"))
        expect_equal(sort(res[["hooks"]][["end"]]), c("a", "b", "c"))
    }
    # a cluster left open by another test would be reused
    .parallel_stop(TRUE)
    seq_run <- run()
    check(seq_run)
    seq_pids <- purrr::map_int(seq_run[["values"]], "pid")
    expect_true(all(seq_pids == Sys.getpid()))
    started <- .parallel_start(workers = 2L)
    on.exit(.parallel_stop(started), add = TRUE)
    par_run <- run()
    check(par_run)
    par_pids <- purrr::map_int(par_run[["values"]], "pid")
    expect_false(any(par_pids == Sys.getpid()))
    # the jobs of a group must be contiguous
    expect_error(
        .jobs_map_queue(
            jobs = tibble::tibble(group = c("a", "b", "a")), fn = fn, dir = dir
        )
    )
})
