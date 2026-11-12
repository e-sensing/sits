test_that(".parallel_queue sends ready jobs, first ones before the others", {
    # job 3 waits for job 1 and goes before the other ready jobs; job 2 is
    # slow, so job 1 ends first and its worker takes job 3
    jobs <- list(
        list(id = 1L, wait = 0), list(id = 2L, wait = 1),
        list(id = 3L, wait = 0), list(id = 4L, wait = 0),
        list(id = 5L, wait = 0)
    )
    after <- list(integer(), integer(), 1L, integer(), integer())
    first <- c(FALSE, FALSE, TRUE, FALSE, FALSE)
    fn <- function(job) {
        Sys.sleep(job[["wait"]])
        list(id = job[["id"]], pid = Sys.getpid())
    }
    run <- function() {
        sent <- integer()
        done <- integer()
        values <- .parallel_queue(
            jobs = jobs, fn = fn, after = after, first = first,
            on_send = function(k) sent <<- c(sent, k),
            on_done = function(k, value) done <<- c(done, k)
        )
        list(values = values, sent = sent, done = done)
    }
    # a cluster left open by another test would be reused
    .parallel_stop(TRUE)
    # without a cluster: this process runs every job, in the order of sending
    seq_run <- run()
    expect_equal(seq_run[["sent"]], c(1L, 2L, 3L, 4L, 5L))
    expect_equal(purrr::map_int(seq_run[["values"]], "id"), 1:5)
    expect_true(all(purrr::map_int(seq_run[["values"]], "pid") == Sys.getpid()))
    # with two workers
    started <- .parallel_start(workers = 2L)
    on.exit(.parallel_stop(started), add = TRUE)
    expect_length(sits_env[["cluster"]], 2L)
    par_run <- run()
    expect_equal(par_run[["sent"]], c(1L, 2L, 3L, 4L, 5L))
    expect_equal(sort(par_run[["done"]]), 1:5)
    # the values in the order of the jobs, each from a worker
    expect_equal(purrr::map_int(par_run[["values"]], "id"), 1:5)
    pids <- purrr::map_int(par_run[["values"]], "pid")
    expect_false(any(pids == Sys.getpid()))
    # a job that fails stops the call with its error
    expect_error(
        .parallel_queue(
            jobs = list(1L, 2L),
            fn = function(job) if (job == 2L) stop("job two fails") else job
        ),
        "job two fails"
    )
})
