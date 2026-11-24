block <- c(nrows = 512L, ncols = 512L)
image <- c(nrows = 8192L, ncols = 8192L)

test_that(".jobs_size with one budget gives the workers and block of memory", {
    # GB of one block and budgets, chosen so that each case limits differently
    b <- 0.05
    # blocks by hand, 16 base blocks across the image: memsize 1 gives 20
    # workers of 0.05 GB, one base block each; memsize 4 gives 8 of 0.5 GB,
    # 10 base blocks, 2 segments of 8 across; memsize 100 gives 8 of 12.5 GB,
    # 250 base blocks, 15 lines, 2 segments of 8 lines
    cases <- data.frame(memsize = c(1, 4, 100), multicores = c(64L, 8L, 8L))
    cases$want <- list(c(512L, 512L), c(512L, 4096L), c(4096L, 8192L))
    for (i in seq_len(nrow(cases))) {
        m <- cases$memsize[[i]]
        mc <- cases$multicores[[i]]
        got <- .jobs_size(
            budgets = list(.jobs_budget("memsize", m, b)),
            block = block, image_size = image, multicores = mc
        )
        workers <- min(mc, floor(m / b))
        expect_equal(got[["multicores"]], workers)
        expect_equal(
            unname(got[["block"]][c("nrows", "ncols")]),
            cases$want[[i]]
        )
    }
})

test_that(".jobs_size counts the fixed memory of each worker", {
    b <- 0.05
    w <- 0.2
    m <- 4
    got <- .jobs_size(
        budgets = list(.jobs_budget("memsize", m, b, worker_memsize = w)),
        block = block, image_size = image, multicores = 64L
    )
    # 4 / 0.25 = 16 workers
    expect_equal(got[["multicores"]], 16L)
    pixels <- got[["block"]][["nrows"]] * got[["block"]][["ncols"]]
    per_block <- b / prod(block)
    # the block may fill its share exactly
    expect_lte(pixels * per_block, (m - 16 * w) / 16 + 1e-9)
})

test_that(".jobs_size takes the tighter budget and names what cut", {
    b <- 0.05
    ram <- .jobs_budget("memsize", 1, b)
    gpu <- .jobs_budget("gpu_memsize", 0.5, b, worker_memsize = 0.05)
    # memsize allows 20, gpu_memsize 0.5 / 0.1 = 5; only gpu_memsize is
    # below a request of 10
    got <- .jobs_size(list(ram, gpu), block, image, multicores = 10L)
    expect_equal(got[["multicores"]], 5L)
    expect_equal(got[["limit"]], "gpu_memsize")
    # by hand: memsize gives 1 / 5 = 0.2 GB per worker, 4 base blocks, a
    # block of 512 x 2048; gpu_memsize gives (0.5 - 5 x 0.05) / 5 = 0.05 GB,
    # one base block; the smaller wins
    expect_equal(unname(got[["block"]][c("nrows", "ncols")]), c(512L, 512L))
    # both cut below the request
    expect_equal(
        .jobs_size(list(.jobs_budget("memsize", 0.3, b), gpu), block, image,
            multicores = 64L)[["limit"]],
        "memsize and gpu_memsize"
    )
    # none cuts when the request is small
    expect_null(.jobs_size(list(ram, gpu), block, image,
        multicores = 2L)[["limit"]])
})

test_that(".jobs_size names the budget that one block does not fit", {
    b <- 0.05
    expect_error(
        .jobs_size(list(.jobs_budget("memsize", 1, b),
            .jobs_budget("gpu_memsize", 0.06, b, worker_memsize = 0.02)),
            block, image, multicores = 2L),
        "gpu_memsize"
    )
    expect_error(
        .jobs_size(list(.jobs_budget("memsize", 0.04, b)), block, image,
            multicores = 2L),
        "memsize"
    )
})

test_that(".jobs_request_workers follows the open cluster", {
    old <- sits_env[["cluster"]]
    on.exit(sits_env[["cluster"]] <- old, add = TRUE)
    sits_env[["cluster"]] <- NULL
    expect_equal(.jobs_request_workers(NULL), 2L)
    # a stand-in cluster of 3 nodes: its length, and open by the mock
    sits_env[["cluster"]] <- vector("list", 3L)
    open <- TRUE
    testthat::local_mocked_bindings(.parallel_is_open = function() open)
    expect_equal(.jobs_request_workers(NULL), 3L)
    expect_warning(
        n <- .jobs_request_workers(5L),
        "3 of the 5"
    )
    expect_equal(n, 3L)
    expect_no_warning(n <- .jobs_request_workers(1L))
    expect_equal(n, 1L)
    # a cluster whose sockets died gives the default request, and does not
    # cut a request above its size
    open <- FALSE
    expect_equal(.jobs_request_workers(NULL), 2L)
    expect_no_warning(n <- .jobs_request_workers(5L))
    expect_equal(n, 5L)
})

test_that(".jobs_size takes the fixed memory of the call before the workers", {
    b <- 0.05
    w <- 0.2
    got <- .jobs_size(
        budgets = list(.jobs_budget("memsize", 4, b, worker_memsize = w,
            call_memsize = 1)),
        block = block, image_size = image, multicores = 64L
    )
    # (4 - 1) / 0.25 = 12 workers
    expect_equal(got[["multicores"]], 12L)
    pixels <- got[["block"]][["nrows"]] * got[["block"]][["ncols"]]
    expect_lte(pixels * b / prod(block), (4 - 1 - 12 * w) / 12 + 1e-9)
    # a call that leaves no room for one worker names its budget
    expect_error(
        .jobs_size(list(.jobs_budget("memsize", 4, b, worker_memsize = w,
            call_memsize = 3.8)), block, image, multicores = 2L),
        "memsize"
    )
})
