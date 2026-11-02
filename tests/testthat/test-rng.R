# Expected values computed outside sits; the first one, 0.127011122046577,
# is the value published with L'Ecuyer's RngStreams
rng_base <- c(10407L, rep(12345L, 6L))
rng_set_state <- function(state) {
    assign(".Random.seed", state, envir = globalenv())
}
rng_get_state <- function() {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
}
test_that("substreams have the expected L'Ecuyer-CMRG states", {
    old <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
    RNGkind("L'Ecuyer-CMRG")
    rng_set_state(rng_base)
    subs <- sits:::.rng_substreams(3L)
    expect_identical(
        subs[[1L]][-1L],
        c(870504860L, -1653269569L, 884013853L,
          339352413L, -1920660590L, -643363409L)
    )
    expect_identical(
        subs[[2L]][-1L],
        c(460387934L, 1532391390L, 877287553L,
          120103512L, -2141851355L, 335837774L)
    )
    expect_identical(
        subs[[3L]][-1L],
        c(-519857236L, -1086671252L, 1257177538L,
          378684317L, -1427855118L, -2093661213L)
    )
    # the caller moves past the last substream it handed out
    expect_identical(
        rng_get_state()[-1L],
        c(1870130441L, 490396226L, 1081325149L,
          -609881575L, -1946924678L, 1094489500L)
    )
    rng_set_state(subs[[3L]])
    expect_equal(
        stats::runif(2L), c(0.5032122888761005, 0.16517391832456343),
        tolerance = 1e-15
    )
})
test_that("a stream given as seed is used as given", {
    set.seed(1L, kind = "Mersenne-Twister")
    before <- rng_get_state()
    f <- function(seed) {
        rng_exit <- sits:::.rng_enter(seed)
        on.exit(rng_exit(), add = TRUE)
        stats::runif(3L)
    }
    expect_equal(
        f(rng_base),
        c(0.12701112204657714, 0.3185275653967945, 0.3091860155832701),
        tolerance = 1e-15
    )
    # with seed, the state as received, kind included
    expect_identical(rng_get_state(), before)
    expect_identical(RNGkind()[[1L]], "Mersenne-Twister")
})
test_that("an integer seed gives the same draws and leaves the state", {
    set.seed(5L, kind = "Mersenne-Twister")
    before <- rng_get_state()
    f <- function(seed) {
        rng_exit <- sits:::.rng_enter(seed)
        on.exit(rng_exit(), add = TRUE)
        c(RNGkind()[[1L]], stats::runif(2L))
    }
    a <- f(42L)
    expect_identical(a[[1L]], "L'Ecuyer-CMRG")
    expect_identical(f(42L), a)
    expect_false(identical(f(43L), a))
    expect_identical(rng_get_state(), before)
})
test_that("without seed, a L'Ecuyer-CMRG global state goes on", {
    old <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
    RNGkind("L'Ecuyer-CMRG")
    rng_set_state(rng_base)
    f <- function() {
        rng_exit <- sits:::.rng_enter(NULL)
        on.exit(rng_exit(), add = TRUE)
        stats::runif(3L)
    }
    expect_equal(
        f(),
        c(0.12701112204657714, 0.3185275653967945, 0.3091860155832701),
        tolerance = 1e-15
    )
    # the restored state includes the draws spent
    expect_identical(
        rng_get_state()[-1L],
        c(-1271176443L, -1271176443L, -909607723L,
          -1816685032L, 1655725443L, 2057415812L)
    )
    expect_equal(stats::runif(1L), 0.8258468629271136, tolerance = 1e-15)
})
test_that("without seed, another global generator gives six draws", {
    set.seed(7L, kind = "Mersenne-Twister")
    invisible(stats::runif(6L))
    after_six <- rng_get_state()
    set.seed(7L, kind = "Mersenne-Twister")
    f <- function() {
        rng_exit <- sits:::.rng_enter(NULL)
        on.exit(rng_exit(), add = TRUE)
        c(RNGkind()[[1L]], stats::runif(2L))
    }
    a <- f()
    expect_identical(a[[1L]], "L'Ecuyer-CMRG")
    # the kind comes back, and the state includes the six draws
    expect_identical(RNGkind()[[1L]], "Mersenne-Twister")
    expect_identical(rng_get_state(), after_six)
    # two calls in a row get two streams; set.seed() repeats them
    b <- f()
    expect_false(identical(a, b))
    set.seed(7L, kind = "Mersenne-Twister")
    expect_identical(f(), a)
})
test_that("an invalid seed is refused", {
    f <- function(seed) {
        rng_exit <- sits:::.rng_enter(seed)
        on.exit(rng_exit(), add = TRUE)
        stats::runif(1L)
    }
    expect_error(f("a"))
    expect_error(f(c(1L, 2L)))
    expect_error(f(c(10403L, rep(1L, 6L))))
})
test_that("parallel tasks draw from the substream of their index", {
    # expected values computed outside sits
    expected <- list(
        c(0.07939898979733463, 0.4803395047575741),
        c(0.2619834061461847, 0.5359922918692224),
        c(0.5032122888761005, 0.16517391832456343)
    )
    old <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
    run <- function(workers) {
        sits_parallel(workers = 0L)
        if (workers > 1L) {
            sits_parallel(workers = workers)
        }
        on.exit(sits_parallel(workers = 0L), add = TRUE)
        RNGkind("L'Ecuyer-CMRG")
        assign(".Random.seed", c(10407L, rep(12345L, 6L)), envir = globalenv())
        sits:::.parallel_map(list(1L, 2L, 3L), function(i) stats::runif(2L))
    }
    for (workers in c(1L, 2L, 3L)) {
        expect_equal(run(workers), expected, tolerance = 1e-15)
    }
    # the caller goes on past the substreams of the tasks
    second <- sits:::.parallel_map(list(1L, 2L, 3L), function(i) stats::runif(2L))
    expect_false(any(unlist(second) %in% unlist(expected)))
    # another global generator comes back after the map
    set.seed(4L, kind = "Mersenne-Twister")
    a <- sits:::.parallel_map(list(1L, 2L), function(i) stats::runif(1L))
    expect_identical(RNGkind()[[1L]], "Mersenne-Twister")
    set.seed(4L, kind = "Mersenne-Twister")
    expect_identical(
        sits:::.parallel_map(list(1L, 2L), function(i) stats::runif(1L)), a
    )
    expect_false(identical(a[[1L]], a[[2L]]))
})
test_that("a seed out of range or an invalid state is refused by sits", {
    msg <- "seed must be an integer or a L'Ecuyer-CMRG state of 7 integers"
    f <- function(seed) {
        rng_exit <- sits:::.rng_enter(seed)
        on.exit(rng_exit(), add = TRUE)
        stats::runif(1L)
    }
    # the message of each refusal, so that one case does not stop the rest
    refused <- function(seed) {
        tryCatch(
            {
                suppressWarnings(f(seed))
                "accepted"
            },
            error = function(e) conditionMessage(e)
        )
    }
    set.seed(1L, kind = "Mersenne-Twister")
    before <- rng_get_state()
    # outside the 32-bit signed range; -2^31 is NA_integer_ in R
    expect_match(refused(2^31), msg, fixed = TRUE)
    expect_match(refused(-2^31), msg, fixed = TRUE)
    expect_match(refused(1.5), msg, fixed = TRUE)
    # kind code 03 is Mersenne-Twister, not L'Ecuyer-CMRG
    expect_match(refused(c(10403L, rep(1L, 6L))), msg, fixed = TRUE)
    # -1L is 2^32 - 1 unsigned, above both moduli of MRG32k3a
    expect_match(refused(c(10407L, -1L, 1L, 1L, 1L, 1L, 1L)), msg, fixed = TRUE)
    expect_match(refused(c(10407L, 1L, 1L, 1L, 1L, -1L, 1L)), msg, fixed = TRUE)
    # -209L is 4294967087 unsigned, equal to the first modulus
    expect_match(
        refused(c(10407L, 1L, -209L, 1L, 1L, 1L, 1L)), msg, fixed = TRUE
    )
    # -22853L is 4294944443 unsigned, equal to the second modulus
    expect_match(
        refused(c(10407L, 1L, 1L, 1L, 1L, 1L, -22853L)), msg, fixed = TRUE
    )
    # a triple of zeros cannot leave zero in MRG32k3a
    expect_match(refused(c(10407L, 0L, 0L, 0L, 1L, 1L, 1L)), msg, fixed = TRUE)
    expect_match(refused(c(10407L, 1L, 1L, 1L, 0L, 0L, 0L)), msg, fixed = TRUE)
    # doubles outside the signed 32-bit range become NA in as.integer()
    expect_match(refused(c(10407, 2^31 + 5, 1, 1, 1, 1, 1)), msg, fixed = TRUE)
    expect_match(refused(c(10407, 1, 1, 1, -2^31 - 5, 1, 1)), msg, fixed = TRUE)
    # ?RNGkind: normal kinds 0 to 5 (hundreds), sample kinds 0 to 1
    expect_match(refused(c(607L, rep(1L, 6L))), msg, fixed = TRUE)
    expect_match(refused(c(20007L, rep(1L, 6L))), msg, fixed = TRUE)
    # -93 %% 100 is 7 in R, but no kind code is negative
    expect_match(refused(c(-93L, rep(1L, 6L))), msg, fixed = TRUE)
    # a refused seed leaves the global generator as received
    expect_identical(rng_get_state(), before)
    expect_identical(RNGkind()[[1L]], "Mersenne-Twister")
    # the limits of the range and the largest components are accepted
    expect_silent(f(2^31 - 1))
    expect_silent(f(-2^31 + 1))
    expect_silent(f(c(10407L, 1L, 1L, -210L, 1L, 1L, -22854L)))
    expect_silent(f(c(10407L, 0L, 0L, 1L, 0L, 1L, 0L)))
    # Inversion with Rejection (10407) and Ahrens-Dieter with Rounding (107)
    expect_silent(f(c(10407L, rep(1L, 6L))))
    expect_silent(f(c(107L, rep(1L, 6L))))
})
