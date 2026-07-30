test_that("vendored dtw2vec reproduces IncDTW::dtw2vec exactly", {
    skip_if_not_installed("IncDTW")

    # define reference function
    ref <- function(a, b) {
        IncDTW::dtw2vec(
            Q = a,
            C = b,
            dist_method  = "norm1",
            step_pattern = "symmetric2"
        )[["distance"]]
    }

    # case generators covering:
    # - equal/unequal lengths
    # - length-1,
    # - identical
    # - constant
    # - large-magnitude
    # - strongly asymmetric series
    generators <- list(
        function() list(
            cumsum(stats::rnorm(12)),
            cumsum(stats::rnorm(12))
        ),
        function() list(
            stats::rnorm(sample(2:40, 1)),
            stats::rnorm(sample(2:40, 1))
        ),
        function() list(
            stats::rnorm(1),
            stats::rnorm(1)
        ),
        function() {
            v <- cumsum(stats::rnorm(15))
            list(v, v)
        },
        function() list(
            rep(0.3, 10),
            rep(-0.7, 10)
        ),
        function() list(
            stats::runif(20, 1e3, 1e6),
            stats::runif(25, 1e3, 1e6)
        ),
        function() list(
            round(stats::rnorm(8), 2),
            round(stats::rnorm(30), 2)
        )
    )

    max_diff <- 0
    for (gen in generators) {

        # test 200 times all the cases
        for (rep in seq_len(200L)) {
            # generate test
            ab <- gen()
            a <- as.numeric(ab[[1L]])
            b <- as.numeric(ab[[2L]])

            # use sits dtw
            ours <- dtw2vec_cpp(a, b)

            # use IncDTW
            theirs <- ref(a, b)

            # compare
            expect_equal(ours, theirs, tolerance = 1e-9)

            # max difference
            max_diff <- max(max_diff, abs(ours - theirs))
        }
    }

    # numerical agreement should be at floating-point noise level
    expect_lt(max_diff, 1e-6)
})

test_that("vendored dtw2vec has expected DTW properties", {
    # identical series must produce distance 0
    expect_equal(dtw2vec_cpp(as.numeric(1:12), as.numeric(1:12)), 0)

    # warping actually happens: on a phase-shifted series DTW is below the
    # plain Euclidean distance.
    s <- sin(seq(0, 2 * pi, length.out = 12L))
    s_shift <- c(s[3:12], s[1:2])

    # euclidean distance
    euclid <- sqrt(sum((s - s_shift)^2))

    # dtw is smaller than euclidean
    expect_lt(dtw2vec_cpp(s, s_shift), euclid)

    # symmetry of the distance
    a <- cumsum(stats::rnorm(20))
    b <- cumsum(stats::rnorm(25))

    expect_equal(dtw2vec_cpp(a, b), dtw2vec_cpp(b, a), tolerance = 1e-9)
})
