# the RNG state of the global environment
.test_rng_get_state <- function()
    get(".Random.seed", globalenv(), inherits = FALSE)

# sets the Mersenne-Twister generator to `seed` and returns its state
.test_rng_seed_mt <- function(seed = 1L) {
    set.seed(seed, kind = "Mersenne-Twister")
    .test_rng_get_state()
}

# the first two draws of substreams 1 to 3 of the seed of .test_rng_local_base()
.test_rng_subs_draws <- list(
    c(0.07939898979733463, 0.4803395047575741),
    c(0.2619834061461847, 0.5359922918692224),
    c(0.5032122888761005, 0.16517391832456343)
)

# L'Ecuyer-CMRG at rng_base; the test restores the kinds with on.exit()
.test_rng_local_base <- function() {
    rng_base <- c(10407L, rep(12345L, 6L))
    assign(".Random.seed", rng_base, envir = globalenv())
}

# n draws in the stream that .rng_enter() gives to `seed`, and its kind
.test_rng_draw <- function(seed, n = 2L) {
    rng_exit <- sits:::.rng_enter(seed)
    on.exit(rng_exit(), add = TRUE)
    list(kind = RNGkind()[[1L]], x = stats::runif(n))
}

# load_all() has no installed copy for workers or a fresh process to load
.test_rng_skip_if_not_installed <- function() {
    sits_path <- find.package("sits")
    skip_if(!dir.exists(file.path(sits_path, "Meta")), "sits is not installed")
}
test_that("RNG - L'Ecuyer-CMRG substreams", {
    old_kinds <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old_kinds)), add = TRUE)
    .test_rng_local_base()
    subs <- sits:::.rng_substreams(3L)
    # the last state is the caller's, moved past the last substream
    expect_identical(purrr::map(c(subs, list(.test_rng_get_state())), `[`, -1L),
        list(
        c(870504860L, -1653269569L, 884013853L,
          339352413L, -1920660590L, -643363409L),
        c(460387934L, 1532391390L, 877287553L,
          120103512L, -2141851355L, 335837774L),
        c(-519857236L, -1086671252L, 1257177538L,
          378684317L, -1427855118L, -2093661213L),
        c(1870130441L, 490396226L, 1081325149L,
          -609881575L, -1946924678L, 1094489500L)
    ))
    assign(".Random.seed", subs[[3L]], envir = globalenv())
    expect_equal(stats::runif(2L), .test_rng_subs_draws[[3L]],
        tolerance = 1e-15)
})
test_that("RNG - stream as seed", {
    rng_base <- c(10407L, rep(12345L, 6L))
    # computed outside sits; 0.127011122046577 is the first value published
    # with L'Ecuyer's RngStreams
    rng_first3 <- c(0.12701112204657714, 0.3185275653967945, 0.3091860155832701)
    before <- .test_rng_seed_mt()
    expect_equal(.test_rng_draw(rng_base, 3L)[["x"]], rng_first3,
        tolerance = 1e-15)
    # with seed, the state as received, kind included
    expect_identical(.test_rng_get_state(), before)
})
test_that("RNG - integer seed", {
    before <- .test_rng_seed_mt(5L)
    a <- .test_rng_draw(7L)
    expect_identical(a[["kind"]], "L'Ecuyer-CMRG")
    # as set.seed(7L) and no seed: MRG32k3a in Python, state
    # floor(draw * modulus) of the six Mersenne-Twister draws
    expect_equal(a[["x"]], c(0.6728980012617038, 0.6211333435950186),
                 tolerance = 1e-15)
    expect_identical(.test_rng_draw(7L), a)
    expect_false(identical(.test_rng_draw(8L), a))
    # with seed, the state as received, kind included
    expect_identical(.test_rng_get_state(), before)
})
test_that("RNG - integer seed, L'Ecuyer-CMRG global state", {
    old_kinds <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old_kinds)), add = TRUE)
    .test_rng_local_base()
    before <- .test_rng_get_state()
    # an integer starts from the default generator of R in any session:
    # the values of set.seed(7L) under Mersenne-Twister (Python oracle)
    expect_equal(.test_rng_draw(7L)[["x"]],
                 c(0.6728980012617038, 0.6211333435950186),
                 tolerance = 1e-15)
    expect_identical(.test_rng_get_state(), before)
})
test_that("RNG - integer seed does not depend on the session kinds", {
    data(cerrado_2classes)
    old <- RNGkind()
    on.exit(suppressWarnings(do.call(RNGkind, as.list(old))), add = TRUE)
    .test_rng_draw <- function() {
        rng_exit <- sits:::.rng_enter(7L)
        on.exit(rng_exit(), add = TRUE)
        list(RNGkind(), stats::runif(2L), stats::rnorm(2L),
             sample.int(100L, 5L))
    }
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(5L)
    ref <- .test_rng_draw()
    ref_samples <- sits_sample(cerrado_2classes, frac = 0.1, seed = 7L)
    expect_identical(ref[[1L]], c("L'Ecuyer-CMRG", "Inversion", "Rejection"))
    kinds <- list(
        c("Wichmann-Hill", "Inversion", "Rejection"),
        c("Mersenne-Twister", "Box-Muller", "Rejection"),
        c("Mersenne-Twister", "Inversion", "Rounding"),
        c("L'Ecuyer-CMRG", "Inversion", "Rejection")
    )
    for (kind in kinds) {
        suppressWarnings(do.call(RNGkind, as.list(kind)))
        set.seed(5L)
        before <- .test_rng_get_state()
        expect_identical(.test_rng_draw(), ref, info = kind)
        expect_identical(
            sits_sample(cerrado_2classes, frac = 0.1, seed = 7L),
            ref_samples, info = kind
        )
        # the session comes back, kinds included
        expect_identical(.test_rng_get_state(), before, info = kind)
        expect_identical(RNGkind(), kind, info = kind)
    }
})
test_that("RNG - integer seed inside a parallel job", {
    data(cerrado_2classes)
    set.seed(1L, kind = "Mersenne-Twister")
    top <- sits_sample(cerrado_2classes, frac = 0.1, seed = 7L)
    # the job runs under a L'Ecuyer-CMRG substream, not the session kind
    inner <- sits:::.parallel_map(list(1L), function(i) {
        list(RNGkind()[[1L]],
             sits_sample(cerrado_2classes, frac = 0.1, seed = 7L))
    })[[1L]]
    expect_identical(inner[[1L]], "L'Ecuyer-CMRG")
    expect_identical(inner[[2L]], top)
})
test_that("RNG - integer seed without .Random.seed", {
    old <- RNGkind()
    on.exit(suppressWarnings(do.call(RNGkind, as.list(old))), add = TRUE)
    # a stale L'Ecuyer-CMRG kind is what set.seed(NULL) would keep
    RNGkind("L'Ecuyer-CMRG")
    rm(".Random.seed", envir = globalenv())
    expect_equal(.test_rng_draw(7L)[["x"]],
                 c(0.6728980012617038, 0.6211333435950186),
                 tolerance = 1e-15)
})
test_that("RNG - error under a seed restores the session", {
    old <- RNGkind()
    on.exit(suppressWarnings(do.call(RNGkind, as.list(old))), add = TRUE)
    suppressWarnings(RNGkind("Wichmann-Hill", "Box-Muller", "Rounding"))
    set.seed(5L)
    before <- .test_rng_get_state()
    .test_rng_fails <- function() {
        rng_exit <- sits:::.rng_enter(7L)
        on.exit(rng_exit(), add = TRUE)
        stop("fail after entry")
    }
    expect_error(.test_rng_fails(), "fail after entry")
    expect_identical(.test_rng_get_state(), before)
    expect_identical(RNGkind(), c("Wichmann-Hill", "Box-Muller", "Rounding"))
})
test_that("RNG - seeded call inside a seeded call", {
    data(cerrado_2classes)
    set.seed(1L, kind = "Mersenne-Twister")
    top <- sits_sample(cerrado_2classes, frac = 0.1, seed = 7L)
    .test_rng_outer <- function(inner) {
        rng_exit <- sits:::.rng_enter(5L)
        on.exit(rng_exit(), add = TRUE)
        a <- stats::runif(1L)
        if (inner) res <- sits_sample(cerrado_2classes, frac = 0.1, seed = 7L)
        list(a, stats::runif(1L), if (inner) res)
    }
    plain <- .test_rng_outer(FALSE)
    nested <- .test_rng_outer(TRUE)
    expect_identical(nested[[3L]], top)
    # the inner call does not move the outer stream
    expect_identical(nested[1:2], plain[1:2])
})
test_that("RNG - torch seed under a seed does not depend on the kinds", {
    old <- RNGkind()
    on.exit(suppressWarnings(do.call(RNGkind, as.list(old))), add = TRUE)
    # the draw of .torch_set_seed(), without torch
    .test_rng_torch_seed <- function(seed = 42L) {
        rng_exit <- sits:::.rng_enter(seed)
        on.exit(rng_exit(), add = TRUE)
        sample.int(.Machine[["integer.max"]], 1L)
    }
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    ref <- .test_rng_torch_seed()
    # as set.seed(42L) and no seed
    set.seed(42L)
    expect_identical(.test_rng_torch_seed(NULL), ref)
    for (kind in list(c("Wichmann-Hill", "Inversion", "Rounding"),
                      c("Mersenne-Twister", "Box-Muller", "Rounding"),
                      c("L'Ecuyer-CMRG", "Ahrens-Dieter", "Rounding"))) {
        suppressWarnings(do.call(RNGkind, as.list(kind)))
        set.seed(5L)
        expect_identical(.test_rng_torch_seed(), ref, info = kind)
    }
})
test_that("RNG - seeded sample inside real workers", {
    .test_rng_skip_if_not_installed()
    data(cerrado_2classes)
    set.seed(1L, kind = "Mersenne-Twister")
    top <- sits_sample(cerrado_2classes, frac = 0.1, seed = 7L)
    started <- sits:::.parallel_start(2L)
    on.exit(sits:::.parallel_stop(started), add = TRUE)
    # the data travels as an argument; the worker has no `cerrado_2classes`
    inner <- sits:::.parallel_map(list(cerrado_2classes), function(x) {
        sits::sits_sample(x, frac = 0.1, seed = 7L)
    })
    expect_identical(inner[[1L]], top)
})
test_that("RNG - seed gives the result of set.seed()", {
    data(cerrado_2classes)
    set.seed(7L, kind = "Mersenne-Twister")
    by_session <- sits_sample(cerrado_2classes, frac = 0.1)
    before <- .test_rng_seed_mt(5L)
    by_seed <- sits_sample(cerrado_2classes, frac = 0.1, seed = 7L)
    # instrument: another seed gives other samples
    expect_false(identical(
        sits_sample(cerrado_2classes, frac = 0.1, seed = 8L), by_seed
    ))
    expect_identical(by_seed, by_session)
    expect_identical(.test_rng_get_state(), before)
})
test_that("RNG - no seed, L'Ecuyer-CMRG global state", {
    # computed outside sits; 0.127011122046577 is the first value published
    # with L'Ecuyer's RngStreams
    rng_first3 <- c(0.12701112204657714, 0.3185275653967945, 0.3091860155832701)
    old_kinds <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old_kinds)), add = TRUE)
    .test_rng_local_base()
    expect_equal(.test_rng_draw(NULL, 3L)[["x"]], rng_first3, tolerance = 1e-15)
    # the restored state includes the draws spent
    expect_identical(.test_rng_get_state()[-1L], c(-1271176443L, -1271176443L,
        -909607723L, -1816685032L, 1655725443L, 2057415812L))
    expect_equal(stats::runif(1L), 0.8258468629271136, tolerance = 1e-15)
})
test_that("RNG - no seed, other global generator", {
    set.seed(7L, kind = "Mersenne-Twister")
    invisible(stats::runif(6L))
    after_six <- .test_rng_get_state()
    set.seed(7L, kind = "Mersenne-Twister")
    a <- .test_rng_draw(NULL)
    expect_identical(a[["kind"]], "L'Ecuyer-CMRG")
    # MRG32k3a in Python, state floor(draw * modulus) of the six draws
    expect_equal(a[["x"]], c(0.6728980012617038, 0.6211333435950186),
                 tolerance = 1e-15)
    # the kind comes back, and the state includes the six draws
    expect_identical(.test_rng_get_state(), after_six)
    # two calls in a row get two streams; set.seed() repeats them
    expect_false(identical(.test_rng_draw(NULL), a))
    set.seed(7L, kind = "Mersenne-Twister")
    expect_identical(.test_rng_draw(NULL), a)
})
test_that("RNG - invalid seed", {
    msg <- "seed must be an integer or a L'Ecuyer-CMRG state of 7 integers"
    expect_error(.test_rng_draw("a"), msg, fixed = TRUE)
    expect_error(.test_rng_draw(c(1L, 2L)), msg, fixed = TRUE)
    expect_error(.test_rng_draw(c(10403L, rep(1L, 6L))), msg, fixed = TRUE)
})
test_that("RNG - substream of each parallel job", {
    old_kinds <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old_kinds)), add = TRUE)
    .test_rng_skip_if_not_installed()
    .test_rng_map <- function(n, k) {
        sits:::.parallel_map(seq_len(n), function(i) stats::runif(k))
    }
    on.exit(sits_parallel(workers = 0L), add = TRUE)
    for (workers in c(2L, 1L)) {
        sits_parallel(workers = workers)
        .test_rng_local_base()
        expect_equal(.test_rng_map(3L, 2L), .test_rng_subs_draws,
            tolerance = 1e-15)
    }
    # no cluster now; the caller goes on past the substreams of the jobs
    expect_false(any(
        unlist(.test_rng_map(3L, 2L)) %in% unlist(.test_rng_subs_draws)
    ))
    # another global generator comes back after the map
    set.seed(4L, kind = "Mersenne-Twister")
    a <- .test_rng_map(2L, 1L)
    expect_identical(RNGkind()[[1L]], "Mersenne-Twister")
    set.seed(4L, kind = "Mersenne-Twister")
    expect_identical(.test_rng_map(2L, 1L), a)
    expect_false(identical(a[[1L]], a[[2L]]))
})
test_that("RNG - substream of each job of a queue", {
    old_kinds <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old_kinds)), add = TRUE)
    .test_rng_skip_if_not_installed()
    # job 2 goes first and job 3 waits for jobs 1 and 2: with a cluster
    # the send order is 2, 1, 3, not the index order
    .test_rng_queue <- function(n, k) {
        after <- rep(list(integer(0L)), n)
        after[[3L]] <- 1:2
        sits:::.parallel_queue(
            as.list(seq_len(n)), function(i) stats::runif(k),
            after = after, first = rep(c(FALSE, TRUE), length.out = n)
        )
    }
    on.exit(sits_parallel(workers = 0L), add = TRUE)
    for (workers in c(2L, 1L)) {
        sits_parallel(workers = workers)
        .test_rng_local_base()
        expect_equal(.test_rng_queue(3L, 2L), .test_rng_subs_draws,
            tolerance = 1e-15)
    }
    # the caller goes on as after a map of as many jobs
    .test_rng_local_base()
    .test_rng_queue(3L, 2L)
    after_queue <- stats::runif(1L)
    .test_rng_local_base()
    sits:::.parallel_map(1:3, function(i) stats::runif(2L))
    expect_identical(after_queue, stats::runif(1L))
})
test_that("RNG - seed out of range or invalid state", {
    msg <- "seed must be an integer or a L'Ecuyer-CMRG state of 7 integers"
    .test_rng_s <- function(i, v) replace(c(10407L, rep(1L, 6L)), i, v)
    bad <- c(
        # outside the signed 32-bit range (-2^31 is NA_integer_), not whole
        list(2^31, -2^31, 1.5, .test_rng_s(2L, 2^31 + 5), .test_rng_s(5L,
            -2^31 - 5)),
        # as unsigned, -1L is above both moduli; -209L, -22853L equal them
        list(.test_rng_s(2L, -1L), .test_rng_s(6L, -1L), .test_rng_s(3L, -209L),
            .test_rng_s(7L, -22853L)),
        # a triple of zeros; kind codes 03, normal 6, sample 2, negative
        list(.test_rng_s(2L:4L, 0L), .test_rng_s(5L:7L, 0L)),
        purrr::map(c(10403L, 607L, 20007L, -93L), .test_rng_s, i = 1L)
    )
    before <- .test_rng_seed_mt()
    purrr::walk(bad, function(seed) {
        expect_error(.test_rng_draw(seed), msg, fixed = TRUE)
    })
    expect_identical(.test_rng_get_state(), before)
    # the limits of the range and components; kinds 10407 and 107 exist
    ok <- c(list(2^31 - 1, -2^31 + 1, .test_rng_s(c(4L, 7L), c(-210L,
        -22854L))),
            list(.test_rng_s(c(2L, 3L, 5L, 7L), 0L)),
            purrr::map(c(10407L, 107L), .test_rng_s, i = 1L))
    purrr::walk(ok, function(seed) expect_silent(.test_rng_draw(seed)))
})
test_that("RNG - package check", {
    # some packages draw when they load (torch 0.17.0 in .onLoad), so the
    # check of the packages runs requireNamespace() in .rng_protect()
    before <- .test_rng_seed_mt()
    sits:::.rng_protect(invisible(stats::runif(1L)))
    expect_identical(.test_rng_get_state(), before)
    # instrument: the same draw outside the guard moves the generator
    invisible(stats::runif(1L))
    expect_false(identical(.test_rng_get_state(), before))
    # the check of a package goes through the guard
    seen <- FALSE
    testthat::local_mocked_bindings(.rng_protect = function(expr) {
        seen <<- TRUE
        expr
    })
    sits:::.check_require_packages("stats")
    expect_true(seen)
})
test_that("RNG - package check with unseeded generator", {
    saved <- .test_rng_seed_mt()
    on.exit(assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
    rm(".Random.seed", envir = globalenv())
    sits:::.rng_protect(invisible(stats::runif(1L)))
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    # instrument: the same draw outside the guard creates the state
    invisible(stats::runif(1L))
    expect_true(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

# runs `setup` and `code` in a new sits worker; skips without a working torch
.test_rng_fresh <- function(code, setup = NULL) {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    skip_if_not(torch::torch_is_installed(), "torch is not functional")
    .test_rng_skip_if_not_installed()
    sits_path <- find.package("sits")
    started <- .parallel_start(workers = 1L, force = TRUE)
    on.exit(.parallel_stop(started), add = TRUE)
    skip_if(!started, "a sits cluster is open, so the worker is not new")
    .test_rng_job <- function(setup, code) {
        suppressPackageStartupMessages(library(sits))
        .test_rng_torch_loaded <- function() "torch" %in% loadedNamespaces()
        res <- list(sits_path = find.package("sits"),
            torch = .test_rng_torch_loaded())
        env <- environment()
        if (!is.null(setup)) eval(setup, env)
        eval(code, env)
        res
    }
    environment(.test_rng_job) <- globalenv()
    # clusterCall() and not .parallel_map(), which gives the job its own
    # random substream: the session must start as R starts
    res <- parallel::clusterCall(
        sits_env[["cluster"]], .test_rng_job, setup, substitute(code)
    )[[1L]]
    # each case needs the sits under test and a torch not yet loaded
    expect_identical(normalizePath(res[["sits_path"]]),
                     normalizePath(sits_path))
    expect_false(res[["torch"]])
    res
}

# every 6th row mixes the four classes; no draw builds it
.test_rng_mlp <- quote({
    smp <- samples_modis_ndvi[seq(1L, nrow(samples_modis_ndvi), by = 6L), ]
    point <- sits_select(point_mt_6bands, bands = "NDVI")
    .test_rng_train <- function(seed)
        sits_train(smp, sits_mlp(epochs = 2L, seed = seed))
    .test_rng_probs_of <- function(model) {
        pred <- sits_classify(point, model, progress = FALSE)
        pred <- pred[["predicted"]][[1L]]
        as.matrix(pred[purrr::map_lgl(pred, is.numeric)])
    }
})
test_that("RNG - first torch training with set.seed()", {
    skip_if(!.torch_cpu_train(),
            "torch training on a GPU is not reproducible")
    res <- .test_rng_fresh(setup = .test_rng_mlp, {
        set.seed(1L)
        res[["first"]] <- .test_rng_probs_of(.test_rng_train(NULL))
        set.seed(1L)
        res[["second"]] <- .test_rng_probs_of(.test_rng_train(NULL))
        set.seed(2L)
        res[["other"]] <- .test_rng_probs_of(.test_rng_train(NULL))
    })
    # instrument: probabilities not all 0 or 1; another seed changes them
    expect_false(all(res[["first"]] %in% c(0, 1)))
    expect_false(isTRUE(all.equal(res[["other"]], res[["second"]])))
    expect_identical(res[["first"]], res[["second"]])
})
test_that("RNG - first torch training with seed", {
    skip_if(!.torch_cpu_train(),
            "torch training on a GPU is not reproducible")
    res <- .test_rng_fresh(setup = .test_rng_mlp, {
        set.seed(1L)
        res[["before"]] <- .Random.seed
        model <- .test_rng_train(42L)
        # read before sits_classify(), which draws without seed
        res[["after"]] <- .Random.seed
        res[["first"]] <- .test_rng_probs_of(model)
        res[["second"]] <- .test_rng_probs_of(.test_rng_train(42L))
        res[["other"]] <- .test_rng_probs_of(.test_rng_train(43L))
        set.seed(42L)
        res[["set_seed"]] <- .test_rng_probs_of(.test_rng_train(NULL))
    })
    # instrument: another seed changes the probabilities
    expect_false(all(res[["first"]] %in% c(0, 1)))
    expect_false(isTRUE(all.equal(res[["other"]], res[["first"]])))
    expect_identical(res[["first"]], res[["second"]])
    # the seed gives the training of set.seed() with the same integer
    expect_identical(res[["first"]], res[["set_seed"]])
    expect_identical(res[["after"]], res[["before"]])
})
test_that("RNG - torch check", {
    res <- .test_rng_fresh({
        set.seed(1L)
        res[["before"]] <- .Random.seed
        res[["functional"]] <- sits:::.torch_is_functional()
        res[["after"]] <- .Random.seed
        res[["torch_after"]] <- .test_rng_torch_loaded()
    })
    expect_true(res[["functional"]])
    expect_true(res[["torch_after"]])
    expect_identical(res[["after"]], res[["before"]])
    # instrument: a plain load of torch moves the generator
    plain <- .test_rng_fresh({
        set.seed(1L)
        res[["before"]] <- .Random.seed
        loadNamespace("torch")
        res[["after"]] <- .Random.seed
    })
    expect_false(identical(plain[["after"]], plain[["before"]]))
})
test_that("RNG - first torch tuning with seed", {
    skip_if(!.torch_cpu_train(),
            "torch training on a GPU is not reproducible")
    res <- .test_rng_fresh({
        .test_rng_tune <- function(seed) {
            hp <- sits_tuning_hparams(
                epochs = 2L, opt_hparams = list(lr = loguniform(0.01, 0.0001))
            )
            tuned <- sits_tuning(samples_modis_ndvi, ml_method = sits_mlp(),
                params = hp, trials = 2L, multicores = 1L, seed = seed)
            as.matrix(tuned[c("accuracy", "kappa")])
        }
        res[["first"]] <- .test_rng_tune(42L)
        res[["torch_after"]] <- .test_rng_torch_loaded()
        res[["second"]] <- .test_rng_tune(42L)
        res[["other"]] <- .test_rng_tune(43L)
    })
    expect_true(res[["torch_after"]])
    # instrument: another seed changes the accuracies
    expect_false(isTRUE(all.equal(res[["other"]], res[["second"]])))
    expect_identical(res[["first"]], res[["second"]])
})

# the class map in extdata; one worker, as a cluster start would load torch
.test_rng_cube <- quote({
    cube <- sits_cube(
        source = "MPC", collection = "SENTINEL-2-L2A", bands = "class",
        data_dir = system.file("extdata/raster/classif", package = "sits"),
        labels = c("1" = "ClearCut_Fire", "2" = "ClearCut_BareSoil",
                   "3" = "ClearCut_Veg", "4" = "Forest"),
        multicores = 1L, progress = FALSE
    )
    res[["torch_cube"]] <- .test_rng_torch_loaded()
    .test_rng_strata <- function(seed) {
        sf::st_coordinates(sits_stratified_sampling(cube,
            samples_per_class = 20L, multicores = 2L, progress = FALSE,
            seed = seed))
    }
})
test_that("RNG - first stratified sampling with seed", {
    res <- .test_rng_fresh(setup = .test_rng_cube, {
        set.seed(1L)
        res[["before"]] <- .Random.seed
        res[["first"]] <- .test_rng_strata(42L)
        res[["after"]] <- .Random.seed
        res[["torch_after"]] <- .test_rng_torch_loaded()
        res[["second"]] <- .test_rng_strata(42L)
        res[["before_noseed"]] <- .Random.seed
        invisible(.test_rng_strata(NULL))
        res[["after_noseed"]] <- .Random.seed
    })
    # neither the cube nor the start of the cluster loads torch
    expect_false(res[["torch_cube"]])
    expect_false(res[["torch_after"]])
    # instrument: without seed the sampling moves the generator
    expect_false(identical(res[["after_noseed"]], res[["before_noseed"]]))
    expect_identical(res[["first"]], res[["second"]])
    expect_identical(res[["after"]], res[["before"]])
})
test_that("RNG - package that fails to load", {
    # a package that draws and then fails in .onLoad: the guard restores the
    # generator on the error too
    before <- .test_rng_seed_mt()
    expect_error(
        sits:::.rng_protect({
            stats::runif(1L)
            stop("load fails")
        }),
        "load fails"
    )
    expect_identical(.test_rng_get_state(), before)
    # instrument: the same draw and error outside the guard move it
    set.seed(1L, kind = "Mersenne-Twister")
    expect_error({
        stats::runif(1L)
        stop("load fails")
    }, "load fails")
    expect_false(identical(.test_rng_get_state(), before))
    # a package that is not there gives the message of the check
    expect_error(sits:::.check_require_packages("sitsnosuchpackage"),
                 "Please install package(s) sitsnosuchpackage", fixed = TRUE)
})
test_that("Classification - non-torch model", {
    res <- .test_rng_fresh({
        model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 50L))
        res[["torch_train"]] <- .test_rng_torch_loaded()
        point <- sits_select(point_mt_6bands, bands = "NDVI")
        pred <- sits_classify(point, model, multicores = 1L, progress = FALSE)
        res[["classes"]] <- nrow(pred[["predicted"]][[1L]])
        res[["torch_classify"]] <- .test_rng_torch_loaded()
    })
    # the training does not load torch, and the classification ran
    expect_false(res[["torch_train"]])
    expect_identical(res[["classes"]], 17L)
    expect_false(res[["torch_classify"]])
})
test_that("RNG - first stratified sampling with set.seed()", {
    res <- .test_rng_fresh(setup = .test_rng_cube, {
        set.seed(1L)
        res[["first"]] <- .test_rng_strata(NULL)
        res[["torch_after"]] <- .test_rng_torch_loaded()
        set.seed(1L)
        res[["second"]] <- .test_rng_strata(NULL)
        set.seed(2L)
        res[["other"]] <- .test_rng_strata(NULL)
    })
    # neither the cube nor the start of the cluster loads torch
    expect_false(res[["torch_cube"]])
    expect_false(res[["torch_after"]])
    # instrument: another seed changes the samples
    expect_false(identical(res[["other"]], res[["second"]]))
    expect_identical(res[["first"]], res[["second"]])
})
