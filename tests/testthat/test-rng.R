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
# Installs a throwaway package whose .onLoad draws, as torch 0.17.0 does,
# and puts its library first in .libPaths(); returns the package name and
# the function that undoes both
rng_onload_package <- function() {
    pkg <- "sitsrngonload"
    src <- file.path(tempfile("rngsrc"), pkg)
    lib <- tempfile("rnglib")
    dir.create(file.path(src, "R"), recursive = TRUE)
    dir.create(lib)
    writeLines(
        c(
            paste("Package:", pkg),
            "Version: 0.0.1",
            "Title: Draws on Load",
            "Description: Draws one number when loaded.",
            "Author: Test",
            "Maintainer: Test <test@example.com>",
            "License: GPL-3"
        ),
        file.path(src, "DESCRIPTION")
    )
    writeLines("", file.path(src, "NAMESPACE"))
    writeLines(
        ".onLoad <- function(libname, pkgname) invisible(stats::runif(1L))",
        file.path(src, "R", "zzz.R")
    )
    system2(
        file.path(R.home("bin"), "R"),
        c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(src)),
        stdout = FALSE, stderr = FALSE
    )
    old_libs <- .libPaths()
    # unload before the library goes
    cleanup <- function() {
        if (pkg %in% loadedNamespaces()) unloadNamespace(pkg)
        .libPaths(old_libs)
        unlink(c(dirname(src), lib), recursive = TRUE)
    }
    if (!dir.exists(file.path(lib, pkg))) {
        cleanup()
        skip("throwaway package not installed")
    }
    .libPaths(c(lib, old_libs))
    list(pkg = pkg, cleanup = cleanup)
}
test_that("loading a package to check it leaves the R generator as received", {
    skip_on_cran()
    onload <- rng_onload_package()
    on.exit(onload[["cleanup"]](), add = TRUE)
    pkg <- onload[["pkg"]]
    set.seed(1L, kind = "Mersenne-Twister")
    before <- rng_get_state()
    sits:::.check_require_packages(pkg)
    expect_true(pkg %in% loadedNamespaces())
    expect_identical(rng_get_state(), before)
    # instrument: a plain load of the same package does move the generator
    unloadNamespace(pkg)
    set.seed(1L, kind = "Mersenne-Twister")
    loadNamespace(pkg)
    expect_false(identical(rng_get_state(), before))
})
test_that("loading a package to check it leaves an unseeded R generator", {
    skip_on_cran()
    onload <- rng_onload_package()
    on.exit(onload[["cleanup"]](), add = TRUE)
    pkg <- onload[["pkg"]]
    set.seed(1L, kind = "Mersenne-Twister")
    saved <- rng_get_state()
    on.exit(rng_set_state(saved), add = TRUE)
    rm(".Random.seed", envir = globalenv())
    sits:::.check_require_packages(pkg)
    expect_true(pkg %in% loadedNamespaces())
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    # instrument: a plain load from the same unseeded start creates the state
    unloadNamespace(pkg)
    loadNamespace(pkg)
    expect_true(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
# Runs the given lines in a fresh R process, with the sits installed where
# the test process loaded it from; the lines add their results to `res`
rng_fresh <- function(lines) {
    sits_path <- find.package("sits")
    # load_all() has no installed copy for the fresh process to load
    skip_if(
        !dir.exists(file.path(sits_path, "Meta")), "sits is not installed"
    )
    script <- tempfile("rngfresh", fileext = ".R")
    out <- tempfile("rngfresh", fileext = ".rds")
    on.exit(unlink(c(script, out)), add = TRUE)
    writeLines(
        c(
            "suppressPackageStartupMessages(library(sits))",
            "res <- list(",
            "    sits_path = find.package('sits'),",
            "    torch_loaded = 'torch' %in% loadedNamespaces()",
            ")",
            lines,
            "saveRDS(res, commandArgs(trailingOnly = TRUE)[[1L]])"
        ),
        script
    )
    libs <- paste(
        unique(c(dirname(sits_path), .libPaths())),
        collapse = .Platform$path.sep
    )
    status <- system2(
        file.path(R.home("bin"), "Rscript"),
        c(shQuote(script), shQuote(out)),
        stdout = FALSE, stderr = FALSE,
        env = paste0("R_LIBS=", shQuote(libs))
    )
    expect_identical(status, 0L)
    skip_if(!file.exists(out), "the fresh R process wrote no result")
    res <- readRDS(out)
    # each case needs the sits under test and a torch not yet loaded
    expect_identical(
        normalizePath(res[["sits_path"]]), normalizePath(sits_path)
    )
    expect_false(res[["torch_loaded"]])
    res
}
skip_if_no_torch <- function() {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    skip_if_not(torch::torch_is_installed(), "torch is not functional")
}
# every 6th row mixes the four classes; no draw builds it
rng_mlp_lines <- c(
    "smp <- samples_modis_ndvi[",
    "    seq(1L, nrow(samples_modis_ndvi), by = 6L), ]",
    "point <- sits_select(point_mt_6bands, bands = 'NDVI')",
    "train <- function(seed) {",
    "    sits_train(smp, sits_mlp(epochs = 2L, seed = seed))",
    "}",
    "probs_of <- function(model) {",
    "    pred <- sits_classify(point, model, progress = FALSE)",
    "    pred <- pred[['predicted']][[1L]]",
    "    as.matrix(pred[vapply(pred, is.numeric, logical(1L))])",
    "}"
)
test_that("without seed, the first torch training repeats under set.seed()", {
    skip_if_no_torch()
    res <- rng_fresh(c(
        rng_mlp_lines,
        "set.seed(1L)",
        "res[['first']] <- probs_of(train(NULL))",
        "set.seed(1L)",
        "res[['second']] <- probs_of(train(NULL))",
        "set.seed(2L)",
        "res[['other']] <- probs_of(train(NULL))"
    ))
    # instrument: probabilities are not all 0 or 1,
    # and another seed changes them
    expect_false(all(res[["first"]] %in% c(0, 1)))
    expect_false(isTRUE(all.equal(res[["other"]], res[["second"]])))
    expect_identical(res[["first"]], res[["second"]])
})
test_that("with seed, the first torch training leaves the R generator", {
    skip_if_no_torch()
    res <- rng_fresh(c(
        rng_mlp_lines,
        "set.seed(1L)",
        "res[['before']] <- .Random.seed",
        "model <- train(42L)",
        "# read before sits_classify(), which draws without seed",
        "res[['after']] <- .Random.seed",
        "res[['first']] <- probs_of(model)",
        "res[['second']] <- probs_of(train(42L))",
        "res[['other']] <- probs_of(train(43L))"
    ))
    # instrument: another seed changes the probabilities
    expect_false(all(res[["first"]] %in% c(0, 1)))
    expect_false(isTRUE(all.equal(res[["other"]], res[["first"]])))
    expect_identical(res[["first"]], res[["second"]])
    expect_identical(res[["after"]], res[["before"]])
})
test_that("checking that torch is functional leaves the R generator", {
    skip_if_no_torch()
    res <- rng_fresh(c(
        "set.seed(1L)",
        "res[['before']] <- .Random.seed",
        "res[['functional']] <- sits:::.torch_is_functional()",
        "res[['after']] <- .Random.seed",
        "res[['torch_after']] <- 'torch' %in% loadedNamespaces()"
    ))
    # the check is the call that loaded torch
    expect_true(res[["functional"]])
    expect_true(res[["torch_after"]])
    expect_identical(res[["after"]], res[["before"]])
    # instrument: a plain load of torch moves the generator
    plain <- rng_fresh(c(
        "set.seed(1L)",
        "res[['before']] <- .Random.seed",
        "loadNamespace('torch')",
        "res[['after']] <- .Random.seed"
    ))
    expect_false(identical(plain[["after"]], plain[["before"]]))
})
test_that("with seed, the first torch tuning repeats in a session", {
    skip_if_no_torch()
    res <- rng_fresh(c(
        "tune <- function(seed) {",
        "    tuned <- sits_tuning(",
        "        samples_modis_ndvi,",
        "        ml_method = sits_mlp(),",
        "        params = sits_tuning_hparams(",
        "            epochs = 2L,",
        "            opt_hparams = list(lr = loguniform(0.01, 0.0001))",
        "        ),",
        "        trials = 2L, multicores = 1L, seed = seed",
        "    )",
        "    as.matrix(tuned[c('accuracy', 'kappa')])",
        "}",
        "res[['first']] <- tune(42L)",
        "res[['torch_after']] <- 'torch' %in% loadedNamespaces()",
        "res[['second']] <- tune(42L)",
        "res[['other']] <- tune(43L)"
    ))
    expect_true(res[["torch_after"]])
    # instrument: another seed changes the accuracies
    expect_false(isTRUE(all.equal(res[["other"]], res[["second"]])))
    expect_identical(res[["first"]], res[["second"]])
})
test_that("with seed, the first stratified sampling leaves the R generator", {
    skip_if_no_torch()
    res <- rng_fresh(c(
        "cube <- sits_cube(",
        "    source = 'MPC', collection = 'SENTINEL-2-L2A',",
        "    data_dir = system.file('extdata/raster/classif',",
        "        package = 'sits'),",
        "    parse_info = c('X1', 'X2', 'tile', 'start_date', 'end_date',",
        "        'band', 'version'),",
        "    bands = 'class',",
        "    labels = c('1' = 'ClearCut_Fire', '2' = 'ClearCut_BareSoil',",
        "        '3' = 'ClearCut_Veg', '4' = 'Forest'),",
        "    # one worker: a cluster start here would load torch",
        "    multicores = 1L, progress = FALSE",
        ")",
        "res[['torch_cube']] <- 'torch' %in% loadedNamespaces()",
        "strata <- function(seed) {",
        "    samples <- sits_stratified_sampling(",
        "        cube, samples_per_class = 20L, multicores = 2L,",
        "        progress = FALSE, seed = seed",
        "    )",
        "    sf::st_coordinates(samples)",
        "}",
        "set.seed(1L)",
        "res[['before']] <- .Random.seed",
        "res[['first']] <- strata(42L)",
        "res[['after']] <- .Random.seed",
        "res[['torch_after']] <- 'torch' %in% loadedNamespaces()",
        "res[['second']] <- strata(42L)",
        "res[['before_noseed']] <- .Random.seed",
        "invisible(strata(NULL))",
        "res[['after_noseed']] <- .Random.seed"
    ))
    # torch is loaded by the start of the cluster, not by the cube
    expect_false(res[["torch_cube"]])
    expect_true(res[["torch_after"]])
    # instrument: without seed the sampling moves the generator
    expect_false(identical(res[["after_noseed"]], res[["before_noseed"]]))
    expect_identical(res[["first"]], res[["second"]])
    expect_identical(res[["after"]], res[["before"]])
})
