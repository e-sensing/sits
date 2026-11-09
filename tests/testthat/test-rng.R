# Expected values computed outside sits; the first one, 0.127011122046577,
# is the value published with L'Ecuyer's RngStreams
rng_base <- c(10407L, rep(12345L, 6L))
rng_first3 <- c(0.12701112204657714, 0.3185275653967945, 0.3091860155832701)
# the first two draws of substreams 1 to 3 of rng_base
rng_subs_draws <- list(
    c(0.07939898979733463, 0.4803395047575741),
    c(0.2619834061461847, 0.5359922918692224),
    c(0.5032122888761005, 0.16517391832456343)
)
# L'Ecuyer-CMRG at rng_base, until the calling test ends
rng_local_base <- function(env = parent.frame()) {
    old <- RNGkind()
    rng_defer(function() do.call(RNGkind, as.list(old)), env)
    assign(".Random.seed", rng_base, envir = globalenv())
}
# n draws in the stream that .rng_enter() gives to `seed`, and its kind
rng_draw <- function(seed, n = 2L) {
    rng_exit <- sits:::.rng_enter(seed)
    on.exit(rng_exit(), add = TRUE)
    list(kind = RNGkind()[[1L]], x = stats::runif(n))
}
# load_all() has no installed copy for workers or a fresh process to load
rng_skip_if_not_installed <- function() {
    sits_path <- find.package("sits")
    skip_if(!dir.exists(file.path(sits_path, "Meta")), "sits is not installed")
}
test_that("substreams have the expected L'Ecuyer-CMRG states", {
    rng_local_base()
    subs <- sits:::.rng_substreams(3L)
    # the last state is the caller's, moved past the last substream
    expect_identical(lapply(c(subs, list(rng_get_state())), `[`, -1L), list(
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
    expect_equal(stats::runif(2L), rng_subs_draws[[3L]], tolerance = 1e-15)
})
test_that("a stream given as seed is used as given", {
    before <- rng_seed_mt()
    expect_equal(rng_draw(rng_base, 3L)[["x"]], rng_first3, tolerance = 1e-15)
    # with seed, the state as received, kind included
    expect_identical(rng_get_state(), before)
})
test_that("an integer seed gives the same draws and leaves the state", {
    before <- rng_seed_mt(5L)
    a <- rng_draw(42L)
    expect_identical(a[["kind"]], "L'Ecuyer-CMRG")
    # set.seed(42L, kind = "L'Ecuyer-CMRG") and runif(2L) in base R
    expect_equal(a[["x"]], c(0.17384558454153168, 0.55474009676509084),
                 tolerance = 1e-15)
    expect_identical(rng_draw(42L), a)
    expect_false(identical(rng_draw(43L), a))
    expect_identical(rng_get_state(), before)
})
test_that("without seed, a L'Ecuyer-CMRG global state goes on", {
    rng_local_base()
    expect_equal(rng_draw(NULL, 3L)[["x"]], rng_first3, tolerance = 1e-15)
    # the restored state includes the draws spent
    expect_identical(rng_get_state()[-1L], c(-1271176443L, -1271176443L,
        -909607723L, -1816685032L, 1655725443L, 2057415812L))
    expect_equal(stats::runif(1L), 0.8258468629271136, tolerance = 1e-15)
})
test_that("without seed, another global generator gives six draws", {
    set.seed(7L, kind = "Mersenne-Twister")
    invisible(stats::runif(6L))
    after_six <- rng_get_state()
    set.seed(7L, kind = "Mersenne-Twister")
    a <- rng_draw(NULL)
    expect_identical(a[["kind"]], "L'Ecuyer-CMRG")
    # MRG32k3a in Python, state floor(draw * modulus) of the six draws
    expect_equal(a[["x"]], c(0.6728980012617038, 0.6211333435950186),
                 tolerance = 1e-15)
    # the kind comes back, and the state includes the six draws
    expect_identical(rng_get_state(), after_six)
    # two calls in a row get two streams; set.seed() repeats them
    expect_false(identical(rng_draw(NULL), a))
    set.seed(7L, kind = "Mersenne-Twister")
    expect_identical(rng_draw(NULL), a)
})
test_that("an invalid seed is refused", {
    msg <- "seed must be an integer or a L'Ecuyer-CMRG state of 7 integers"
    expect_error(rng_draw("a"), msg, fixed = TRUE)
    expect_error(rng_draw(c(1L, 2L)), msg, fixed = TRUE)
    expect_error(rng_draw(c(10403L, rep(1L, 6L))), msg, fixed = TRUE)
})
test_that("parallel jobs draw from the substream of their index", {
    rng_skip_if_not_installed()
    map <- function(n, k) {
        sits:::.parallel_map(seq_len(n), function(i) stats::runif(k))
    }
    on.exit(sits_parallel(workers = 0L), add = TRUE)
    for (workers in c(3L, 2L, 1L)) {
        sits_parallel(workers = workers)
        rng_local_base()
        expect_equal(map(3L, 2L), rng_subs_draws, tolerance = 1e-15)
    }
    # no cluster now; the caller goes on past the substreams of the jobs
    expect_false(any(unlist(map(3L, 2L)) %in% unlist(rng_subs_draws)))
    # another global generator comes back after the map
    set.seed(4L, kind = "Mersenne-Twister")
    a <- map(2L, 1L)
    expect_identical(RNGkind()[[1L]], "Mersenne-Twister")
    set.seed(4L, kind = "Mersenne-Twister")
    expect_identical(map(2L, 1L), a)
    expect_false(identical(a[[1L]], a[[2L]]))
})
test_that("a seed out of range or an invalid state is refused by sits", {
    msg <- "seed must be an integer or a L'Ecuyer-CMRG state of 7 integers"
    s <- function(i, v) replace(c(10407L, rep(1L, 6L)), i, v)
    bad <- c(
        # outside the signed 32-bit range (-2^31 is NA_integer_), not whole
        list(2^31, -2^31, 1.5, s(2L, 2^31 + 5), s(5L, -2^31 - 5)),
        # as unsigned, -1L is above both moduli; -209L, -22853L equal them
        list(s(2L, -1L), s(6L, -1L), s(3L, -209L), s(7L, -22853L)),
        # a triple of zeros; kind codes 03, normal 6, sample 2, negative
        list(s(2L:4L, 0L), s(5L:7L, 0L)),
        lapply(c(10403L, 607L, 20007L, -93L), s, i = 1L)
    )
    before <- rng_seed_mt()
    for (seed in bad) expect_error(rng_draw(seed), msg, fixed = TRUE)
    expect_identical(rng_get_state(), before)
    # the limits of the range and components; kinds 10407 and 107 exist
    ok <- c(list(2^31 - 1, -2^31 + 1, s(c(4L, 7L), c(-210L, -22854L))),
            list(s(c(2L, 3L, 5L, 7L), 0L)), lapply(c(10407L, 107L), s, i = 1L))
    for (seed in ok) expect_silent(rng_draw(seed))
})
# Installs a throwaway package whose .onLoad runs `onload`, as torch 0.17.0
# draws there, first in .libPaths() until the calling test ends
rng_onload_package <- function(pkg = "sitsrngonload",
                               onload = "invisible(stats::runif(1L))",
                               env = parent.frame()) {
    skip_on_cran()
    src <- file.path(tempfile("rngsrc"), pkg)
    lib <- tempfile("rnglib")
    dir.create(file.path(src, "R"), recursive = TRUE)
    dir.create(lib)
    writeLines(c(paste("Package:", pkg), "Version: 0.0.1"),
               file.path(src, "DESCRIPTION"))
    writeLines("", file.path(src, "NAMESPACE"))
    writeLines(paste0(".onLoad <- function(libname, pkgname) {", onload, "}"),
               file.path(src, "R", "zzz.R"))
    system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL",
            "--no-test-load", "-l", shQuote(lib), shQuote(src)),
            stdout = FALSE, stderr = FALSE)
    old_libs <- .libPaths()
    # unload before the library goes
    rng_defer(function() {
        if (pkg %in% loadedNamespaces()) unloadNamespace(pkg)
        .libPaths(old_libs)
        unlink(c(dirname(src), lib), recursive = TRUE)
    }, env)
    skip_if_not(dir.exists(file.path(lib, pkg)), "package not installed")
    .libPaths(c(lib, old_libs))
    pkg
}
test_that("loading a package to check it leaves the R generator as received", {
    pkg <- rng_onload_package()
    before <- rng_seed_mt()
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
    pkg <- rng_onload_package()
    saved <- rng_seed_mt()
    on.exit(assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
    rm(".Random.seed", envir = globalenv())
    sits:::.check_require_packages(pkg)
    expect_true(pkg %in% loadedNamespaces())
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    # instrument: a plain load from the same unseeded start creates the state
    unloadNamespace(pkg)
    loadNamespace(pkg)
    expect_true(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
# Runs `setup`, then `code`, in a fresh R process with the sits under test;
# the code adds its results to `res`. Skips without a functional torch
rng_fresh <- function(code, setup = NULL) {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")
    skip_if_not(torch::torch_is_installed(), "torch is not functional")
    rng_skip_if_not_installed()
    sits_path <- find.package("sits")
    script <- tempfile("rngfresh", fileext = ".R")
    out <- tempfile("rngfresh", fileext = ".rds")
    on.exit(unlink(c(script, out)), add = TRUE)
    writeLines(c(
        "suppressPackageStartupMessages(library(sits))",
        "torch_loaded <- function() 'torch' %in% loadedNamespaces()",
        "res <- list(sits_path = find.package('sits'), torch = torch_loaded())",
        deparse(setup), deparse(substitute(code)),
        sprintf("saveRDS(res, %s)", deparse(out))
    ), script)
    libs <- paste(unique(c(dirname(sits_path), .libPaths())),
                  collapse = .Platform$path.sep)
    status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                      stdout = FALSE, stderr = FALSE,
                      env = paste0("R_LIBS=", shQuote(libs)))
    expect_identical(status, 0L)
    skip_if(!file.exists(out), "the fresh R process wrote no result")
    res <- readRDS(out)
    # each case needs the sits under test and a torch not yet loaded
    expect_identical(normalizePath(res[["sits_path"]]),
                     normalizePath(sits_path))
    expect_false(res[["torch"]])
    res
}
# every 6th row mixes the four classes; no draw builds it
rng_mlp <- quote({
    smp <- samples_modis_ndvi[seq(1L, nrow(samples_modis_ndvi), by = 6L), ]
    point <- sits_select(point_mt_6bands, bands = "NDVI")
    train <- function(seed) sits_train(smp, sits_mlp(epochs = 2L, seed = seed))
    probs_of <- function(model) {
        pred <- sits_classify(point, model, progress = FALSE)
        pred <- pred[["predicted"]][[1L]]
        as.matrix(pred[vapply(pred, is.numeric, logical(1L))])
    }
})
test_that("without seed, the first torch training repeats under set.seed()", {
    res <- rng_fresh(setup = rng_mlp, {
        set.seed(1L)
        res[["first"]] <- probs_of(train(NULL))
        set.seed(1L)
        res[["second"]] <- probs_of(train(NULL))
        set.seed(2L)
        res[["other"]] <- probs_of(train(NULL))
    })
    # instrument: probabilities not all 0 or 1; another seed changes them
    expect_false(all(res[["first"]] %in% c(0, 1)))
    expect_false(isTRUE(all.equal(res[["other"]], res[["second"]])))
    expect_identical(res[["first"]], res[["second"]])
})
test_that("with seed, the first torch training leaves the R generator", {
    res <- rng_fresh(setup = rng_mlp, {
        set.seed(1L)
        res[["before"]] <- .Random.seed
        model <- train(42L)
        # read before sits_classify(), which draws without seed
        res[["after"]] <- .Random.seed
        res[["first"]] <- probs_of(model)
        res[["second"]] <- probs_of(train(42L))
        res[["other"]] <- probs_of(train(43L))
    })
    # instrument: another seed changes the probabilities
    expect_false(all(res[["first"]] %in% c(0, 1)))
    expect_false(isTRUE(all.equal(res[["other"]], res[["first"]])))
    expect_identical(res[["first"]], res[["second"]])
    expect_identical(res[["after"]], res[["before"]])
})
test_that("checking that torch is functional leaves the R generator", {
    res <- rng_fresh({
        set.seed(1L)
        res[["before"]] <- .Random.seed
        res[["functional"]] <- sits:::.torch_is_functional()
        res[["after"]] <- .Random.seed
        res[["torch_after"]] <- torch_loaded()
    })
    expect_true(res[["functional"]])
    expect_true(res[["torch_after"]])
    expect_identical(res[["after"]], res[["before"]])
    # instrument: a plain load of torch moves the generator
    plain <- rng_fresh({
        set.seed(1L)
        res[["before"]] <- .Random.seed
        loadNamespace("torch")
        res[["after"]] <- .Random.seed
    })
    expect_false(identical(plain[["after"]], plain[["before"]]))
})
test_that("with seed, the first torch tuning repeats in a session", {
    res <- rng_fresh({
        tune <- function(seed) {
            hp <- sits_tuning_hparams(
                epochs = 2L, opt_hparams = list(lr = loguniform(0.01, 0.0001))
            )
            tuned <- sits_tuning(samples_modis_ndvi, ml_method = sits_mlp(),
                params = hp, trials = 2L, multicores = 1L, seed = seed)
            as.matrix(tuned[c("accuracy", "kappa")])
        }
        res[["first"]] <- tune(42L)
        res[["torch_after"]] <- torch_loaded()
        res[["second"]] <- tune(42L)
        res[["other"]] <- tune(43L)
    })
    expect_true(res[["torch_after"]])
    # instrument: another seed changes the accuracies
    expect_false(isTRUE(all.equal(res[["other"]], res[["second"]])))
    expect_identical(res[["first"]], res[["second"]])
})
# the class map in extdata; one worker, as a cluster start would load torch
rng_cube <- quote({
    cube <- sits_cube(
        source = "MPC", collection = "SENTINEL-2-L2A", bands = "class",
        data_dir = system.file("extdata/raster/classif", package = "sits"),
        labels = c("1" = "ClearCut_Fire", "2" = "ClearCut_BareSoil",
                   "3" = "ClearCut_Veg", "4" = "Forest"),
        multicores = 1L, progress = FALSE
    )
    res[["torch_cube"]] <- torch_loaded()
    strata <- function(seed) {
        sf::st_coordinates(sits_stratified_sampling(cube,
            samples_per_class = 20L, multicores = 2L, progress = FALSE,
            seed = seed))
    }
})
test_that("with seed, the first stratified sampling leaves the R generator", {
    res <- rng_fresh(setup = rng_cube, {
        set.seed(1L)
        res[["before"]] <- .Random.seed
        res[["first"]] <- strata(42L)
        res[["after"]] <- .Random.seed
        res[["torch_after"]] <- torch_loaded()
        res[["second"]] <- strata(42L)
        res[["before_noseed"]] <- .Random.seed
        invisible(strata(NULL))
        res[["after_noseed"]] <- .Random.seed
    })
    # torch is loaded by the start of the cluster, not by the cube
    expect_false(res[["torch_cube"]])
    expect_true(res[["torch_after"]])
    # instrument: without seed the sampling moves the generator
    expect_false(identical(res[["after_noseed"]], res[["before_noseed"]]))
    expect_identical(res[["first"]], res[["second"]])
    expect_identical(res[["after"]], res[["before"]])
})
test_that("a package that fails to load leaves the R generator as received", {
    pkg <- rng_onload_package("sitsrngfail",
                              "stats::runif(1L); stop('load fails')")
    before <- rng_seed_mt()
    expect_error(sits:::.check_require_packages(pkg),
                 "Please install package(s) sitsrngfail", fixed = TRUE)
    expect_false(pkg %in% loadedNamespaces())
    expect_identical(rng_get_state(), before)
    # instrument: a plain load fails and moves the generator
    set.seed(1L, kind = "Mersenne-Twister")
    expect_error(loadNamespace(pkg), "load fails")
    expect_false(identical(rng_get_state(), before))
})
test_that("classifying with a model that is not torch does not load torch", {
    res <- rng_fresh({
        model <- sits_train(samples_modis_ndvi, sits_rfor(num_trees = 50L))
        res[["torch_train"]] <- torch_loaded()
        point <- sits_select(point_mt_6bands, bands = "NDVI")
        pred <- sits_classify(point, model, multicores = 1L, progress = FALSE)
        res[["classes"]] <- nrow(pred[["predicted"]][[1L]])
        res[["torch_classify"]] <- torch_loaded()
    })
    # the training does not load torch, and the classification ran
    expect_false(res[["torch_train"]])
    expect_identical(res[["classes"]], 17L)
    expect_false(res[["torch_classify"]])
})
test_that("without seed, the first stratified sampling repeats with set.seed", {
    res <- rng_fresh(setup = rng_cube, {
        set.seed(1L)
        res[["first"]] <- strata(NULL)
        res[["torch_after"]] <- torch_loaded()
        set.seed(1L)
        res[["second"]] <- strata(NULL)
        set.seed(2L)
        res[["other"]] <- strata(NULL)
    })
    # torch is loaded by the start of the cluster, not by the cube
    expect_false(res[["torch_cube"]])
    expect_true(res[["torch_after"]])
    # instrument: another seed changes the samples
    expect_false(identical(res[["other"]], res[["second"]]))
    expect_identical(res[["first"]], res[["second"]])
})
