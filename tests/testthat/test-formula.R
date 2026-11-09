# Literal formulas: the oracle for what the formula functions build
.test_formula_linear <- factor(label) ~ NDVI1 + NDVI2 + NDVI3 + NDVI4 +
    NDVI5 + NDVI6 + NDVI7 + NDVI8 + NDVI9 + NDVI10 + NDVI11 + NDVI12
.test_formula_logref <- factor(label) ~ log(NDVI1) + log(NDVI2) +
    log(NDVI3) + log(NDVI4) + log(NDVI5) + log(NDVI6) + log(NDVI7) +
    log(NDVI8) + log(NDVI9) + log(NDVI10) + log(NDVI11) + log(NDVI12)
# 0.618... is inside the NDVI range, so normalisation does not clip it
.test_formula_planted <- 0.6180339887498949
.test_formula_samples <- function() {
    samples <- samples_modis_ndvi
    ts <- samples[["time_series"]][[2L]]
    ts[["NDVI"]][[5L]] <- .test_formula_planted
    samples[["time_series"]][[2L]] <- ts
    samples
}
# Normalised planted value as C_normalize_data_inplace computes it,
# as big-endian bytes, the encoding of serialize()
.test_formula_planted_bytes <- function(ml_model) {
    stats <- environment(ml_model)[["ml_stats"]]
    q02 <- stats[["q02"]][[5L]]
    q98 <- stats[["q98"]][[5L]]
    value <- (.test_formula_planted - q02) * (1.0 / (q98 - q02))
    writeBin(value, raw(), endian = "big")
}
.test_formula_hits <- function(obj, pattern) {
    length(grepRaw(pattern, serialize(obj, NULL), fixed = TRUE, all = TRUE))
}
.test_formula_train <- function(samples, formula) {
    set.seed(42L)
    sits_train(samples, sits_svm(formula = formula))
}
.test_formula_probs <- function(ml_model, band = "NDVI") {
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    sits_bands(point_ndvi) <- band
    point_class <- sits_classify(
        data = point_ndvi, ml_model = ml_model, progress = FALSE
    )
    dplyr::bind_rows(point_class[["predicted"]])
}

test_that("formula functions build literal formulas and write no global", {
    env <- get("sits_env", envir = asNamespace("sits"))
    before <- mget("model_formula", envir = env, ifnotfound = list(NULL))
    pred <- .predictors(samples_modis_ndvi)
    linear <- sits_formula_linear()(pred)
    logref <- sits_formula_logref()(pred)
    some <- sits_formula_linear(predictors_index = 3L:4L)(pred)
    after <- mget("model_formula", envir = env, ifnotfound = list(NULL))
    expect_identical(after, before)
    expect_identical(linear, .test_formula_linear, ignore_formula_env = TRUE)
    expect_identical(logref, .test_formula_logref, ignore_formula_env = TRUE)
    expect_identical(
        some, factor(label) ~ NDVI1 + NDVI2, ignore_formula_env = TRUE
    )
})

test_that("formula functions with NULL index use all feature columns", {
    cases <- list(
        list(
            sits_formula_linear(predictors_index = NULL),
            .test_formula_linear
        ),
        list(
            sits_formula_logref(predictors_index = NULL),
            .test_formula_logref
        )
    )
    purrr::walk(cases, function(case) {
        ml_model <- .test_formula_train(samples_modis_ndvi, case[[1L]])
        svm <- environment(ml_model)[["model"]]
        expect_identical(
            attr(svm[["terms"]], "term.labels"),
            attr(stats::terms(case[[2L]]), "term.labels")
        )
        expect_identical(
            .test_formula_probs(ml_model),
            .test_formula_probs(
                .test_formula_train(samples_modis_ndvi, case[[2L]])
            )
        )
    })
})

test_that("linear formula works with a band name that is not syntactic", {
    # "1NDVI1" starts with a digit, so R parses it only in backquotes
    samples <- samples_modis_ndvi
    sits_bands(samples) <- "1NDVI"
    literal <- factor(label) ~ `1NDVI1` + `1NDVI2` + `1NDVI3` + `1NDVI4` +
        `1NDVI5` + `1NDVI6` + `1NDVI7` + `1NDVI8` + `1NDVI9` + `1NDVI10` +
        `1NDVI11` + `1NDVI12`
    linear <- sits_formula_linear()(.predictors(samples))
    expect_identical(linear, literal, ignore_formula_env = TRUE)
    ml_model <- .test_formula_train(samples, sits_formula_linear())
    expect_identical(
        .test_formula_probs(ml_model, band = "1NDVI"),
        .test_formula_probs(
            .test_formula_train(samples, literal), band = "1NDVI"
        )
    )
})

test_that("formula functions with NULL index drop sample_id and label", {
    pred <- .predictors(samples_modis_ndvi)
    features <- setdiff(names(pred), c("sample_id", "label"))
    # tables differ in which of sample_id and label exist and where
    tables <- list(
        both_first = pred,
        no_sample_id = pred[c("label", features)],
        features_only = pred[features],
        both_last = pred[c(features, "sample_id", "label")]
    )
    cases <- list(
        linear = list(
            sits_formula_linear(predictors_index = NULL),
            .test_formula_linear
        ),
        logref = list(
            sits_formula_logref(predictors_index = NULL),
            .test_formula_logref
        )
    )
    # for loops, not purrr: a failure in one case must not hide the others
    for (table in names(tables)) {
        for (case in names(cases)) {
            label <- paste(case, "on", table)
            formula <- cases[[case]][[1L]](tables[[table]])
            expect_false("label" %in% all.vars(formula[[3L]]), label = label)
            expect_identical(
                formula, cases[[case]][[2L]],
                ignore_formula_env = TRUE, label = label
            )
        }
    }
})

test_that("SVM with formula functions keeps no training data in terms", {
    samples <- .test_formula_samples()
    cases <- list(
        list(sits_formula_linear(), .test_formula_linear),
        list(sits_formula_logref(), .test_formula_logref)
    )
    purrr::walk(cases, function(case) {
        ml_model <- .test_formula_train(samples, case[[1L]])
        # predictions equal those of the literal formula
        expect_identical(
            .test_formula_probs(ml_model),
            .test_formula_probs(.test_formula_train(samples, case[[2L]]))
        )
        # check of the instrument: the predictors normalised by sits hold
        # the value once; built here, as a compacted model keeps no samples
        pattern <- .test_formula_planted_bytes(ml_model)
        feats <- .pred_features_normalize(
            .predictors(samples),
            stats = environment(ml_model)[["ml_stats"]]
        )
        expect_identical(.test_formula_hits(feats, pattern), 1L)
        # only support vectors may hold it: those of the linear formula
        # hold the value itself, those of the log formula its log
        svm <- environment(ml_model)[["model"]]
        expect_identical(
            .test_formula_hits(svm, pattern),
            .test_formula_hits(svm[["SV"]], pattern)
        )
    })
})

test_that("SVM terms do not grow with the number of samples", {
    set.seed(1L)
    idx <- sample(nrow(samples_modis_ndvi), 1200L)
    formulas <- list(sits_formula_linear(), sits_formula_logref())
    purrr::walk(formulas, function(formula) {
        sizes <- purrr::map_int(c(300L, 1200L), function(n) {
            samples <- samples_modis_ndvi[idx[seq_len(n)], ]
            ml_model <- .test_formula_train(samples, formula)
            svm <- environment(ml_model)[["model"]]
            length(serialize(svm[["terms"]], NULL))
        })
        expect_identical(sizes[[1L]], sizes[[2L]])
    })
})
