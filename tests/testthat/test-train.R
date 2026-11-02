# A trained model keeps no training data beyond the first sample of each
# label. Each model is compared with a reference: the same model trained with
# the compaction mocked away, as models were built before it.

# Paths of the objects reachable from `x` that hold one of `values` (relative
# tolerance of float32) or `strings`; namespaces are not entered.
.train_paths_with <- function(x, values, strings) {
    seen <- list()
    found <- character(0L)
    visit <- function(x, path) {
        if (is.environment(x)) {
            if (isNamespace(x) || identical(x, globalenv()) ||
                identical(x, baseenv()) || identical(x, emptyenv()) ||
                any(purrr::map_lgl(seen, identical, x))) {
                return(invisible())
            }
            seen[[length(seen) + 1L]] <<- x
            for (name in ls(x, all.names = TRUE)) {
                # active bindings of R6 objects compute values on access
                if (!bindingIsActive(name, x)) {
                    # a binding to a missing argument cannot be read
                    obj <- tryCatch(
                        get(name, envir = x), error = function(e) NULL
                    )
                    visit(obj, paste0(path, "$", name))
                }
            }
            visit(parent.env(x), paste0(path, "<parent>"))
        } else if (is.function(x) && !is.primitive(x)) {
            visit(environment(x), paste0(path, "<env>"))
        } else if (inherits(x, "torch_tensor")) {
            # a tensor read back by unserialize() has no data
            values_x <- tryCatch(
                as.numeric(torch::as_array(x$cpu())),
                error = function(e) NULL
            )
            visit(values_x, paste0(path, "<tensor>"))
        } else if (is.list(x)) {
            x <- unclass(x)
            for (i in seq_along(x)) {
                visit(x[[i]], paste0(path, "[[", i, "]]"))
            }
        } else if (is.double(x)) {
            x <- as.vector(unclass(x))
            near <- purrr::map_lgl(values, function(v) {
                any(abs(x - v) <= 1e-6 * abs(v), na.rm = TRUE)
            })
            if (any(near)) found <<- c(found, path)
        } else if (is.character(x) && any(x %in% strings)) {
            found <<- c(found, path)
        }
        attrs <- attributes(x)
        for (name in setdiff(names(attrs), c("names", "row.names", "class"))) {
            visit(attrs[[name]], paste0(path, "@", name))
        }
        invisible()
    }
    visit(x, "model")
    unique(found)
}
# TRUE if the serialized model holds one of `values` as R writes doubles
# (XDR) or one of `strings`
.train_bytes_with <- function(x, values, strings) {
    bytes <- serialize(x, NULL)
    patterns <- c(
        purrr::map(values, writeBin, con = raw(), endian = "big"),
        purrr::map(strings, charToRaw)
    )
    any(purrr::map_lgl(patterns, function(p) {
        length(grepRaw(p, bytes, fixed = TRUE)) > 0L
    }))
}
# 1000 samples and 100 of them; the second Forest sample of the 1000, which
# is not the first of its label, carries values absent from the data
set.seed(42L)
samples_10n <- samples_modis_ndvi[
    sort(sample(nrow(samples_modis_ndvi), 1000L)),
]
signed <- which(samples_10n[["label"]] == "Forest")[[2L]]
sig_out <- 1234.5678
sig_in <- 0.6180339887498949
sig_lon <- -54.987654321987
sig_cube <- "ZQX_CUBE_4412"
samples_10n[["time_series"]][[signed]][["NDVI"]][[3L]] <- sig_out
samples_10n[["time_series"]][[signed]][["NDVI"]][[5L]] <- sig_in
samples_10n[["longitude"]][[signed]] <- sig_lon
samples_10n[["cube"]][[signed]] <- sig_cube
samples_n <- samples_10n[sort(sample(nrow(samples_10n), 100L)), ]
# the normalised value as torch models and svm keep it (2% and 98% quantiles)
ndvi <- unlist(purrr::map(samples_10n[["time_series"]], "NDVI"))
q02 <- stats::quantile(ndvi, 0.02, names = FALSE)
q98 <- stats::quantile(ndvi, 0.98, names = FALSE)
sig_norm <- (sig_in - q02) / (q98 - q02)
point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
cube <- sits_cube(
    source = "BDC",
    collection = "MOD13Q1-6.1",
    data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
    progress = FALSE
)
# Trains `method` (constructor, arguments) on `samples` with sits_train() or
# sits_pre_train(); `compact = FALSE` mocks the compaction away
.train_model <- function(method, samples, encoder, compact = TRUE) {
    set.seed(1L)
    train_fn <- if (encoder) sits_pre_train else sits_train
    train <- function() {
        train_fn(samples, do.call(method[[1L]], method[[2L]]))
    }
    if (compact) {
        return(train())
    }
    with_mocked_bindings(
        train(),
        .ml_model_compact = function(model) model
    )
}
# Checks a trained model and a model saved before the compaction and
# compacted after reading, against the reference: same output of
# `predict_fn`, no value of the signed sample, and for models of fixed size,
# the same size on 10 times the samples
.train_check <- function(method, name, predict_fn, encoder = FALSE) {
    reference <- .train_model(method, samples_10n, encoder, compact = FALSE)
    expected <- predict_fn(reference)
    # svm keeps its support vectors, which are normalised samples
    values <- c(sig_out, sig_in, sig_lon)
    if (name != "svm") values <- c(values, sig_norm)
    # the walk finds the signed sample in the reference
    expect_true(length(.train_paths_with(reference, values, sig_cube)) > 0L)
    expect_true(.train_bytes_with(reference, values, sig_cube))
    saved <- unserialize(serialize(reference, NULL))
    models <- list(
        train = .train_model(method, samples_10n, encoder),
        saved = .ml_model_compact(saved)
    )
    expect_identical(predict_fn(models[["saved"]]), expected)
    for (model in models) {
        expect_equal(predict_fn(model), expected)
        expect_equal(class(model), class(reference))
        expect_equal(sits_bands(model), sits_bands(samples_10n))
        expect_equal(sits_timeline(model), sits_timeline(samples_10n))
        expect_equal(.train_paths_with(model, values, sig_cube), character(0L))
        expect_false(.train_bytes_with(model, values, sig_cube))
        # one sample of each label
        expect_equal(
            .ml_samples(model)[["label"]],
            unique(samples_10n[["label"]])
        )
    }
    if (!encoder) {
        expect_equal(sits_labels(models[["train"]]), sits_labels(samples_10n))
    }
    if (method[[4L]]) {
        size_10n <- length(serialize(models[["train"]], NULL))
        model_n <- .train_model(method, samples_n, encoder)
        size_n <- length(serialize(model_n, NULL))
        expect_lt(abs(size_10n - size_n), 0.01 * size_n)
    }
}
# constructor, its arguments, the package it needs, whether the fitted model
# has a size that does not depend on the number of samples (trees and
# support vectors grow with them), and whether the cube is classified too
train_methods <- list(
    rfor = list(sits_rfor, list(num_trees = 30L), "randomForest", FALSE, TRUE),
    svm = list(sits_svm, list(), "e1071", FALSE, TRUE),
    xgboost = list(sits_xgboost, list(nrounds = 10L), "xgboost", FALSE, TRUE),
    lightgbm = list(
        sits_lightgbm, list(num_iterations = 10L), "lightgbm", FALSE, FALSE
    ),
    mlp = list(sits_mlp, list(epochs = 2L, seed = 1L), "torch", TRUE, TRUE),
    tempcnn = list(
        sits_tempcnn, list(epochs = 2L, seed = 1L), "torch", TRUE, FALSE
    ),
    lighttae = list(
        sits_lighttae, list(epochs = 2L, seed = 1L), "torch", TRUE, FALSE
    ),
    tae = list(sits_tae, list(epochs = 2L, seed = 1L), "torch", TRUE, FALSE),
    resnet = list(
        sits_resnet, list(epochs = 2L, seed = 1L), "torch", TRUE, FALSE
    ),
    lstm_fcn = list(
        sits_lstm_fcn, list(epochs = 2L, seed = 1L), "torch", TRUE, FALSE
    )
)
for (name in names(train_methods)) {
    test_that(paste("a trained model keeps no training data:", name), {
        method <- train_methods[[name]]
        skip_if_not_installed(method[[3L]])
        # probabilities on a series of 17 years, split by the reference
        # dates of the model, and on the cube for some models
        predict_fn <- function(m) {
            probs <- sits_classify(point_ndvi, m, progress = FALSE)
            # sits draws the class of tied probabilities, so compare these
            predicted <- probs[["predicted"]][[1L]]
            predicted <- predicted[setdiff(names(predicted), "class")]
            if (!method[[5L]]) {
                return(predicted)
            }
            output_dir <- tempfile("train-")
            dir.create(output_dir)
            on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
            probs_cube <- sits_classify(
                cube, m, output_dir = output_dir, progress = FALSE
            )
            list(
                predicted,
                .raster_open_rast(.tile_path(probs_cube))[]
            )
        }
        .train_check(method, name, predict_fn)
    })
}
ssl_args <- list(
    embedding_dim = 8L, proj_dim = 16L, epochs = 2L, batch_size = 32L
)
encoder_methods <- list(
    ssl_barlow_twins = list(sits_ssl_barlow_twins, ssl_args, "torch", TRUE),
    ssl_vicreg = list(sits_ssl_vicreg, ssl_args, "torch", TRUE),
    ssl_lejepa = list(sits_ssl_lejepa, ssl_args, "torch", TRUE),
    ssl_mae = list(
        sits_ssl_mae, ssl_args[c("embedding_dim", "epochs", "batch_size")],
        "torch", TRUE
    ),
    barlow_twins = list(sits_barlow_twins, ssl_args, "torch", TRUE),
    contrastive = list(sits_contrastive_learning, ssl_args, "torch", TRUE)
)
for (name in names(encoder_methods)) {
    test_that(paste("a trained encoder keeps no training data:", name), {
        method <- encoder_methods[[name]]
        skip_if_not_installed(method[[3L]])
        # embeddings of a series of 17 years
        predict_fn <- function(m) {
            sits_encode(point_ndvi, m, progress = FALSE)
        }
        suppressWarnings(.train_check(method, name, predict_fn, TRUE))
    })
}
test_that("the compaction leaves the frame of the training running", {
    skip_if_not_installed("torch")
    record <- new.env()
    # a training function with an exit handler that reads a local, and a
    # module whose class is defined in its frame
    train <- function() {
        restore <- function() assign("value", "restored", envir = record)
        on.exit(restore(), add = TRUE)
        n_out <- 2L
        net <- torch::nn_module(
            initialize = function() self$fc <- torch::nn_linear(1L, n_out),
            forward = function(x) self$fc(x)
        )
        torch_model <- net()
        predict_fun <- function(values) torch_model(values)
        .ml_model_compact(predict_fun)
    }
    model <- train()
    expect_equal(record[["value"]], "restored")
    expect_equal(dim(model(torch::torch_ones(3L, 1L))), c(3L, 2L))
})
test_that("the compaction leaves the methods of users unchanged", {
    skip_if_not_installed("torch")
    encoder <- sits_tempcnn(epochs = 1L, seed = 1L)
    method <- sits_ssl_barlow_twins(
        embedding_dim = 8L, proj_dim = 16L, encoder_model = encoder,
        epochs = 1L, batch_size = 32L
    )
    n_names <- function(f) length(ls(environment(f), all.names = TRUE))
    n_encoder <- n_names(encoder)
    n_method <- n_names(method)
    # the same method object trained twice, as in a loop or a tuning
    for (i in 1:2) {
        model <- suppressWarnings(sits_pre_train(samples_n, method))
        expect_s3_class(model, "sits_model")
        model <- sits_train(samples_n, encoder)
        expect_s3_class(model, "sits_model")
    }
    expect_equal(n_names(encoder), n_encoder)
    expect_equal(n_names(method), n_method)
})
