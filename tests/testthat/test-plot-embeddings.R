# Helper: build a synthetic embeddings tibble with the exact structure
# produced by sits_encode() (class "embeddings", single-row time_series
# whose columns are EMB1..EMBn), without training a torch encoder.
.make_embeddings <- function(n_samples = 40L, n_emb = 16L, seed = 42L) {
    set.seed(seed)
    emb_names <- paste0(.conf("embedding_band_prefix"), seq_len(n_emb))
    base <- cerrado_2classes[
        sample(nrow(cerrado_2classes), n_samples),
    ]
    # give each label a distinct mean profile so classes separate
    label_means <- list(
        Cerrado = seq(-1.0, 1.0, length.out = n_emb),
        Pasture = seq(1.0, -1.0, length.out = n_emb)
    )
    emb_matrix <- t(vapply(seq_len(nrow(base)), function(i) {
        lb <- as.character(base[["label"]][[i]])
        label_means[[lb]] + stats::rnorm(n_emb, sd = 0.4)
    }, numeric(n_emb)))
    colnames(emb_matrix) <- emb_names
    enc <- .tibble_embedding(data = base, embeddings = emb_matrix)
    .set_class(enc, "embeddings", class(base))
}

test_that("plot.embeddings defaults to a per-dimension boxplot per label", {
    enc <- .make_embeddings()

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)

    p <- plot(enc)
    # default 'dimensions': a list, one ggplot per label
    expect_type(p, "list")
    expect_false(inherits(p, "ggplot"))
    expect_length(p, length(sits_labels(enc)))
    expect_s3_class(p[[1]], "ggplot")
    # per-dimension view: x axis is the embedding dimension
    expect_equal(p[[1]]$labels$x, "Embedding dimension")
    expect_equal(p[[1]]$labels$y, "Value")
    expect_match(p[[1]]$labels$title, "^Embeddings \\([0-9]+\\) for class ")
    # the geom is a boxplot (no lines implying continuity across dims)
    geoms <- vapply(p[[1]]$layers, function(l) class(l$geom)[[1]], character(1))
    expect_true("GeomBoxplot" %in% geoms)
    expect_false("GeomLine" %in% geoms)
})

test_that("plot.embeddings mode = 'PCA' projects to 2D by label", {
    enc <- .make_embeddings()

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)

    p <- plot(enc, mode = "PCA")
    # PCA returns a single ggplot with one point per embedding
    expect_s3_class(p, "ggplot")
    expect_equal(nrow(p$data), nrow(enc))
    expect_setequal(unique(p$data$label), sits_labels(enc))
    expect_equal(p$labels$title, "Embeddings (PCA projection)")
    # axis labels report the variance explained
    expect_match(p$labels$x, "^PC1 \\([0-9.]+%\\)$")
    expect_match(p$labels$y, "^PC2 \\([0-9.]+%\\)$")
})

test_that("plot.embeddings PCA forwards ... to prcomp (scale.)", {
    enc <- .make_embeddings()

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)

    p_default <- plot(enc, mode = "PCA")
    p_scaled  <- plot(enc, mode = "PCA", scale. = TRUE)
    # scaling changes the variance-explained reported on the axes
    expect_false(identical(p_default$labels$x, p_scaled$labels$x))
})

test_that("plot.embeddings mode = 'tsne' projects with Rtsne", {
    skip_if_not_installed("Rtsne")
    enc <- .make_embeddings()

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)

    set.seed(1)
    p <- plot(enc, mode = "tsne", perplexity = 5L, max_iter = 250L)
    expect_s3_class(p, "ggplot")
    expect_equal(nrow(p$data), nrow(enc))
    expect_equal(p$labels$title, "Embeddings (t-SNE projection)")
    expect_equal(p$labels$x, "t-SNE 1")
})

test_that("plot.embeddings rejects an unknown mode", {
    enc <- .make_embeddings()
    expect_error(plot(enc, mode = "umap"))
})

test_that("plot.embeddings tsne guards the suggested Rtsne package", {
    # emulate Rtsne being unavailable: the tsne branch must error via
    # .check_require_packages before doing any projection work
    enc <- .make_embeddings()

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)

    testthat::local_mocked_bindings(
        .check_require_packages = function(x, ...) {
            stop(paste("Please install package(s)", x))
        }
    )
    expect_error(plot(enc, mode = "tsne"), "Rtsne")
})
