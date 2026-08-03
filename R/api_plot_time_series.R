#' @title Plot all intervals of one time series for the same lat/long together
#' @name .plot_allyears
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description For each lat/long location in the data, join temporal
#' instances of the same place together for plotting.
#' @param data    One or more time series.
#' @return        A plot object produced by the ggplot2 package
#'                showing an individual time series.
#'
.plot_allyears <- function(data) {
    locs <- dplyr::distinct(data, .data[["longitude"]], .data[["latitude"]])

    p <- purrr::pmap(
        list(locs[["longitude"]], locs[["latitude"]]),
        function(long, lat) {
            dplyr::filter(
                data,
                .data[["longitude"]] == long,
                .data[["latitude"]] == lat
            ) |>
                .plot_ggplot_series()
        }
    )

    graphics::plot(p[[1]])

    return(p)
}

#' @title Plot a set of time series for the same spatiotemporal reference
#' @name .plot_together
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Plots all time series for the same label together.
#' This function is useful to find out the spread of the values of
#' the time series for a given label.
#'
#' @param    data    A sits tibble with the list of time series to be plotted.
#' @return           A set of plots produced by the ggplot2 package
#'                   each containing all time series associated to one band
#'                   and one label.
.plot_together <- function(data) {
    # create a data frame with the median, and 25% and 75% quantiles
    create_iqr <- function(melted) {
        qts <- melted |>
            dplyr::group_by(.data[["Index"]]) |>
            dplyr::summarise(
                med  = stats::median(.data[["value"]]),
                qt25 = stats::quantile(.data[["value"]], 0.25),
                qt75 = stats::quantile(.data[["value"]], 0.75)
            )
        qts
    }
    # this function plots the values of all time series together (for one band)
    plot_samples <- function(melted, qts, band, label, number) {
        # make the plot title
        title <- paste0(
            "Samples (", number, ") for class ",
            label, " in band = ", band
        )
        # plot all data together
        graphics::plot(.plot_ggplot_together(melted, qts, title))
    }

    # how many different labels are there?
    labels <- .samples_labels(data)

    labels |>
        purrr::map(function(l) {
            lb <- as.character(l)
            # filter only those rows with the same label
            data2 <- dplyr::filter(data, .data[["label"]] == lb)
            # how many time series are to be plotted?
            number <- nrow(data2)
            # what are the band names?
            bands <- .samples_bands(data2, include_base = FALSE)
            # what are the reference dates?
            ref_dates <- .samples_timeline(data2)
            # align all time series to the same dates
            data2 <- .tibble_align_dates(data2, ref_dates)

            band_plots <- bands |>
                purrr::map(function(band) {
                    # select the band to be shown
                    band_tb <- .samples_select_bands(data2, band)

                    melted <- band_tb |>
                        dplyr::select("time_series") |>
                        dplyr::mutate(variable = seq_len(dplyr::n())) |>
                        tidyr::unnest(cols = "time_series")
                    names(melted) <- c("Index", "value", "variable")

                    qts <- create_iqr(melted)
                    # plot the time series together
                    # (highlighting the median and quartiles 25% and 75%)
                    plot_samples(melted, qts, band, lb, number)
                })
            band_plots
        })
}

#' @title Plot a set of embeddings
#' @name .plot_embeddings
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Plots a set of embeddings, either as a 2D projection
#' (\code{"PCA"}/\code{"tsne"}), where each sample's embedding vector is a
#' point colored by label, or as a per-dimension cloud
#' (\code{"dimensions"}) with median and quartile ranges. Embedding
#' dimensions are unordered and not individually interpretable, so the
#' meaningful signal is the arrangement of samples in the latent space
#' rather than the value along any single dimension.
#'
#' @param    data     An embeddings tibble with the embeddings to be plotted.
#' @param    mode     Plot mode: \code{"PCA"}, \code{"tsne"} or
#'                    \code{"dimensions"}.
#' @param    palette  HCL palette for labels absent from the color table.
#' @param    ...      Extra arguments passed to the projection function.
#' @return            For \code{"PCA"}/\code{"tsne"}, a ggplot2 plot object;
#'                    for \code{"dimensions"}, a list of ggplot2 plots (one
#'                    per label).
.plot_embeddings <- function(data, mode, palette, ...) {
    # per-dimension cloud view (median + quartile ranges, one plot per label)
    if (mode == "dimensions") {
        return(.plot_embeddings_dimensions(data))
    }
    # t-SNE relies on the suggested Rtsne package - fail fast if missing
    if (mode == "tsne") {
        .check_require_packages("Rtsne")
    }
    # embedding dimensions (the columns of each single-row time series)
    dims <- .samples_bands(data, include_base = FALSE)
    # build the samples-by-dimensions matrix (one row per embedding)
    emb <- purrr::map_dfr(
        data[["time_series"]],
        function(ts) ts[dims]
    )
    emb_mat <- as.matrix(emb)
    # labels aligned to the rows of the matrix
    labels <- as.character(data[["label"]])
    # project the embeddings to 2D
    proj <- .plot_embeddings_project(emb_mat, mode = mode, ...)
    # assemble the plotting data frame
    plot_df <- tibble::tibble(
        x = proj[["coords"]][, 1L],
        y = proj[["coords"]][, 2L],
        label = labels
    )
    # colors for the labels (uses the sits color table when available)
    colors <- .colors_get(
        labels = sort(unique(labels)),
        palette = palette,
        rev = TRUE
    )
    # plot
    p <- .plot_ggplot_embeddings(
        plot_df = plot_df,
        colors = colors,
        axis_labels = proj[["axis_labels"]],
        plot_title = proj[["title"]]
    )
    graphics::plot(p)
    p
}
#' @title Plot embeddings as a per-dimension cloud (one plot per label)
#' @name .plot_embeddings_dimensions
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description For each label, plots all embeddings together with the
#' embedding dimension on the x axis and the embedding value on the y axis,
#' highlighting the median and the 25% and 75% quantiles. Note the x axis
#' order is the (arbitrary) dimension order and does not represent a
#' continuous quantity.
#'
#' @param    data    An embeddings tibble with the embeddings to be plotted.
#' @return           A list of plots produced by the ggplot2 package, one
#'                   per label.
.plot_embeddings_dimensions <- function(data) {
    # the embedding dimensions play the role of the "bands"
    dims <- .samples_bands(data, include_base = FALSE)
    # how many different labels are there?
    labels <- .samples_labels(data)

    labels |>
        purrr::map(function(l) {
            lb <- as.character(l)
            # filter only those rows with the same label
            data2 <- dplyr::filter(data, .data[["label"]] == lb)
            # how many embeddings are to be plotted?
            number <- nrow(data2)
            # melt the embeddings into long format:
            # one row per (sample, dimension) with the embedding value
            melted <- data2 |>
                dplyr::select("time_series") |>
                tidyr::unnest(cols = "time_series") |>
                dplyr::select(-"Index") |>
                tidyr::pivot_longer(
                    cols = tidyr::all_of(dims),
                    names_to = "dimension",
                    values_to = "value"
                ) |>
                dplyr::mutate(
                    dimension = factor(.data[["dimension"]], levels = dims)
                )
            # make the plot title
            title <- paste0("Embeddings (", number, ") for class ", lb)
            # plot a boxplot per dimension (box = IQR, line = median);
            # the discrete x axis avoids implying continuity across dims
            p <- .plot_ggplot_dimensions(melted, title)
            graphics::plot(p)
            p
        })
}
#' @title Project an embeddings matrix to two dimensions
#' @name .plot_embeddings_project
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Reduces an embeddings matrix to two dimensions using PCA
#' (base \pkg{stats}) or t-SNE (package \pkg{Rtsne}).
#'
#' @param    emb_mat  Numeric matrix (samples by embedding dimensions).
#' @param    mode     Projection mode: \code{"PCA"} or \code{"tsne"}.
#' @param    ...      Extra arguments passed to the projection function.
#' @return            A list with \code{coords} (a 2-column matrix),
#'                    \code{axis_labels} and \code{title}.
.plot_embeddings_project <- function(emb_mat, mode = "PCA", ...) {
    if (mode == "tsne") {
        # Rtsne availability is checked by the caller (.plot_embeddings)
        dots <- list(...)
        # t-SNE requires perplexity < (n - 1) / 3; pick a safe default
        n <- nrow(emb_mat)
        perplexity <- .default(
            dots[["perplexity"]],
            max(1.0, min(30.0, floor((n - 1L) / 3.0)))
        )
        dots[["perplexity"]] <- perplexity
        tsne <- do.call(Rtsne::Rtsne, c(
            list(
                X = emb_mat,
                dims = 2L,
                check_duplicates = FALSE
            ),
            dots
        ))
        list(
            coords = tsne[["Y"]],
            axis_labels = c("t-SNE 1", "t-SNE 2"),
            title = "Embeddings (t-SNE projection)"
        )
    } else {
        # PCA - center the columns; embeddings share a common scale.
        # Extra args in ... (e.g. scale.) override these defaults and are
        # forwarded to stats::prcomp, keeping ... consistent across modes.
        args <- utils::modifyList(
            list(x = emb_mat, center = TRUE, scale. = FALSE),
            list(...)
        )
        pca <- do.call(stats::prcomp, args)
        # percentage of variance explained by the first two components
        var_pct <- (pca[["sdev"]]^2L) / sum(pca[["sdev"]]^2L) * 100.0
        list(
            coords = pca[["x"]][, 1L:2L, drop = FALSE],
            axis_labels = c(
                sprintf("PC1 (%.1f%%)", var_pct[[1L]]),
                sprintf("PC2 (%.1f%%)", var_pct[[2L]])
            ),
            title = "Embeddings (PCA projection)"
        )
    }
}
#' @title Plot projected embeddings using ggplot
#' @name .plot_ggplot_embeddings
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Plots embeddings projected to 2D as points colored by
#' label.
#'
#' @param plot_df      tibble with columns \code{x}, \code{y}, \code{label}.
#' @param colors       named vector of label colours.
#' @param axis_labels  length-2 character vector with x/y axis labels.
#' @param plot_title   title for the plot.
#' @return             A plot object produced by the ggplot2 package.
.plot_ggplot_embeddings <- function(plot_df, colors, axis_labels, plot_title) {
    ggplot2::ggplot(plot_df, ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]],
        color = .data[["label"]]
    )) +
        ggplot2::geom_point(alpha = 0.7, size = 2L) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::labs(
            title = plot_title,
            x = axis_labels[[1L]],
            y = axis_labels[[2L]],
            color = "Label"
        ) +
        ggplot2::theme_bw()
}
#' @title Plot embeddings as a per-dimension boxplot using ggplot
#' @name .plot_ggplot_dimensions
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Plots a boxplot of the embedding values for each dimension.
#' The box shows the interquartile range (25\%--75\%) and the median, so
#' the per-dimension distribution is summarised without connecting the
#' dimensions, which are unordered and do not form a continuous axis.
#'
#' @param melted         tibble with the embeddings (already melted), with
#'                       columns \code{dimension} and \code{value}.
#' @param plot_title     title for the plot.
#' @return               A plot object produced by the ggplot2 package
#'                       showing all embeddings for one label.
.plot_ggplot_dimensions <- function(melted, plot_title) {
    ggplot2::ggplot(data = melted, ggplot2::aes(
        x = .data[["dimension"]],
        y = .data[["value"]]
    )) +
        ggplot2::geom_boxplot(
            fill = "#819BB1",
            colour = "#4C5B6A",
            outlier.colour = "#B16240",
            outlier.alpha = 0.5,
            linewidth = 0.4
        ) +
        ggplot2::labs(
            title = plot_title,
            x = "Embedding dimension",
            y = "Value"
        ) +
        ggplot2::theme_bw()
}
#' @title Plot one time series using ggplot
#' @name .plot_ggplot_series
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Plots a set of time series using ggplot. This function is used
#' for showing the same lat/long location in a series of time steps.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            A plot object produced by the ggplot2 package showing
#'                    one time series.
.plot_ggplot_series <- function(row) {
    # Are there NAs in the data?
    if (anyNA(row[["time_series"]][[1L]])) {
        .plot_ggplot_series_na(row)
    } else {
        .plot_ggplot_series_no_na(row)
    }
}
#' @title Plot one time series using ggplot (no NAs present)
#' @name .plot_ggplot_series_no_na
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Plots a set of time series using ggplot in the case the series
#'              has no NA values.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            A plot object produced by the ggplot2 package where the
#'                    the time series has no NA values.
#'
.plot_ggplot_series_no_na <- function(row) {
    # create the plot title
    plot_title <- .plot_title(
        row[["latitude"]],
        row[["longitude"]],
        row[["label"]]
    )
    # extract the time series
    data_ts <- dplyr::bind_rows(row[["time_series"]])
    # melt the data into long format
    melted_ts <- data_ts |>
        tidyr::pivot_longer(cols = -"Index", names_to = "variable") |>
        as.data.frame()
    # plot the data with ggplot
    ggplot2::ggplot(melted_ts, ggplot2::aes(
        x = .data[["Index"]],
        y = .data[["value"]],
        group = .data[["variable"]]
    )) +
        ggplot2::geom_line(ggplot2::aes(color = .data[["variable"]])) +
        ggplot2::labs(title = plot_title)
}
#' @title Plot one time series with NAs using ggplot
#' @name .plot_ggplot_series_na
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Plots a set of time series using ggplot, showing where NAs are.
#'
#' @param row         row of a sits tibble with the time series to be plotted.
#' @return            A plot object produced by the ggplot2 package
#'                    which shows the NA values of a time series.
.plot_ggplot_series_na <- function(row) {
    # verifies if tidyr package is installed
    .check_require_packages("tidyr")

    # define a function to replace the NAs for unique values
    replace_na <- function(x) {
        x[is.na(x)] <- -10000.0
        x[x != -10000.0] <- NA
        x[x == -10000.0] <- 1.0
        x
    }
    # create the plot title
    plot_title <- .plot_title(
        row[["latitude"]],
        row[["longitude"]],
        row[["label"]]
    )
    # include a new band in the data to show the NAs
    data <- row[["time_series"]][[1L]]
    data_x1 <- dplyr::select_if(data, function(x) anyNA(x))
    data_x1 <- data_x1[, 1L]
    colnames(data_x1) <- "X1"
    data_x1 <- dplyr::transmute(data_x1, cld = replace_na(.data[["X1"]]))
    data <- dplyr::bind_cols(data, data_x1)

    # prepare tibble to ggplot (fortify)
    ts1 <- tidyr::pivot_longer(data, -"Index")
    ggplot2::ggplot(data = dplyr::filter(ts1, .data[["name"]] != "cld")) +
        ggplot2::geom_col(
            ggplot2::aes(
                x = .data[["Index"]],
                y = .data[["value"]]
            ),
            fill = "sienna",
            alpha = 0.3,
            data = dplyr::filter(
                ts1,
                .data[["name"]] == "cld",
                !is.na(.data[["value"]])
            )
        ) +
        ggplot2::geom_line(ggplot2::aes(
            x = .data[["Index"]],
            y = .data[["value"]],
            color = .data[["name"]]
        )) +
        ggplot2::geom_point(ggplot2::aes(
            x = .data[["Index"]],
            y = .data[["value"]],
            color = .data[["name"]]
        )) +
        ggplot2::labs(title = plot_title)
}

#' @title Plot many time series together using ggplot
#' @name .plot_ggplot_together
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Plots a set of  time series together.
#'
#' @param melted         tibble with the time series (already melted).
#' @param means          means and std deviations of the time series.
#' @param plot_title     title for the plot.
#' @return               A plot object produced by the ggplot2 package
#'                       each time series associated to one band
#'                       and one label.
#'
.plot_ggplot_together <- function(melted, means, plot_title) {
    ggplot2::ggplot(data = melted, ggplot2::aes(
        x = .data[["Index"]],
        y = .data[["value"]],
        group = .data[["variable"]]
    )) +
        ggplot2::geom_line(colour = "#819BB1", alpha = 0.5) +
        ggplot2::labs(title = plot_title) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = .data[["Index"]], y = .data[["med"]]),
            colour = "#B16240", linewidth = 2L, inherit.aes = FALSE
        ) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = .data[["Index"]], y = .data[["qt25"]]),
            colour = "#B19540", linewidth = 1L, inherit.aes = FALSE
        ) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(x = .data[["Index"]], y = .data[["qt75"]]),
            colour = "#B19540", linewidth = 1L, inherit.aes = FALSE
        )
}

#' @title Create a plot title to use with ggplot
#' @name .plot_title
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Creates a plot title from row information.
#'
#' @param latitude   latitude of the location to be plotted.
#' @param longitude  longitude of the location to be plotted.
#' @param label      label of the location to be plotted.
#' @return           title to be used in the plot.
.plot_title <- function(latitude, longitude, label) {
    paste0(
        "location (",
        signif(latitude, digits = 4L), ", ",
        signif(longitude, digits = 4L), ") - ",
        label
    )
}
