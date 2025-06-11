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

    purrr::pmap(
        list(locs[["longitude"]], locs[["latitude"]]),
        function(long, lat) {
            dplyr::filter(
                data,
                .data[["longitude"]] == long,
                .data[["latitude"]] == lat
            ) |>
                .plot_ggplot_series() |>
                graphics::plot()
        }
    )
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
