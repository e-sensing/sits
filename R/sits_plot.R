#' @title  Plot a set of satellite image time series
#' @name   sits_plot
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Given a SITS tibble with a set of time series, plot them.
#' If the user does not specify the type of plot (parameter "type"),
#' the sits_plot function makes an educated guess of what plot is required,
#' based on the input data. If the input data has less than 30 samples, it
#' will default to "all years". If there is only one sample per class, it will
#' default to "patterns". If there are more than 30 samples, it will default to
#' "together".
#'
#'
#' @param  data.tb       A SITS tibble with the list of time series to be plotted
#' @param  band          The band used for visualisation (optional for sits_plot_classification)
#' @param  type          Type of plot to be generated
#' \itemize{
##'  \item{"all years"}{Plot all samples from the same location together}
##'  \item{"one by one"}{Plot each time series separately}
##'  \item{"patterns"}{Plot the patterns for a given set of classes}
##'  \item{"classification"}{Plot the results of a classification}
##'  \item{"dendogram"}{Plot a cluster object}
##'  \item{"together"}{Plot all samples of the same band and label together}
##' }
#' @param  cluster_obj   Useful for plotting a dendrogram.
#' @param  cutree_height A dashed horizontal line to be drawed indicating the height of dendrogram cutting.
#' @param  colors       Color pallete to be used (based on Color Brewer - default is "Dark2")
#' @return data.tb      Input SITS table (useful for chaining functions)
#' @export
#
sits_plot <- function (data.tb, band = NULL, cluster_obj = NULL, cutree_height = NULL,
                       type = "allyears", colors = "Dark2") {

    # check the input exists
    .sits_test_tibble(data.tb)
    # is there only one sample per label? Plot patterns!
    if (max (sits_labels(data.tb)$count) == 1 && nrow(data.tb) > 1)
        type <- "patterns"
    # Both data.tb and patterns.tb exist? Plot classification!
    if ("predicted" %in% names (data.tb))
        type <- "classification"
    # Is there a "cluster_obj"? Plot dendogram!
    if (!purrr::is_null(cluster_obj))
        type <- "dendogram"
    # Are there more than 50 samples? Plot them together!
    if (nrow(data.tb) > 30)
        type <- "together"

    switch(type,
           "allyears"       = sits_plot_allyears (data.tb, colors),
           "one_by_one"     = sits_plot_one_by_one (data.tb, colors),
           "together"       = sits_plot_together (data.tb, colors),
           "patterns"       = sits_plot_patterns (data.tb),
           "classification" = sits_plot_classification (data.tb, band = band),
           "dendrogram"     = sits_plot_dendrogram (data.tb, cluster_obj, cutree_height = NULL,
                                                    colors = colors),
           "alignments"     = .sits_plot_alignments (data.tb),
           message (paste ("sits_plot: valid plot types are allyears, one_by_one,
                           together, patterns, classification, dendrogram, alignments", "\n", sep = ""))
           )

    # return the original SITS table - useful for chaining
    return (invisible (data.tb))
}

#' @title Plot all time intervals of one time series for the same lat/long together
#' @name sits_plot_allyears
#'
#' @description for each lat/long location in the data, join temporal
#' instances of the same place together for plotting
#' @param data.tb one or more time series (stored in a SITS tibble)
#' @param colors  the color pallete to be used (default is "Dark2")
#' @export
sits_plot_allyears <- function (data.tb, colors) {
     locs <- dplyr::distinct (data.tb, longitude, latitude)
     locs %>%
          purrrlyr::by_row(function (r){
               long = as.double (r$longitude)
               lat  = as.double (r$latitude)
               # filter only those rows with the same label
               data2.tb <- dplyr::filter (data.tb, longitude == long, latitude == lat)
               # use ggplot to plot the time series together
               data2.tb %>%
                    .sits_ggplot_series(colors) %>%
                    graphics::plot()
          })
}

#' @title Plot classification results
#' @name sits_plot_classification
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @description   Plots the classification results (code reused from the dtwSat package by Victor Maus)
#' @param data.tb      A SITS tibble with one or more time series that have been classified
#' @param band         Band for plotting the classification
#' @export
#'
sits_plot_classification <- function (data.tb, band = NULL) {

    if (purrr::is_null (band))
        band <- sits_bands(data.tb)[1]

    # prepare a data frame for plotting

    #get the labels
    labels <- sits_labels (data.tb)$label

    # put the time series in the data frame
    data.tb %>%
        purrrlyr::by_row(function (r){
            #lb = as.character (r$label)
            lb = .sits_plot_title(r)
            # extract the time series
            ts <- r$time_series[[1]]
            # convert to data frame
            df.x <- data.frame (Time = ts$Index, ts[,band], Series = as.factor(lb) )
            # melt the time series data for plotting
            df.x <- reshape2::melt (df.x, id.vars = c("Time", "Series"))
            # define a nice set of breaks for value plotting
            y.labels = scales::pretty_breaks()(range(df.x$value, na.rm = TRUE))
            y.breaks = y.labels

            # get the predicted values as a tibble

            pred <- r$predicted[[1]]
            df.pol <- data.frame()

            # create a data frame with the predicted values and time intervals
            i <- 1
            pred %>%
                purrrlyr::by_row(function (p){
                    best_class <- as.character(p$class)

                    df.p <- data.frame(
                        Time  = c(lubridate::as_date(p$from), lubridate::as_date(p$to),
                                  lubridate::as_date(p$to), lubridate::as_date(p$from)),
                        Group = rep(i, 4),
                        Class = rep(best_class, 4),
                        value = rep(range(y.breaks, na.rm = TRUE), each = 2)
                    )
                    i <<- i + 1
                    df.pol <<- rbind(df.pol, df.p)
                })
            df.pol$Group  <-  factor(df.pol$Group)
            df.pol$Class  <-  factor(df.pol$Class)
            df.pol$Series <-  rep(lb, length(df.pol$Time))


            I = min(df.pol$Time, na.rm = TRUE)-30 <= df.x$Time &
                df.x$Time <= max(df.pol$Time, na.rm = TRUE)+30

            df.x = df.x[I,,drop=FALSE]

            gp <-  ggplot2::ggplot() +
                ggplot2::facet_wrap(~Series, scales = "free_x", ncol=1) +
                ggplot2::geom_polygon(data=df.pol, ggplot2::aes_string(x='Time', y='value', group='Group', fill='Class'), alpha=.7) +
                ggplot2::scale_fill_brewer(palette="Set3") +
                ggplot2::geom_line(data=df.x, ggplot2::aes_string(x='Time', y='value', colour='variable')) +
                ggplot2::scale_y_continuous(expand = c(0, 0), breaks=y.breaks, labels=y.labels) +
                ggplot2::scale_x_date(breaks=ggplot2::waiver(), labels=ggplot2::waiver()) +
                ggplot2::theme(legend.position = "bottom") +
                ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
                ggplot2::ylab("Value") +
                ggplot2::xlab("Time")

            graphics::plot(gp)
        })



    return (invisible(data.tb))
}

#' @title Plot a dendrogram
#' @name sits_plot_dendrogram
#'
#' @description Plot an enhanced dendrogram based on a cluster object usually found in `.sits_last_cluster`
#'
#' @param data.tb       SITS tibble to extract labels
#' @param cluster_obj   cluster object. Usually stored by `sits_cluster` function in `.sits_last_object`
#' @param cutree_height A dashed horizontal line to be drawed indicating the height of dendrogram cutting.
#' @param colors        a color scheme as showed in `sits_color_name` function
#' @export
sits_plot_dendrogram <- function(data.tb,
                                 cluster_obj,
                                 cutree_height = NULL,
                                 colors = "RdYlGn"){

    # ensures that a cluster object is informed or exists in .sits_last_cluster global variable.
    ensurer::ensure_that(cluster_obj, !is.null(.), err_desc = "plot_dendrogram: no valid `cluster_obj` informed or found in `.sits_last_cluster`.")

    # get unique labels
    data_labels <- data.tb$label
    uniq_labels <- base::unique(data_labels)

    # warns if the number of available colors is insufficient to all labels
    if (length(uniq_labels) > (length(.sits_brewerRGB[[.sits_color_name(colors)]]) - 1))
        message("sits_plot_dendrogram: The number of labels is greater than the number of available colors.")

    # extract the dendrogram object
    hclust_cl <- methods::S3Part(cluster_obj, strictS3 = TRUE)
    dendrogram <- hclust_cl %>%
        stats::as.dendrogram()

    # prepare labels color vector
    cols <- character(length(data_labels))
    cols[] <- grDevices::rgb(0/255,   0/255,   0/255,   0/255)
    i <- 1
    seq(uniq_labels) %>%
        purrr::map(function (i){
            cols[data_labels[hclust_cl$order] == uniq_labels[i]] <<- .sits_brewerRGB[[.sits_color_name(colors)]][[length(uniq_labels)]][[i]]
            i <<- i + 1
        })

    # plot the dendrogram
    dendrogram %>%
        dendextend::set("labels", character(length = length(data_labels))) %>%
        dendextend::set("branches_k_color", value = cols, k = length(data_labels)) %>%
        graphics::plot(xlab = "Clusters", ylab = paste(tools::file_path_sans_ext(cluster_obj@method),
                                                       "linkage distance"))


    # plot cutree line
    if (!is.null(cutree_height)) graphics::abline(h = cutree_height, lty = 2)

    # plot legend
    graphics::legend("topright",
                     fill = as.character(.sits_brewerRGB[[.sits_color_name(colors)]][[length(uniq_labels)]]),
                     legend = uniq_labels)
}

#' @title Plot  time intervals of one time series separately (one-by-one)
#' @name sits_plot_one_by_one
#'
#' @description plots each row of a data set separately
#' @param data.tb one or more time series (stored in a SITS tibble)
#' @param colors  the color pallete to be used (default is "Dark2")
#' @export
sits_plot_one_by_one <- function (data.tb, colors){
    data.tb %>%
        purrrlyr::by_row( function (r){
            .sits_ggplot_series (r,colors) %>%
                graphics::plot()
        })

}
#' @title Plot classification patterns
#' @name sits_plot_patterns
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @description plots the patterns to be used for classification
#'              this code is reused from the dtwSat package by Victor Maus
#' @param data.tb one or more time series containing patterns (stored in a SITS tibble)
#' @export
sits_plot_patterns <- function (data.tb) {

    # prepare a data frame for plotting
    plot.df <- data.frame()

    # put the time series in the data frame
    data.tb %>%
        purrrlyr::by_row(function (r){
            lb = as.character (r$label)
            # extract the time series and convert
            ts <- r$time_series[[1]]
            df <- data.frame (Time = ts$Index, ts[-1], Pattern = lb)
            plot.df <<- rbind (plot.df, df)
        })

    plot.df <- reshape2::melt (plot.df, id.vars = c("Time", "Pattern"))

    # Plot temporal patterns
    gp <-  ggplot2::ggplot(plot.df, ggplot2::aes_string(x="Time", y="value", colour="variable") ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~Pattern) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_x_date(labels = scales::date_format("%b")) +
        ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
        ggplot2::ylab("Value")

    graphics::plot (gp)

    return (invisible(data.tb))
}

#' @title Plot a set of time series for the same spatio-temporal reference
#'
#' @name sits_plot_together
#'
#' @description plots all time series for the same label together
#' This function is useful to find out the spread of the values of the time serie
#' for a given label
#'
#' @param    data.tb    tibble - a SITS table with the list of time series to be plotted
#' @param    colors     the color pallete to be used (default is "Set1")
#' @return   data.tb    tibble - the input SITS table (useful for chaining functions)
#' @keywords SITS
#'
sits_plot_together <- function (data.tb, colors) {
     # this function plots all the values of all time series together (for one band)
     plot_samples <- function (ts, band, label, number) {
          series.tb <- ts %>%
               data.frame ()                      %>%  # convert list to data frame
               tibble::as_tibble()                        %>%  # convert data fram to tibble
               dplyr::select (Index, dplyr::starts_with(band))       # select only the index and the chosen band

          # compute the quantiles
          qts   = apply (series.tb[,2:ncol(series.tb)],1,stats::quantile, na.rm = TRUE)
          # create a data frame with the median, and 25% and 75% quantiles
          means.tb <- data.frame  (Index = series.tb$Index,
                                   qt25  = qts[2,],
                                   med   = qts[3,],
                                   qt75  = qts[4,]) %>%
               tibble::as_tibble()

          # melt the data into long format (required for ggplot to work)
          melted.tb <- series.tb %>%
               reshape2::melt (id.vars = "Index")
          # make the plot title
          title <- paste ("Samples (", number, ") for class ", label, " in band = ", band, sep="")
          # plot all data together
          g <- .sits_ggplot_together (melted.tb, means.tb, title)
          graphics::plot (g)
     }

     # how many different labels are there?
     labels <- dplyr::distinct (data.tb, label)

     labels %>%
          purrrlyr::by_row( function (r) {
               lb = as.character (r$label)
               # filter only those rows with the same label
               data2.tb <- dplyr::filter (data.tb, label == lb)
               # how many time series are to be plotted?
               number <- nrow (data2.tb)
               # what are the band names?
               bands  <- sits_bands (data2.tb)
               # what are the reference dates?
               ref_dates <- data2.tb[1,]$time_series[[1]]$Index
               # align all time series to the same dates
               data2.tb <- .sits_align(data2.tb, ref_dates)
               # extract the time series
               ts <- data2.tb$time_series
               # plot all samples for the same label
               bands %>%
                    purrr::map (function (band) {plot_samples(ts, band, lb, number)})
               return (data2.tb)
          })
}

#' @title Plot one timeSeries using ggplot
#'
#' @name .sits_ggplot_series
#'
#' @description plots a set of time series using ggplot. This function is used
#' for showing the same lat/long location in a series of time steps.
#'
#' @param row         a row of a SITS table with the time series to be plotted
#' @param colors      string - the set of Brewer colors to be used for plotting
.sits_ggplot_series <- function (row, colors = "Dark2") {
     # create the plot title
     plot_title <- .sits_plot_title (row)
     #extract the time series
     data.ts <- row$time_series
     # melt the data into long format
     melted.ts <- data.ts %>%
          reshape2::melt (id.vars = "Index") %>%
          as.data.frame()
     # plot the data with ggplot
     g <- ggplot2::ggplot(melted.ts, ggplot2::aes(x = Index, y = value, group = variable)) +
          ggplot2::geom_line(ggplot2::aes(color = variable)) +
          ggplot2::labs (title = plot_title) +
          # ylim(0.0, 1.0) +
          ggplot2::scale_color_brewer (palette = colors)
     # scale_colour_hue(h=c(90, 270)) - alternative to brewer
     return (g)
}

#' @title Plot many timeSeries together using ggplot
#'
#' @name .sits_ggplot_together
#'
#' @description plots a set of  time series together
#'
#' @param melted.tb   a tibble with the time series (a lot of data already melted)
#' @param means.tb    a tibble with the means and std deviations of the time series
#' @param plot_title  the title for the plot
.sits_ggplot_together <- function (melted.tb, means.tb, plot_title) {
     g <- ggplot2::ggplot(data = melted.tb, ggplot2::aes(x = Index, y = value, group = variable)) +
          ggplot2::geom_line(colour = "#819BB1", alpha = 0.5) +
          ggplot2::labs (title = plot_title) +
          ggplot2::geom_line (data = means.tb, ggplot2::aes (x = Index, y = med),  colour = "#B16240", size = 2, inherit.aes=FALSE) +
          ggplot2::geom_line (data = means.tb, ggplot2::aes (x = Index, y = qt25), colour = "#B19540", size = 1, inherit.aes=FALSE) +
          ggplot2::geom_line (data = means.tb, ggplot2::aes (x = Index, y = qt75), colour = "#B19540", size = 1, inherit.aes=FALSE)
     return (g)

}

#' @title Create a plot title to use with ggplot
#' @name sits_plot_title
#'
#' @description creates a plot title from row information
#'
#' @param row      data row with <longitude, latitude, label> information
#' @return title   string - the title to be used in the plot
.sits_plot_title <- function (row) {
     title <- paste ("location (",
                     row$latitude,  ", ",
                     row$longitude, ") - ",
                     row$label,
                     sep = "")
     return (title)
}
#' @title Create a plot title to use with ggplot
#' @name .sits_plot_alignments
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description This function should plot the alignments in a similar way as the
#'              sits_plot_TWDTW_alignments
#' @param  data.tb       A SITS tibble with the list of time series to be plotted
#' @param  distances.tb  Distances from each series to each attribute (generated by sits_distances)
#' @return data.tb       The input SITS tibble
.sits_plot_alignments <- function (data.tb, distances.tb){
    message ("sits_plot_alignments function not yet available")
    return (data.tb)

}
#' @title Plot classification alignments using the dtwSat package
#' @name sits_plot_TWDTW_alignments
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description        plots the alignments from TWDTW classification (uses dtwSat)
#' @param matches      a list of dtwSat S4 matches objects produced by sits_TWDTW_matches
#'
#' @export
sits_plot_TWDTW_alignments <- function (matches){

    matches %>%
        purrr::map( function (m.twdtw) {
            dtwSat::plot (m.twdtw, type = "alignments") %>%
                graphics::plot()
        })
    return (invisible(matches))
}

#' @title Plot classification results using the dtwSat package
#' @name sits_plot_TWDTW_classification
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description         Plots the results of TWDTW classification (uses dtwSat)
#' @param  matches      A list of dtwSat S4 matches objects produced by sits_TWDTW_matches
#' @param  start_date   Start date of the plot (used for showing classifications)
#' @param  end_date     End date of the plot (used for showing classifications)
#' @param  interval     Interval between classifications (used for showing classifications)
#' @param  overlap      Minimum overlapping between one match and the interval of classification. For details see dtwSat::twdtwApply help.
#' @export
sits_plot_TWDTW_classification <- function (matches, start_date = NULL, end_date = NULL, interval = "12 month", overlap = 0.5){

    matches %>%
        purrr::map(function (m.twdtw) {
            if (purrr::is_null (start_date) | purrr::is_null (end_date))
                dplot <- dtwSat::plot (m.twdtw, type = "classification", overlap = 0.5)
            else
                dplot <- dtwSat::plot (m.twdtw, type = "classification", from = start_date,
                                       to = end_date, by = interval, overlap = overlap)
            graphics::plot(dplot)

        })
    return (invisible(matches))
}

#' @title Plot matches between a label pattern and a time series using the dtwSat package
#' @name sits_plot_TWDTW_matches
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plots the matches from TWDTW classification for one label
#'
#' @param matches       a list of dtwSat S4 matches objects produced by sits_TWDTW_matches
#' @param patterns.tb   a set of known temporal signatures for the chosen classes
#' @param n_matches     number of matches of a given label to be displayed
#' @export
sits_plot_TWDTW_matches <- function (matches, patterns.tb, n_matches = 4) {

    matches %>%
        purrr::map(function (m.twdtw) {
            dtwSat::plot (m.twdtw, type = "matches", patterns.labels = patterns.tb$label, k = n_matches) %>%
                graphics::plot()
        })
    return (invisible(matches))
}

