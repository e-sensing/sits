#' @title  Plot the SOM grid with neurons labeled
#' @name   sits_plot_som
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#' @description Given a kohonen object with a set of time neurons, plot them.
#'
#' The plot function produces different plots based on the input data:
#' \itemize{
#'  \item{"codes": }{Plot in each neuron the vector weight which corresponds to it.}
#'  \item{"mapping": }{Shows where samples are mapped.}
#' }
#' Function sits_plot_som plots a classified kohonen map. A set of neurons
#' with same category corresponds to a same cluster.
#' @param  koh  Data to be plotted (must be a kohonen object).
#' @param  type Type of plot. "codes" is the weight of neuron (time series) and "mapping" is the number of samples allocated in a neuron.
#' @param  whatmap What data layer will be plotted.
#' @examples
#' \donttest{
#' # Plot kohonen map with vector of weight
#' sits_plot_som(koh, type = "codes")
#' # Plot kohonen map showing where the samples were allocated
#' sits_plot_som(koh, type = "mapping")
#' }
#'
#' @export
sits_plot_som <- function(koh, type = "codes", whatmap = 1)
{
    if (type == "mapping"){
        graphics::plot(koh$som_properties,  bgcol = koh$som_properties$paint_map , "mapping", whatmap = whatmap)
    } else{
        graphics::plot(koh$som_properties,  bgcol = koh$som_properties$paint_map , "codes", whatmap = whatmap)
    }

    #create a legend
    leg <- cbind(koh$som_properties$neuron_label, koh$som_properties$paint_map)
    graphics::legend(
        "bottomright",
        legend = unique(leg[, 1]),
        col = unique(leg[, 2]),
        pch = 15,
        pt.cex = 2,
        cex = 1,
        text.col = "black",
        #horiz = T ,
        inset = c(0.0095, 0.05),
        xpd = TRUE,
        ncol = 1
    )
}

#' @title  Plot information about clusters
#' @name   sits_plot_cluster_info
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#'
#' @description Plot a bar graph with informations about each cluster.
#' The percentage of mixture between the clusters.
#'
#' @param data       Table containing the percentage of mixture between the clusters.
#' @param text_title Title of plot. Default is "Cluster".
#'
#' @examples
#' \donttest{
#' # Plot confusion between the clusters
#' sits_plot_cluster_info(confusion_by_cluster, "Confusion by cluster")
#' }
#' @export
sits_plot_cluster_info <- function(data, text_title = " Cluster ")
{
    data <- data$mixture_cluster
    labels <- data$original_class

    p <-
        ggplot2::ggplot() + ggplot2::geom_bar(
            ggplot2::aes(
                y = data$mixture_percentage,
                x = data$cluster,
                fill = labels
            ),
            data = data,
            stat = "identity",
            position = ggplot2::position_dodge()
        )  +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
        ggplot2::labs(x = "Clusters", y = "Percentage of mixture", colour = "cluster")+
        ggplot2::ggtitle(text_title)

    return(p)
}

#' @title  Plot the patterns of subgroups
#' @name   sits_plot_subgroups
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#'
#' @description Plot the average pattern of subgroups from  weights of neurons labelled.
#' Each neuron has a weight which can be represent a set of time series samples. Neurons of same
#' category or label form a cluster, but the neurons within a cluster can have different patterns.
#' The plots are also saved into files named "class_neurons<class_neuron>_plot_EVI<subgroup>.png".
#' @param  neurons_subgroup  The list contain the EVI and NDVI time series (weight of each neuron) by class.
#' @export
sits_plot_subgroups <- function(subgroups.tb, cluster = cluster_name, band = "ndvi")
{
    samples_current_class.tb <- dplyr::filter(subgroups.tb$samples_subgroups.tb, subgroups.tb$samples_subgroups.tb$som_label == cluster)

    #Get the amount of subclasses
    names_subgroups <- sort(unique(samples_current_class.tb$label_subgroup))
    number_of_subgroups <- length(unique(samples_current_class.tb$label_subgroup))

    #Put this value in a dynamic way
    if(band == "ndvi")
        codes <- subgroups.tb$som_properties$codes$ndvi

    if(band == "evi")
        codes <- subgroups.tb$som_properties$codes$evi

    plot.vec <- vector()
    for (i in 1:number_of_subgroups)
    {
        get_samples_current_subgroup.tb <- dplyr::filter(samples_current_class.tb, samples_current_class.tb$label_subgroup == names_subgroups[i] )
        get_neurons <- get_samples_current_subgroup.tb$id_neuron

        current_codes <- codes[get_neurons, ]
        time_series_current_subgroup.ts <- zoo::zoo(t(current_codes))

        #Arrumar aqui quando tem 1 neuron só
        # if (length(get_neurons) == 1)
        # {
        #     colnames(time_series_current_subgroup.ts) <- "V"
        #     time_series_current_subgroup.ts <-
        #         zoo::zoo((current_codes))
        # }


        group_ts <-
            data.frame(
                value = as.vector(time_series_current_subgroup.ts),
                index_time = stats::time(time_series_current_subgroup.ts),
                neurons = rep(
                    names(time_series_current_subgroup.ts),
                    each = nrow(time_series_current_subgroup.ts)
                )
            )

        plot_ts_subgroup <-
            ggplot2::ggplot(group_ts, ggplot2::aes(x = index_time, y = value)) +
            ggplot2::stat_summary(fun.data = "mean_cl_boot",
                                  geom = "smooth") + ggplot2::labs(x = "Time", y = band) +
            ggplot2::ggtitle(paste(names_subgroups[i]))


        graphics::plot(plot_ts_subgroup)

    }
    return(plot_ts_subgroup)
}
