#' @title  Plot the SOM grid with neurons labeled
#' @name   sits_plot_kohonen
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#' @description Given a kohonen object with a set of time neurons, plot them.
#'
#' The plot function produces different plots based on the input data:
#' \itemize{
#'  \item{"codes": }{Plot in each neuron the vector weight which corresponds to it.}
#'  \item{"mapping": }{Shows where samples are mapped.}
#' }
#' Function sits_plot_kohonen plots a classified kohonen map. A set of neurons
#' with same category corresponds to a same cluster.
#' @param  koh  Data to be plotted (must be a kohonen object).
#' @param  type Type of plot. "codes" is the weight of neuron (time series) and "mapping" is the number of samples allocated in a neuron.
#'
#' @export
sits_plot_kohonen <- function(koh, type = "codes")
{
    if (type == "mapping"){
        graphics::plot(koh$kohonen_obj,  bgcol = koh$kohonen_obj$paint_map , "mapping")
    } else{
        graphics::plot(koh$kohonen_obj,  bgcol = koh$kohonen_obj$paint_map , "codes")
    }

    #create a legend
    leg <- cbind(koh$kohonen_obj$neurons_labelled, koh$kohonen_obj$paint_map)
    graphics::legend(
        "bottomright",
        legend = unique(leg[, 1]),
        col = unique(leg[, 2]),
        pch = 15,
        pt.cex = 2,
        cex = 1,
        text.col = "black",
        #horiz = T ,
        inset = c(0.0350, 0.05),
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
        ggplot2::labs(x = "Clusters", y = "Percentage of Mixture", colour = "cluster")+
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
sits_plot_subgroups <- function(neurons_subgroup)
{
    #get the name of class_neurons from list neurons_subgroup
    class_neurons <- names(neurons_subgroup)

    for (i in 1:length(class_neurons))
    {
        #get the current group
        group <- neurons_subgroup[i]

        #acess each list inside the list of group
        index_time <- vector()
        for (j in 1:length(group[[1]]))
        {
            #get list of class i and subgroup j, for example
            #class pasture subgroup 1
            subgroup_j <- as.matrix(group[[1]][[j]])

            if (NCOL(subgroup_j) == 1)
            {
                ts_ndvi.ts <- as.matrix(subgroup_j[1:23])
                colnames(ts_ndvi.ts) <- "V"
                #get only evi
                ts_evi.ts <- as.matrix(subgroup_j[24:46])
                colnames(ts_evi.ts) <-"V"
                ts_group_ndvi.ts <- zoo::zoo((ts_ndvi.ts))
                ts_group_evi.ts <- zoo::zoo((ts_evi.ts))
            } else{
                #get only ndvi
                ts_ndvi.ts <- subgroup_j[, 1:23]
                #get only evi
                ts_evi.ts <- subgroup_j[, 24:46]
                ts_group_ndvi.ts <- zoo::zoo(t(ts_ndvi.ts))
                ts_group_evi.ts  <- zoo::zoo(t(ts_evi.ts))
            }

            groupts_ndvi.df <-
                data.frame(
                    value = as.vector(ts_group_ndvi.ts),
                    index_time = stats::time(ts_group_ndvi.ts),
                    neurons = rep(
                        names(ts_group_ndvi.ts),
                        each = nrow(ts_group_ndvi.ts)
                    )
                )

            groupts_evi.df <-
                data.frame(
                    value = as.vector(ts_group_evi.ts),
                    index_time = stats::time(ts_group_evi.ts),
                    neurons = rep(names(ts_group_evi.ts), each = nrow(ts_group_evi.ts))
                )
            # -------------------------------- Plots -------------------------------------------------------
            p.ndvi <-
                ggplot2::ggplot(groupts_ndvi.df, ggplot2::aes(x = index_time, y = value)) +
                ggplot2::stat_summary(fun.data = "mean_cl_boot",
                                      geom = "smooth") + ggplot2::labs(x = "Time", y = "NDVI") +
                ggplot2::ggtitle(paste(class_neurons[i], " Group ", j , sep = ''))

            p.evi <-
                ggplot2::ggplot(groupts_evi.df, ggplot2::aes(x = index_time, y = value)) +
                ggplot2::stat_summary(fun.data = "mean_cl_boot",
                                      geom = "smooth") + ggplot2::labs(x = "Time", y =
                                                                           "EVI") + ggplot2::ggtitle(paste(class_neurons[i], " Group ", j , sep = ''))
            #save plots in a set folder
            ggplot2::ggsave(
                paste(class_neurons[i], "_plot_EVI", j, ".png" , sep = ''),
                plot = p.evi,
                device = "png"
            )

            ggplot2::ggsave(
                paste(class_neurons[i], "_plot_NDVI", j, ".png" , sep = ''),
                plot = p.ndvi,
                device = "png"
            )
        }
    }
}
