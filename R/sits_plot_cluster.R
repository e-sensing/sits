#' @title  Plot the SOM grid with neurons labeled
#' @name   sits_plot_kohonen
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#' @description Given a kohonen object with a set of time neurons, plot them.
#'
#' The plot function produces different plots based on the input data:
#' \itemize{
#'  \item{"codes" - }{Plot in each neuron the vector weight which corresponds to it }
#'  \item{"mapping" - }{shows where samples are mapped.}
#'
#' }
#' The sits_plot_kohonen function plots a classified kohonen map.A set of neuron
#' with same category corresponds to a same cluster.
#' @param  data  data to be plotted (must be a kohonen object)
#'
#' @export
sits_plot_kohonen <- function(koh, type="codes") {

    if (type =="mapping"){
        plot(koh$kohonen_obj,  bgcol = koh$kohonen_obj$paint_map , "mapping")
    } else{
        plot(koh$kohonen_obj,  bgcol = koh$kohonen_obj$paint_map , "codes")
    }

    #create a legend
    leg <- cbind(koh$kohonen_obj$neurons_labelled, koh$kohonen_obj$paint_map)

    legend(
        "bottomright",
        legend = unique(leg[, 1]),
        col = unique(leg[, 2]),
        pch = c(15),
        pt.cex = 2,
        cex = 1,
        text.col = "black",
        #horiz = T ,
        inset = c(0.0350, 0.05),
        xpd = TRUE,
        ncol =1
    )

    # return the original SITS table - useful for chaining
    return(invisible(data))
}

#' @title  Plot information about clusters
#' @name   sits_plot_clusterInfo
#' @author Lorena Santos \email{lorena.santos@@inpe.br}
#'
#' @description Plot a bar graph with informations about each cluster.
#' The percentage of mixture between the clusters.
#'
#' @param  data  Table containing the percentage of mixture between the clusters
#' @export
sits_plot_clusterInfo <- function(data,text_title = " Cluster ")
{
    data <- data$mixture_cluster
    Labels = data$original_class
    palette <- .sits_kohonen_pallete()

    #this plot correspond to metrics by cluster
    p <-
        ggplot2::ggplot() + geom_bar(
            aes(
                y = data$mixture_percentage,
                x = data$cluster,
                fill = Labels
            ),
            data = data,
            stat = "identity",
            position = position_dodge()
        )  +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        labs(x = "Clusters", y="Percentage of Mixture", colour="cluster")+
        ggtitle(text_title)

    return(p)
}
