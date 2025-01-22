library(sits)

som_map <- sits_som_map(samples_modis_ndvi)
# evaluate the SOM cluster
som_clusters <- sits_som_evaluate_cluster(som_map)
plot(som_clusters)

eval <- sits_som_clean_samples(
    som_map = som_map,
    prior_threshold = 0.5,
    posterior_threshold = 0.5,
    keep = c("clean", "analyze", "remove")
)

plot_eval <- function(eval){
    eval <- eval |>
        dplyr::group_by(label, eval) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::mutate(n_class  = sum(n)) |>
        dplyr::ungroup() |>
        dplyr::mutate(percentage = (n/n_class)*100) |>
        dplyr::select(label, eval, percentage) |>
        tidyr::pivot_wider(names_from = eval, values_from = percentage) |>
        dplyr::select(label, clean, remove, analyze) |>
        tidyr::replace_na(list(clean = 0, remove = 0, analyze = 0))

    pivot <- tidyr::pivot_longer(eval, cols = c(clean, remove, analyze),
                          names_to = "Eval", values_to = "value")
    labels <- unique(pivot[["label"]])
    pivot$label <- factor(pivot$label, levels = labels)

    colores_eval <- c("gold", "#009ACD", "red2")

    # Stacked bar graphs for Noise Detection
    g <- ggplot2::ggplot(pivot, ggplot2::aes(x = value,
                      y = factor(label, levels = rev(levels(label))),
                      fill = Eval)) +
        ggplot2::geom_bar(stat = "identity", color = "white", width = 0.9) +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(value/100, 1)),
                  position = ggplot2::position_stack(vjust = 0.5),
                  color = "black", size = 3,fontface = "bold",
                  check_overlap = TRUE) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
              legend.title = ggplot2::element_text(size = 11),
              legend.text = ggplot2::element_text(size = 9),
              legend.key.size = ggplot2::unit(0.5, "cm"),
              legend.spacing.y = ggplot2::unit(0.5, "cm"),
              legend.position = "right",
              legend.justification = "center") +
        ggplot2::xlab("%") +
        ggplot2::scale_fill_manual(values = colores_eval,
                          name = "Evaluation") +
        ggplot2::ggtitle("Class noise detection")

    return(g)
}

ggsave(
    filename = "Paper_Quality/Images/som_noise_detection.png",
    plot = last_plot(),
    scale = 1.1,
    width = 8,
    height = 4)
