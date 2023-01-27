#' @title Function to show colors in SITS
#' @name sits_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Returns a color palette for plotting
#'
#' @return              A ggplot2 plot
#'
#' @description         Shows the default SITS colors
#'
#' @examples
#' if (sits_run_examples()) {
#'     # show the names of the colors supported by SITS
#'     sits_colors()
#' }
#' @export
#'
sits_colors <- function() {
    colors <- .conf("colors")
    colors_tb <- tibble::tibble(name = names(colors),
                                hex = unname(unlist(colors)),
                                y = seq(0, length(colors) - 1) %% 25,
                                x = seq(0, length(colors) - 1) %/% 25)

    g <- ggplot2::ggplot() +
        ggplot2::scale_x_continuous(name = "",
                                    breaks = NULL,
                                    expand = c(0, 0)) +
        ggplot2::scale_y_continuous(name = "",
                                    breaks = NULL,
                                    expand = c(0, 0)) +
        ggplot2::geom_rect(data = colors_tb,
                  mapping = ggplot2::aes(xmin = x + 0.05,
                                         xmax = x + 0.95,
                                         ymin = y + 0.05,
                                         ymax = y + 0.95),
                  fill = colors_tb$hex
        ) +
        ggplot2::geom_text(data = colors_tb,
                  mapping = ggplot2::aes(x = x + 0.5,
                                         y = y + 0.70,
                                         label = name),
                  colour = "grey15",
                  hjust = 0.5,
                  vjust = 1,
                  size = 10 / ggplot2::.pt)

    g + ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#FFFFFF"))

    return(g)
}

#' @title Get colors associated to the labels
#' @name .colors_get
#' @param  labels  labels associated to the training classes
#' @param  palette  palette from `grDevices::hcl.pals()`
#'                  replaces default colors
#'                  when labels are not included in the config palette
#' @param  rev      revert the order of colors?
#' @keywords internal
#' @noRd
#' @return colors required to display the labels
.colors_get <- function(labels,
                         palette = "Harmonic",
                         rev = TRUE) {

    # ensure labels are unique
    labels <- unique(labels)
    # get the names of the colors in the chosen palette
    colors_palette <- unlist(.conf(key = "colors"))
    # if labels are included in the config palette, use them
    if (all(labels %in% names(colors_palette))) {
        colors <- colors_palette[labels]
    } else {
        labels_found <- labels[labels %in% names(colors_palette)]
        if (length(labels_found) > round(length(labels) / 2)) {
            warning("some labels are not available in the chosen palette",
                    call. = FALSE
            )
            missing_labels <- unique(labels[!(labels %in% labels_found)])
            warning("consider adjusting labels: ",
                    paste0(missing_labels, collapse = ", "),
                    call. = FALSE)
        } else {
            warning("most labels are not available in the chosen palette",
                    call. = FALSE
            )
        }

        warning("using hcl_color palette ", palette, call. = FALSE)

        # get the number of labels
        n_labels <- length(labels)
        # generate a set of hcl colors
        colors <- grDevices::hcl.colors(
            n = n_labels,
            palette = palette,
            alpha = 1,
            rev = rev
        )
        names(colors) <- labels
    }
    # post-condition
    .check_chr(colors,
               len_min = length(labels),
               len_max = length(labels),
               is_named = TRUE,
               has_unique_names = FALSE,
               msg = "invalid color values"
    )

    return(colors)
}
