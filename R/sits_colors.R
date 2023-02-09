#' @title Function to retrieve sits color table
#' @name sits_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Returns a color table
#' @return              A tibble with color names and values
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # show the names of the colors supported by SITS
#'     sits_colors()
#' }
#' @export
#'
sits_colors <- function() {
    return(.conf_colors())
}
#' @title Function to show colors in SITS
#' @name sits_colors_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description         Shows the default SITS colors
#'
#' @return  no return, called for side effects
#'
#' @examples
#' if (sits_run_examples()) {
#'     # show the colors supported by SITS
#'     sits_colors_show()
#' }
#' @export
#'
sits_colors_show <- function() {
    g <- .colors_show(sits_colors())
    return(g)
}

#' @title Function to set sits color table
#' @name sits_colors_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Sets a color table
#' @param color_tb New color table
#' @return      A modified sits color table
#' @export
#'
sits_colors_set <- function(color_tb) {
    .conf_set_color_table(color_tb)
    return(invisible(color_tb))
}
#' @title Function to reset sits color table
#' @name sits_colors_reset
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Resets the color table
#' @return      No return, called for side effects
#' @export
#'
sits_colors_reset <- function() {
    .conf_load_color_table()
    return(invisible(NULL))
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
    # get the color table
    color_tb <- .conf_colors()
    # if labels are included in the config palette, use them
    if (all(labels %in% color_tb$name)) {
        colors <- purrr::map_chr(labels, function(l){
            col <- color_tb %>%
                dplyr::filter(.data[["name"]] == l) %>%
                dplyr::pull(color)
            return(col)
        })
        names(colors) <- labels
    } else {
        labels_found <- labels[labels %in% color_tb$name]
        missing_labels <- unique(labels[!(labels %in% labels_found)])
        warning("labels missing in color table: ",
                    paste0(missing_labels, collapse = ", "),
                    call. = FALSE)
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
    # rename colors to fit the label order
    # and deal with duplicate labels
    colors <- colors[labels]
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
#' @title Show color table
#' @name .colors_show
#' @keywords internal
#' @noRd
#' @param color_tb A SITS color table
#' @return a gglot2 object
.colors_show <- function(color_tb) {

    n_colors <- nrow(color_tb)
    n_rows_show <- n_colors %/% 3

    color_tb <- tibble::add_column(color_tb,
                                y = seq(0, n_colors - 1) %% n_rows_show,
                                x = seq(0, n_colors - 1) %/% n_rows_show)
    g <- ggplot2::ggplot() +
         ggplot2::scale_x_continuous(name = "",
                                    breaks = NULL,
                                    expand = c(0, 0)) +
         ggplot2::scale_y_continuous(name = "",
                                    breaks = NULL,
                                    expand = c(0, 0)) +
         ggplot2::geom_rect(data = color_tb,
                           mapping = ggplot2::aes(xmin = x + 0.05,
                                                  xmax = x + 0.95,
                                                  ymin = y + 0.05,
                                                  ymax = y + 0.95),
                           fill = color_tb$color
        ) +
        ggplot2::geom_text(data = color_tb,
                           mapping = ggplot2::aes(x = x + 0.5,
                                                  y = y + 0.70,
                                                  label = name),
                           colour = "grey15",
                           hjust = 0.5,
                           vjust = 1,
                           size = 9 / ggplot2::.pt)

    g + ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#FFFFFF"))

    return(g)
}

