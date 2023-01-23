#' @title Function to handle colors in SITS
#' @name sits_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Returns a color palette for plotting
#'
#' @param  name          Name of sits color palette
#' @param  n_colors      Number of colors to be displayed
#' @return               A vector with valid R colors
#'
#' @description
#' Implements support RColorBrewer and named sits palettes. Valid names are:
#' "veg_index", "probs", "uncert", "soil", "forest", "water", "green",
#' "BuGn", "BuPu", "GnBu", "PuBu", "PuBuGn", "PuRd", "YlGn", "YlGnBu", "YlOrRd",
#' "YlOrBr", "OrRd", "Blues", "Greens", "Oranges", "Reds", "Greys", "Purples".
#'
#' @examples
#' if (sits_run_examples()) {
#'     # show the names of the colors supported by SITS
#'     sits_color_names()
#'     # show a color palette
#'     sits_colors_show("Blues", n_colors = 16)
#' }
#' @export
#'
sits_colors <- function(name, n_colors = 32) {
    # check if Brewer palette exists
    .check_require_packages("RColorBrewer")
    .check_chr_contains(
        x = name,
        contains = .conf("sits_color_palettes"),
        discriminator = "any_of",
        msg = paste0("Color palette not supported"),
        local_msg = paste("Palette should be one of ",
                          paste0(.conf("sits_color_palettes"),
                                             collapse = ", "))
    )
    colors <- (grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(9, name)))(n_colors)
}
#' @title Show names of SITS color palettes
#' @name sits_color_names
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Returns the names of the palettes supported by SITS
#'
#' @return               Names of available color palettes in SITS
#' @examples
#' if (sits_run_examples()) {
#'     # show the names of the colors supported by SITS
#'     sits_color_names()
#'     # show a color palette
#'     sits_colors_show("Blues", n_colors = 16)
#' }
#' @export
sits_color_names <- function(){
    color_names <- .conf("sits_color_palettes")
    return(color_names)
}
#' @title Show one SITS color palette
#' @name sits_colors_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Shows a barplot with the requested color palette
#'
#' @param  name          Name of sits color palette
#' @param  n_colors      Number of colors to be displayed
#' @return               Called for side effects
#' @examples
#' if (sits_run_examples()) {
#'     # show the names of the colors supported by SITS
#'     sits_color_names()
#'     # show a color palette
#'     sits_colors_show("Blues", n_colors = 16)
#' }
#' @export
sits_colors_show <- function(name, n_colors = 32){

    .check_chr_contains(
        x = name,
        contains = .conf("sits_color_palettes"),
        discriminator = "any_of",
        msg = paste0("Color palette not supported"),
        local_msg = paste("Palette should be one of ",
                          paste0(.conf("sits_color_palettes"),
                                             collapse = ", "))
    )
    colors <- sits_colors(name, n_colors)
    graphics::barplot(height = rep(10, n_colors),
            width = rep(2, n_colors),
            col = colors,
            axes = FALSE,
            main = paste0("sits palette ", name),
            names = c(1:n_colors)
    )
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
    # labels <- unique(labels)
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
        n_labels <- length(unique(labels))
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
