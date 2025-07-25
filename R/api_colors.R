#' @title Get colors associated to the labels
#' @name .colors_get
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  labels  labels associated to the training classes
#' @param  palette  palette from `grDevices::hcl.pals()`
#'                  replaces default colors
#'                  when labels are not included in the config palette
#' @param  rev      revert the order of colors?
#' @keywords internal
#' @noRd
#' @return colors required to display the labels
.colors_get <- function(labels,
                        palette = "Set3",
                        legend = NULL,
                        rev = TRUE) {
    .check_set_caller(".colors_get")
    # Get the SITS Color table
    color_tb <- .conf_colors()
    # Try to find colors in the SITS color palette
    names_tb <- dplyr::filter(color_tb, .data[["name"]] %in% labels)[["name"]]
    # find the labels that exist in the color table
    labels_exist <- labels[labels %in% names_tb]
    # get the colors for the names that exist
    colors <- purrr::map_chr(labels_exist, function(l) {
        color_tb |>
            dplyr::filter(.data[["name"]] == l) |>
            dplyr::pull(.data[["color"]])
    })
    # get the names of the colors that exist in the SITS color table
    names(colors) <- labels_exist
    # if there is a legend?
    if (.has(legend)) {
        # what are the names in the legend that are in the labels?
        labels_leg <- labels[labels %in% names(legend)]
        # what are the color labels that are included in the legend?
        colors_leg <- legend[labels_leg]
        # join color names in the legend to those in default colors
        colors <- c(
            colors_leg,
            colors[!names(colors) %in% names(colors_leg)]
        )
    }
    # are there any colors missing?
    if (!all(labels %in% names(colors))) {
        # find out the missing colors
        missing <- labels[!labels %in% names(colors)]
        # issue a warning
        .message_warnings_colors_get(missing, palette)
        # assume colors for the missing labels
        colors_pal <- grDevices::hcl.colors(
            n = length(missing),
            palette = palette,
            rev = rev
        )
        names(colors_pal) <- missing
        # put all colors together
        colors <- c(colors, colors_pal)
    }
    # rename colors to fit the label order
    # and deal with duplicate labels
    colors <- colors[labels]
    # post-condition
    .check_chr(
        colors,
        len_min = length(labels),
        len_max = length(labels),
        is_named = TRUE,
        has_unique_names = FALSE
    )
    colors
}
#' @title Show color table
#' @name .colors_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param color_tb A SITS color table
#' @return a gglot2 object
.colors_show <- function(color_tb, font_family) {
    # strip "_" if they exist
    color_tb[["name"]] <- purrr::map_chr(
        color_tb[["name"]], function(name) {
            paste(name = unlist(
                strsplit(name, split = "_", fixed = TRUE)
            ), collapse = " ")
        }
    )
    # find out how many lines to write per name
    color_tb[["lines"]] <- purrr::map_int(color_tb[["name"]], function(s) {
        stringr::str_count(stringr::str_wrap(s, width = 12L), "\n") + 1L
    })
    n_colors <- nrow(color_tb)
    if (n_colors <= 12L) {
        n_rows_show <- 3L
    } else {
        n_rows_show <- n_colors %/% 4L
    }
    # add place locators to color table entries
    color_tb <- tibble::add_column(
        color_tb,
        y = seq(0L, n_colors - 1L) %% n_rows_show,
        x = seq(0L, n_colors - 1L) %/% n_rows_show
    )
    y_size <- 1.2
    ggplot2::ggplot() +
        ggplot2::scale_x_continuous(
            name = "",
            breaks = NULL,
            expand = c(0.0, 0.0)
        ) +
        ggplot2::scale_y_continuous(
            name = "",
            breaks = NULL,
            expand = c(0.0, 0.0)
        ) +
        ggplot2::geom_rect(
            data = color_tb,
            mapping = ggplot2::aes(
                xmin = .data[["x"]] + 0.05,
                xmax = .data[["x"]] + 0.95,
                ymin = .data[["y"]] + 0.05,
                ymax = .data[["y"]] + y_size
            ),
            fill = color_tb[["color"]]
        ) +
        ggplot2::geom_text(
            data = color_tb,
            mapping = ggplot2::aes(
                x = .data[["x"]] + 0.5,
                y = .data[["y"]] + 0.6 + 0.1 * (.data[["lines"]] - 1L),
                label = stringr::str_wrap(.data[["name"]], width = 12L)
            ),
            family = font_family,
            colour = "grey15",
            hjust = 0.5,
            vjust = 1.0,
            size = 10.0 / ggplot2::.pt
        ) +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "#FFFFFF")
        )
}
#'
#' @title Write a color table in QGIS Style format
#' @name .colors_qml
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param color_table color table to write to QGIS
#' @param file the file name to be written to.
#' @return Called for side effects.
.colors_qml <- function(color_table, file) {
    # check that color table has index, name and color
    .check_chr_contains(colnames(color_table), c("index", "color", "name"))
    # open the file connection
    con <- file(file, "w")

    # read the top part of QGIS style
    top_qgis_style <- system.file("extdata/qgis/qgis_style_top.xml",
        package = "sits"
    )
    top_lines <- readLines(top_qgis_style)
    # write the top part of QGIS style in the output file
    writeLines(top_lines, con = con)
    # write start of color palette
    writeLines("      <colorPalette>", con = con)
    # write palette entries
    purrr::pmap_chr(
        list(
            color_table[["index"]],
            color_table[["color"]],
            color_table[["name"]]
        ),
        function(ind, color, name) {
            writeLines(
                paste0(
                    "         <paletteEntry ",
                    " value=", "\"", ind, "\"",
                    " color=", "\"", color, "\"",
                    " label=", "\"", name, "\"",
                    " alpha=", "\"255\"", "/>"
                ),
                con = con
            )
            invisible("")
        }
    )
    # write end of color palette
    writeLines("     </colorPalette>", con = con)
    # read the bottom part of QGIS style files
    # this part goes after the palette entry
    bottom_qgis_style <- system.file("extdata/qgis/qgis_style_bottom.xml",
        package = "sits"
    )
    bottom_lines <- readLines(bottom_qgis_style)
    # write the bottom part of QGIS style in the output file
    writeLines(bottom_lines, con = con)
    # close the file
    on.exit(close(con))
}
#' @title Transform an RColorBrewer name to cols4all name
#' @name .colors_cols4all_name
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param palette  An RColorBrewer palette name
#' @return A valid cols4all palette name
#'
.colors_cols4all_name <- function(palette) {
    .check_set_caller(".colors_cols4all_name")
    # check if palette name is in RColorBrewer
    brewer_pals <- rownames(RColorBrewer::brewer.pal.info)
    if (palette %in% brewer_pals) {
        # get cols4all palette names
        c4a_pals <- cols4all::c4a_palettes()
        c4a_brewer <- c4a_pals[grep("brewer.", c4a_pals)]
        c4a_pal_name <- c4a_brewer[which(brewer_pals == palette)]
    } else {
        c4a_pal_name <- cols4all::c4a_info(palette, verbose = FALSE)$fullname
    }
    c4a_pal_name
}
#' @title Transform an legend from tibble to vector
#' @name .colors_legend_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @param legend A legend in tibble format
#' @return A valid legend as vector
#'
.colors_legend_set <- function(legend) {
    if (inherits(legend, "tbl_df")) {
        .check_legend(legend)
        legend_vec <- legend[["color"]]
        names(legend_vec) <- legend[["name"]]
        return(legend_vec)
    }
    legend
}
