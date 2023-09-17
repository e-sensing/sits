#' @title Function to retrieve sits color table
#' @name sits_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param legend  One of the accepted legends in sits
#' @description Returns a color table
#' @return              A tibble with color names and values
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # return the names of all colors supported by SITS
#'     sits_colors()
#' }
#' @export
#'
sits_colors <- function(legend = NULL) {
    legends <- .conf("legends")
    if (purrr::is_null(legend)) {
        print("Returning all available colors")
        return(.conf_colors())
    } else {
        if (legend %in% legends) {
            colors <- .conf(legend)
            color_table_legend <- .conf_colors() |>
                dplyr::filter(.data[["name"]] %in% colors)
            color_table_legend <- color_table_legend[
                match(colors, color_table_legend$name), ]
            return(color_table_legend)
        } else {
            print("Selected map legend not available")
            leg <- paste0(paste("Please select one of the legends: "),
                          paste(legends, collapse = ", "))
            print(leg)
            return(NULL)
        }
    }
}
#' @title Function to show colors in SITS
#' @name sits_colors_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description         Shows the default SITS colors
#' @param legend  One of the accepted legends in sits
#' @param font_family A font family loaded in SITS
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
sits_colors_show <- function(legend = NULL,
                             font_family = "roboto") {
    # verifies if sysfonts package is installed
    .check_require_packages("sysfonts")
    # checks if font family is available
    if (!font_family %in% sysfonts::font_families())
        font_family <- "roboto"
    # legend must be valid
    legends <- .conf("legends")
    if (purrr::is_null(legend))
        legend <- "none"
    if (!(legend %in% legends)) {
        msg <- paste0(paste("Please select one of the legends: "),
                      paste(legends, collapse = ", "))
        print(msg)
        return(invisible(NULL))
    }
    # retrieve the color names associated to the legend
    colors <- .conf(legend)
    # retrive the HEX codes associated to each color
    color_table_legend <- .conf_colors() |>
        dplyr::filter(.data[["name"]] %in% colors)
    # order the colors to match the order of the legend
    color_table_legend <- color_table_legend[
        match(colors, color_table_legend$name), ]
    # plot the colors
    g <- .colors_show(color_table_legend, font_family)

    return(g)
}

#' @title Function to set sits color table
#' @name sits_colors_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Sets a color table
#' @param color_tb New color table
#' @return      A modified sits color table
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Define a color table based on the Anderson Land Classification System
#'     us_nlcd <- tibble::tibble(name = character(), color = character())
#'     us_nlcd <- us_nlcd |>
#'         tibble::add_row(name = "Urban Built Up", color = "#85929E") |>
#'         tibble::add_row(name = "Agricultural Land", color = "#F0B27A") |>
#'         tibble::add_row(name = "Rangeland", color = "#F1C40F") |>
#'         tibble::add_row(name = "Forest Land", color = "#27AE60") |>
#'         tibble::add_row(name = "Water", color = "#2980B9") |>
#'         tibble::add_row(name = "Wetland", color = "#D4E6F1") |>
#'         tibble::add_row(name = "Barren Land", color = "#FDEBD0") |>
#'         tibble::add_row(name = "Tundra", color = "#EBDEF0") |>
#'         tibble::add_row(name = "Snow and Ice", color = "#F7F9F9")
#'
#'     # Load the color table into `sits`
#'     sits_colors_set(us_nlcd)
#'
#'     # Show the new color table used by sits
#'     sits_colors_show()
#' }
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
#'
#' @examples
#' if (sits_run_examples()) {
#'     # reset the default colors supported by SITS
#'     sits_colors_reset()
#' }
#' @export
#'
sits_colors_reset <- function() {
    .conf_load_color_table()
    return(invisible(NULL))
}
#' @title Function to save color table as QML style for data cube
#' @name sits_colors_qgis
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Saves a color table associated to a classified
#'              data cube as a QGIS style file
#' @param   cube a classified data cube
#' @param   file a QGIS style file to be written to
#' @return      No return, called for side effects
#'
#' @examples
#' if (sits_run_examples()) {
#'     # reset the default colors supported by SITS
#'     sits_colors_reset()
#' }
#' @export
#'
sits_colors_qgis <- function(cube, file) {
    # check if cube is a class cube
    .check_cube_is_class_cube(cube)
    # check if the file name is valid
    .check_file(file,
                file_exists = FALSE,
                msg = "Please select a valid file name")
    # retrieve the labels of the cube
    labels <- sits_labels(cube)
    # select the colors for the labels of the cube
    color_table <- .conf_colors()
    # check all labels are in the color table
    .check_chr_within(labels,
                      color_table$name,
                      msg = "all labels should be included in the color table")
    # filter the color table
    color_table <- color_table |>
        dplyr::filter(.data[["name"]] %in% labels)
    # order the colors to match the order of the labels
    color_table <- color_table[
        match(labels, color_table$name), ]
    # include an index
    color_table$index <- names(labels)
    # create a QGIS XML file
    .colors_qml(color_table, file)
    return(invisible(NULL))
}
