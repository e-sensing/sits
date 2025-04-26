#' @title Function to retrieve sits color table
#' @name sits_colors
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param legend  One of the accepted legends in sits
#' @description Returns the default color table.
#' @return              A tibble with color names and values
#'
#' @note
#' SITS has a predefined color palette with 238 class names.
#' These colors are grouped by typical legends used by the Earth observation
#' community, which include “IGBP”, “UMD”, “ESA_CCI_LC”, and “WORLDCOVER”.
#' Use \code{\link[sits]{sits_colors_show}} to see a specific palette.
#' The default color table can be extended using
#' \code{\link[sits]{sits_colors_set}}.
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
    if (.has_not(legend)) {
        .conf("messages", "sits_colors_not_legend")
        return(sits_env[["color_table"]])
    } else {
        if (legend %in% names(sits_env[["legends"]])) {
            # retrieve the color names associated to the legend
            colors <- sits_env[["legends"]][[legend]]
            color_table_legend <- .conf_colors() |>
                dplyr::filter(.data[["name"]] %in% colors)
            color_table_legend <- color_table_legend[
                match(colors, color_table_legend[["name"]]),
            ]
            return(color_table_legend)
        } else {
            message(.conf("messages", "sits_colors_legend_not_available"))
            leg <- paste0(paste(.conf("messages", "sits_colors_legends"),
                          toString(names(sits_env[["legends"]])))
            )
            message(leg)
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
                             font_family = "sans") {
    # legend must be valid
    if (.has_not(legend))
        legend <- "none"
    if (!(legend %in% names(sits_env[["legends"]]))) {
        leg <- paste(.conf("messages", "sits_colors_legends"),
                            toString(names(sits_env[["legends"]]))
        )
        message(leg)
        return(invisible(NULL))
    }
    # retrieve the color names associated to the legend
    colors <- sits_env[["legends"]][[legend]]
    # retrieve the HEX codes associated to each color
    color_table_legend <- sits_env[["color_table"]] |>
        dplyr::filter(.data[["name"]] %in% colors)
    # order the colors to match the order of the legend
    color_table_legend <- color_table_legend[
        match(colors, color_table_legend[["name"]]),
    ]
    # plot the colors
    .colors_show(color_table_legend, font_family)
}

#' @title Function to set sits color table
#' @name sits_colors_set
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Includes new colors in the SITS color sets. If the colors exist,
#'              replace them with the new HEX value. Optionally, the new colors
#'              can be associated to a legend. In this case, the new legend
#'              name should be informed.
#'              The colors parameter should be a data.frame or a tibble
#'              with name and HEX code. Colour names should be one character
#'              string only. Composite names need to be combined with
#'              underscores (e.g., use "Snow_and_Ice" and not "Snow and Ice").
#'
#'              This function changes the global sits color table and the
#'              global set of sits color legends. To undo these effects,
#'              please use "sits_colors_reset()".
#'
#' @param colors New color table (a tibble or data.frame with name and HEX code)
#' @param legend Legend associated to the color table (optional)
#' @return      A modified sits color table (invisible)
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Define a color table based on the Anderson Land Classification System
#'     us_nlcd <- tibble::tibble(name = character(), color = character())
#'     us_nlcd <- us_nlcd |>
#'         tibble::add_row(name = "Urban_Built_Up", color = "#85929E") |>
#'         tibble::add_row(name = "Agricultural_Land", color = "#F0B27A") |>
#'         tibble::add_row(name = "Rangeland", color = "#F1C40F") |>
#'         tibble::add_row(name = "Forest_Land", color = "#27AE60") |>
#'         tibble::add_row(name = "Water", color = "#2980B9") |>
#'         tibble::add_row(name = "Wetland", color = "#D4E6F1") |>
#'         tibble::add_row(name = "Barren_Land", color = "#FDEBD0") |>
#'         tibble::add_row(name = "Tundra", color = "#EBDEF0") |>
#'         tibble::add_row(name = "Snow_and_Ice", color = "#F7F9F9")
#'
#'     # Load the color table into `sits`
#'     sits_colors_set(colors = us_nlcd, legend = "US_NLCD")
#'
#'     # Show the new color table used by sits
#'     sits_colors_show("US_NLCD")
#'
#'     # Change colors in the sits global color table
#'     # First show the default colors for the UMD legend
#'     sits_colors_show("UMD")
#'     # Then change some colors associated to the UMD legend
#'     mycolors <- tibble::tibble(name = character(), color = character())
#'     mycolors <- mycolors |>
#'         tibble::add_row(name = "Savannas", color = "#F8C471") |>
#'         tibble::add_row(name = "Grasslands", color = "#ABEBC6")
#'     sits_colors_set(colors = mycolors)
#'     # Notice that the UMD colors change
#'     sits_colors_show("UMD")
#'     # Reset the color table
#'     sits_colors_reset()
#'     # Show the default colors for the UMD legend
#'     sits_colors_show("UMD")
#' }
#' @export
#'
sits_colors_set <- function(colors, legend = NULL) {
    # set caller for error messages
    .check_set_caller("sits_colors_set")
    # add the new color table
    new_color_tb <- .conf_add_color_table(colors)
    if (.has(legend)) {
        # add the list of color names to a new legend
        .check_chr_parameter(legend)
        # create a new legend entry
        new_legend_entry <- list()
        # add the colors from the color table
        new_legend_entry[[1L]] <- dplyr::pull(colors, .data[["name"]])
        # give a new to the new legend entry
        names(new_legend_entry) <- legend
        sits_env[["legends"]] <- c(sits_env[["legends"]], new_legend_entry)
    }
    return(invisible(new_color_tb))
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
#'    data_dir <- system.file("extdata/raster/classif", package = "sits")
#'    ro_class <- sits_cube(
#'       source = "MPC",
#'       collection = "SENTINEL-2-L2A",
#'       data_dir = data_dir,
#'       parse_info = c( "X1", "X2", "tile", "start_date", "end_date",
#'                       "band", "version"),
#'       bands = "class",
#'       labels = c(
#'            "1" = "Clear_Cut_Burned_Area",
#'            "2" = "Clear_Cut_Bare_Soil",
#'            "3" = "Clear_Cut_Vegetation",
#'            "4" = "Forest")
#'   )
#'   qml_file <- paste0(tempdir(), "/qgis.qml")
#'   sits_colors_qgis(ro_class, qml_file)
#' }
#' @export
#'
sits_colors_qgis <- function(cube, file) {
    .check_set_caller("sits_colors_qgis")
    # check if cube is a class cube
    .check_is_class_cube(cube)
    # check if the file name is valid
    .check_file(file, file_exists = FALSE)
    # retrieve the labels of the cube
    labels <- .cube_labels(cube)
    # select the colors for the labels of the cube
    color_table <- .conf_colors()
    # check all labels are in the color table
    .check_chr_within(labels, color_table[["name"]])
    # filter the color table
    color_table <- color_table |>
        dplyr::filter(.data[["name"]] %in% labels)
    # order the colors to match the order of the labels
    color_table <- color_table[
        match(labels, color_table[["name"]]),
    ]
    # include an index
    color_table[["index"]] <- names(labels)
    # create a QGIS XML file
    .colors_qml(color_table, file)
}
