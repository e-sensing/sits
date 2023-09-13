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
#' @param legend  One of the accepted legend in sits
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
sits_colors_show <- function(legend = NULL) {
    # verifies if stringr package is installed
    .check_require_packages("stringr")
    legends <- .conf("legends")
    if (purrr::is_null(legend)) {
        print("Showing all available colors")
        leg <- paste0(paste("Optional - select one of the legends: "),
               paste(legends, collapse = ", "))
        print(leg)
        g <- .colors_show(sits_colors())
        return(g)
    } else {
        if (legend %in% legends) {
            colors <- .conf(legend)
            color_table_legend <- sits_colors() |>
                dplyr::filter(.data[["name"]] %in% colors)
            color_table_legend <- color_table_legend[
                match(colors, color_table_legend$name), ]
            g <- .colors_show(color_table_legend)
            return(g)
        } else {
            print("Selected map legend not available")
            leg <- paste0(paste("Please select one of the legends: "),
                          paste(legends, collapse = ", "))
            print(leg)
            return(NULL)
        }
    }
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
