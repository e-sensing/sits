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
#' @title Function to retrieve sits color value
#' @name sits_color_value
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param name   Name of color to obtain values
#' @description Returns a color value based on name
#' @return              A color value used in sits
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'     # show the names of the colors supported by SITS
#'     sits_color_value("Water")
#' }
#' @export
#'
sits_color_value <- function(name) {
    col_tab <- dplyr::filter(.conf_colors(),
                             .data[["name"]] == !!name)
    .check_that(
        nrow(col_tab) == 1,
        msg = "Class name not available in default sits color table"
    )
    return(unname(col_tab$color))
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
#'
#' @examples
#' if (sits_run_examples()) {
#' # Define a color table based on the Anderson Land Classification System
#' us_nlcd <- tibble::tibble(name = character(), color = character())
#' us_nlcd <- us_nlcd %>%
#'      tibble::add_row(name = "Urban Built Up", color =  "#85929E") %>%
#'      tibble::add_row(name = "Agricultural Land", color = "#F0B27A") %>%
#'      tibble::add_row(name = "Rangeland", color = "#F1C40F") %>%
#'      tibble::add_row(name = "Forest Land", color = "#27AE60") %>%
#'      tibble::add_row(name = "Water", color = "#2980B9") %>%
#'      tibble::add_row(name = "Wetland", color = "#D4E6F1") %>%
#'      tibble::add_row(name = "Barren Land", color = "#FDEBD0") %>%
#'      tibble::add_row(name = "Tundra", color = "#EBDEF0") %>%
#'      tibble::add_row(name = "Snow and Ice", color = "#F7F9F9")
#'
#'  # Load the color table into `sits`
#'  sits_colors_set(us_nlcd)
#'
#'  # Show the new color table used by sits
#'  sits_colors_show()
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
