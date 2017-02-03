
#'
#' Create a plot title with ggplot
#'
#' \code{sits_plot_title} creates a plot title from row information
#'
#' @param row      data row with <longitude, latitude, label> information
#' @return title   string - the title to be used in the plot
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @export
#'
sits_plot_title <- function (row) {
     title <- paste ("location (",
                     row$latitude,  ", ",
                     row$longitude, ") - ",
                     row$label,
                     sep = "")
     return (title)
}
