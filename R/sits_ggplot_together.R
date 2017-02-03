#'
#' Plot many timeSeries together using ggplot
#'
#' \code{sits_ggplot_together} plots a set of  time series together
#'
#' @param melted.tb   a tibble with the time series (a lot of data already melted)
#' @param means.tb    a tibble with the means and std deviations of the time series
#' @keywords SITS
#' @family   SITS auxiliary functions
#' @export
# plot the data with ggplot
sits_ggplot_together <- function (melted.tb, means.tb, plot_title) {
     g <- ggplot(melted.tb, aes(x=Index, y=value, group = variable)) +
          geom_line(colour = "#819BB1", alpha = 0.5) +
          labs (title = plot_title) +
          geom_line (data = means.tb, aes (x = Index, y = mean), colour = "#B16240", size = 2, inherit.aes=FALSE) +
          geom_line (data = means.tb, aes (x = Index, y = stdplus), colour = "#B19540", size = 1, inherit.aes=FALSE) +
          geom_line (data = means.tb, aes (x = Index, y = stdminus), colour = "#B19540", size = 1, inherit.aes=FALSE)
     return (g)

}
