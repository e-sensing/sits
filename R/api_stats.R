#---- stats ----

#' @title Supports former version of stats for Q02
#' @noRd
#' @param stats    Training data statistics
#' @param band     Spectral band
#' @return old_style statistics for band for Q02
.stats_0_q02 <- function(stats, band) {
    quantile_02 <- 2L
    stats[[band]][[quantile_02]]
}
#' @title Supports former version of stats for Q98
#' @noRd
#' @param stats    Training data statistics
#' @param band     Spectral band
#' @return old_style statistics for band for Q98
.stats_0_q98 <- function(stats, band) {
    quantile_98 <- 3L
    stats[[band]][[quantile_98]]
}
#' @title Stats for Q02
#' @noRd
#' @param stats    Training data statistics
#' @return statistics for Q02
.stats_q02 <- function(stats) {
    stats[["q02"]]
}
#' @title Stats for Q98
#' @noRd
#' @param stats    Training data statistics
#' @return statistics for Q98
.stats_q98 <- function(stats) {
    stats[["q98"]]
}
