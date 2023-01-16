
#---- stats ----

#' @title Supports former version of stats
#' @noRd
.stats_0_q02 <- function(stats, band) {
    quantile_02 <- 2
    stats[[band]][[quantile_02]]
}

#' @title Supports former version of stats
#' @noRd
.stats_0_q98 <- function(stats, band) {
    quantile_98 <- 3
    stats[[band]][[quantile_98]]
}

.stats_q02 <- function(stats) {
    stats[["q02"]]
}

.stats_q98 <- function(stats) {
    stats[["q98"]]
}
