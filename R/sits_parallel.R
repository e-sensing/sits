

.sits_parallel_stop <- function() {

    if (!purrr::is_null(sits_env$cl)) {
        parallel::stopCluster(sits_env$cl)
        sits_env$cl <- NULL
    }
}

.sits_parallel_start <- function(workers) {

    if (purrr::is_null(sits_env$cl) || length(sits_env$cl) != workers) {

        .sits_parallel_stop()

        if (workers > 1) {
            sits_env$cl <- parallel::makePSOCKcluster(workers)
        }
    }
}

# define local function map
.sits_parallel_map <- function(x, fun, ...) {

    if (purrr::is_null(sits_env$cl)) {
        return(lapply(x, fun, ...))
    } else {
        return(parallel::clusterApplyLB(sits_env$cl, x, fun, ...))
    }
}

