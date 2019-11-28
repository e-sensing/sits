pbLapply <- function(slaves, progress, X, FUN, ...) {
    cl <- snow::makeSOCKcluster(slaves)
    doSNOW::registerDoSNOW(cl)
    opts <- list()

    if(progress){
        pb <- utils::txtProgressBar(max = length(X))
        on.exit(close(pb))
        progress <- function(n) utils::setTxtProgressBar(pb, n)
        opts <- list(progress = progress)
    }

    on.exit(snow::stopCluster(cl))
    foreach(i = X, .options.snow = opts) %dopar% {
        FUN(i, ...)
    }
}
