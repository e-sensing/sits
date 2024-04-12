.pdf_fun <- function(dist_name) {
    switch(
        dist_name,
        "gaussian" = dnorm,
        "weibull" = dweibull
    )
}
