options(timeout = 600)
kind <- "cpu-intel"
version <- "0.13.0.9001"
options(repos = c(
    torch = sprintf("https://torch-cdn.mlverse.org/packages/%s/%s/", kind, version),
    CRAN = "https://cloud.r-project.org" # or any other from which you want to install the other R dependencies.
))
install.packages("torch", type = "binary")
