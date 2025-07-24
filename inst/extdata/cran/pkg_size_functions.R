pkg_size_ <- function(pkg) {
    # https://stackoverflow.com/questions/70991241/installed-r-packages-size
    res <- system(
        paste("du -sh", system.file(package = pkg), "| awk '{print $1}'"),
        intern = TRUE
    )

    fs::as_fs_bytes(res)
}

pkg_size <- function(pkg) {
    fs::as_fs_bytes(unlist(lapply(pkg, pkg_size_)))
}

pkg_size_recursive <- function(pkg) {
    deps <- c(pkg, unlist(tools::package_dependencies(pkg, which = "all",
                                                      recursive = TRUE)))
    size <- pkg_size(deps)

    total_size <- sum(size)

    data.frame(
        pkg = c(deps, "total"),
        size = structure(c(size, sum(size)), class = c("fs_bytes", "numeric"))
    )
}
