#' @keywords internal
#' @noRd
#' @export
.vector_check_package.sf <- function() {
    # package namespace
    pkg_name <- "sf"

    # check if terra package is available
    .check_require_packages(pkg_name)

    class(pkg_name) <- pkg_name

    return(invisible(pkg_name))
}
