#' @title Create file name
#' @name .create_filename
#' @keywords internal
#
#' @description Create a file name from a character vectors.
#'
#' @param ...         A vector of characters that will be concatenated.
#' @param filenames   Named parameter to create file name. Note: There is
#'                    a difference between \code{dots} and \code{filenames}
#'                    parameters. The \code{filenames} parameter just
#'                    concatenated the provided names, while \code{dots}
#'                    recycled values for each provided vector.
#'                    A vector of characters that will be concatenated.
#' @param sep         A character with a file name separator.
#' @param ext         A character with the extension of file.
#' @param output_dir  A character with the output directory to be concatenated.
#' @param create_dir  A boolean indicating if directory should be created.
#'
#' @return A character with the file name.
.create_filename <- function(...,
                             filenames = NULL,
                             sep = "_",
                             ext = NULL,
                             output_dir = NULL,
                             create_dir = FALSE) {
    filenames_lst <- list(...)

    if (length(filenames_lst) == 0) {
        stopifnot(!is.null(filenames))
        filenames_lst <- as.list(filenames)
    }

    filenames <- do.call(paste, c(filenames_lst, sep = sep))

    if (!is.null(ext)) {
        # remove extension final point
        ext <- gsub("^[.*]*", "\\1", ext)

        filenames <- paste(filenames, ext, sep = ".")
    }

    if (!is.null(output_dir)) {
        if (!dir.exists(output_dir) && !create_dir) {
            stop("Invalid output_dir")
        }
        if (!dir.exists(output_dir) && create_dir) {
            dir.create(output_dir)
        }

        filenames <- file.path(output_dir, filenames)
    }

    return(filenames)
}

#' @title Informs if sits tests should run
#'
#' @name sits_active_tests
#'
#' @description
#' This function informs if sits test should run.
#' Useful to avoid running slow tests in CRAN environment.
#' Behaviour controlled by environmental variable R_CONFIG_ACTIVE_TESTS
#' @return TRUE/FALSE
#' @examples
#' if (sits_run_examples()) {
#' # unset active tests
#' Sys.setenv("R_CONFIG_ACTIVE_TESTS"="NO")
#' # result should be false
#' isFALSE(sits_active_tests())
#' # set active tests
#' Sys.setenv("R_CONFIG_ACTIVE_TESTS"="YES")
#' # result should be true
#' isTRUE(sits_active_tests())
#' }
#'
#' @export
sits_active_tests <- function() {
    return(Sys.getenv("R_CONFIG_ACTIVE_TESTS", unset = "NO") != "NO")
}

#' @title Informs if sits examples should run
#'
#' @name sits_run_examples
#'
#' @description
#' This function informs if sits examples should run.
#' This is useful to avoid running slow examples in CRAN environment.
#'
#' @return A logical value
#' @examples
#' if (sits_run_examples()) {
#' # set active tests
#' #' Sys.setenv("R_CONFIG_RUN_EXAMPLES"="YES")
#' # result should be true
#' isTRUE(sits_run_examples())
#' # unset active tests
#' Sys.setenv("R_CONFIG_RUN_EXAMPLES"="NO")
#' # result should be false
#' isFALSE(sits_run_examples())
#' }
#'
#'

#' @export
sits_run_examples <- function() {
    return(Sys.getenv("R_CONFIG_RUN_EXAMPLES", unset = "NO") != "NO")
}
