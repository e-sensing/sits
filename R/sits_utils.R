.did_you_mean <- function(ref, candidates) {

    # takes only the non-repeated candidates
    candidates <- unique(candidates)

    # compute the levenshtein distance between reference and candidates
    min_dist <- adist(ref, candidates, ignore.case = TRUE, partial = TRUE)[1L, ]
    names(min_dist) <- candidates

    # takes the tree best suggested candidates that are similar with
    # the reference
    suggested <- head(sort(min_dist[min_dist <= ceiling(0.2 * nchar(ref))], ), 3L)

    if (!length(suggested))
        return("")

    quote_sep  <- "'"
    sugg_formated <- paste0(quote_sep, names(suggested), quote_sep)

    sprintf(" Did you mean %s?", paste(sugg_formated, collapse = ", "))
}

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

        if (!dir.exists(output_dir) && !create_dir)
            stop("Invalid output_dir")
        if (!dir.exists(output_dir) && create_dir)
            dir.create(output_dir)

        filenames <- file.path(output_dir, filenames)
    }

    return(filenames)
}
