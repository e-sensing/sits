.bayts_create_stats <- function(samples, stats) {
    if (.has(samples)) {
        bands <- .samples_bands(samples)
        # Create mean and sd columns for each band
        samples <- dplyr::group_by(.ts(samples), .data[["label"]])
        samples <- dplyr::summarise(samples, dplyr::across(
            dplyr::matches(bands), list(mean = mean, sd = stats::sd))
        )
        # Transform to long form
        names_prefix <- NULL
        if (length(bands) > 1) {
            names_prefix <- paste0(bands, collapse = ",")
        }
        stats <- samples |>
            tidyr::pivot_longer(
                cols = dplyr::ends_with(c("mean", "sd")),
                names_sep = "_",
                names_prefix = names_prefix,
                names_to = c("bands", "stats"),
                cols_vary = "fastest") |>
            tidyr::pivot_wider(
                names_from = bands
            )
        # To convert split tibbles into matrix
        stats <- lapply(
            split(stats[, bands], stats[["stats"]]), as.matrix
        )
        return(stats)

    }
    .check_null(
        stats, msg = paste0("Invalid null parameter.",
                            "'stats' must be a valid value.")
    )
    bands <- setdiff(colnames(stats), c("stats", "label"))
    stats <- lapply(
        split(stats[, bands], stats[["stats"]]), as.matrix
    )
    return(stats)
}
