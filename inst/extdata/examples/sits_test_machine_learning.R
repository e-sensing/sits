library (sits)

matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))

patterns.tb <- sits_patterns(matogrosso.tb)

matches.tb <- sits_TWDTW_matches(matogrosso.tb, patterns.tb, bands = c("ndvi", "evi"))

sits_plot(matches.tb[1,], type = "alignments")

# Get best TWDTW aligniments for each class
matches_distance <- matches.tb[1,] %>%
    dplyr::rowwise() %>%
    dplyr::do(twdtw_distances <-
                  tibble::as_tibble(.$matches[[1]][, c("distance", "label")]) %>%
                  dplyr::group_by(label) %>%
                  dplyr::slice(which.min(distance)) %>%
                  dplyr::ungroup() %>%
                  dplyr::rename(predicted = label)
    )

# Select best match and spread pred to columns
out.tb <- matches.tb[1,] %>%
    dplyr::bind_cols(matches_distance) %>%
    tidyr::unnest(twdtw_distances, .drop = FALSE) %>%
    dplyr::mutate(reference = label) %>%
    tidyr::spread(predicted, distance)

