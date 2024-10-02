.radd_calc_tile <- function(tile,
                            band,
                            roi,
                            pdf_fn,
                            mean_stats,
                            sd_stats,
                            deseasonlize,
                            threshold,
                            chi,
                            bwf,
                            block,
                            impute_fn,
                            start_date,
                            end_date,
                            output_dir,
                            version,
                            progress) {
    # Output file
    out_file <- .file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume feature
    if (file.exists(out_file)) {
        if (.check_messages()) {
            message("Recovery: tile '", tile[["tile"]], "' already exists.")
            message(
                "(If you want to produce a new image, please ",
                "change 'output_dir' or 'version' parameters)"
            )
        }
        class_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            derived_class = "radd_cube",
            labels = NULL,
            update_bbox = TRUE
        )
        return(class_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(tile = tile, overlap = 0, block = block)
    # By default, update_bbox is FALSE
    update_bbox <- FALSE
    if (.has(roi)) {
        # How many chunks there are in tile?
        nchunks <- nrow(chunks)
        # Intersecting chunks with ROI
        chunks <- .chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
        # Should bbox of resulting tile be updated?
        update_bbox <- nrow(chunks) != nchunks
    }
    # Get the quantile values for each band
    quantile_values <- .radd_calc_quantile(
        tile = tile,
        deseasonlize = deseasonlize,
        impute_fn = impute_fn
    )
    # Get the number of dates in the timeline
    tile_tl <- .tile_timeline(tile)
    n_times <- length(tile_tl)
    # Get the start and end time of the detection period
    start_detection <- 0
    end_detection <- n_times + 1
    if (.has(start_date) && .has(end_date)) {
        filt_idxs <- which(tile_tl >= start_date & tile_tl <= end_date)
        start_detection <- min(filt_idxs) - 1
        end_detection <- max(filt_idxs)
    }
    # Transform tile timeline into a year day
    tile_yday <- .radd_convert_date_yday(tile_tl)
    # Process jobs in parallel
    block_files <- .jobs_map_parallel_chr(chunks, function(chunk) {
        # Job block
        block <- .block(chunk)
        # Block file name
        block_file <- .file_block_name(
            pattern = .file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Read and preprocess values
        values <- .classify_data_read(
            tile = tile,
            block = block,
            bands = .tile_bands(tile),
            ml_model = NULL,
            impute_fn = impute_fn,
            filter_fn = NULL,
            base_bands = NULL
        )
        # Calculate the probability of a Non-Forest pixel
        values <- C_radd_calc_nf(
            ts = values,
            mean = mean_stats,
            sd = sd_stats,
            n_times = n_times,
            quantile_values = quantile_values,
            bwf = bwf
        )
        # Apply detect changes in time series
        values <- C_radd_detect_changes(
            p_res = values,
            start_detection = start_detection,
            end_detection = end_detection
        )
        # Get date that corresponds to the index value
        values <- tile_yday[as.character(values)]
        # Prepare values to be saved
        band_conf <- .conf_derived_band(
            derived_class = "radd_cube", band = band
        )
        # Prepare and save results as raster
        .raster_write_block(
            files = block_file, block = block, bbox = .bbox(chunk),
            values = values, data_type = .data_type(band_conf),
            missing_value = 0,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned value
        block_file
    }, progress = progress)
    # Merge blocks into a new class_cube tile
    class_tile <- .tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = NULL,
        base_tile = tile,
        block_files = block_files,
        derived_class = "radd_cube",
        multicores = .jobs_multicores(),
        update_bbox = FALSE
    )
    # Return class tile
    class_tile
}

.radd_detect_events <- function(data,
                                threshold = 0.5,
                                start_date = NULL,
                                end_date = NULL) {
    data <- .radd_filter_changes(
        data = data, threshold = threshold, start_date = start_date,
        end_date = end_date
    )
    data <- .radd_add_dummy(data)

    data <- .radd_start_monitoring(data, threshold)
}

.radd_start_monitoring <- function(data, threshold, chi = 0.9) {
    prob_nf <- tidyr::unnest(data, "prob_nf")
    prob_nf <- dplyr::select(
        prob_nf, dplyr::all_of(c("sample_id", "NF", "Index", "Flag", "PChange"))
    )
    prob_nf <- dplyr::group_by(prob_nf, .data[["sample_id"]])
    prob_nf[prob_nf$NF < threshold, "Flag"] <- "0"
    prob_nf <- dplyr::group_modify(prob_nf, ~ {
        # Filter observations to monitoring and remove first dummy data
        valid_idxs <- which(.x$NF >= threshold)[-1] - 1
        for (r in seq_len(length(valid_idxs))) {
            for (t in seq(valid_idxs[r], nrow(.x))) {
                # step 2.1: Update Flag and PChange for current time step (i)
                # (case 1) No confirmed or flagged change:
                if (nrow(.x[t - 1, "Flag"]) > 0 && !is.na(.x[t - 1, "Flag"])[[1]]) {
                    if (.x[t - 1, "Flag"] == "0" || .x[t - 1, "Flag"] == "oldFlag") {
                        i <- 0
                        prior <- .x[t - 1, "NF"]
                        likelihood <- .x[t, "NF"]
                        posterior <- .radd_calc_bayes(prior, likelihood)
                        .x[t, "Flag"] <- "Flag"
                        .x[t, "PChange"] <- posterior
                    }
                    # (case 2) Flagged change at previous time step: update PChange
                    if (.x[t - 1, "Flag"] == "Flag") {
                        prior <- .x[t - 1, "PChange"]
                        likelihood <- .x[t, "NF"]
                        posterior <- .radd_calc_bayes(prior, likelihood)
                        .x[t, "Flag"] <- "Flag"
                        .x[t, "PChange"] <- posterior
                        i <- i + 1
                    }
                }
                # step 2.2: Confirm and reject flagged changes
                if (nrow(.x[t - 1, "Flag"]) > 0 && !is.na(.x[t, "Flag"]) && .x[t, "Flag"] == "Flag") {
                    if ((i > 0)) {
                        if (.x[t, "PChange"] < 0.5) {
                            .x[(t - i):t, "Flag"] <- "0"
                            .x[(t - i), "Flag"] <- "oldFlag"
                            break
                        }
                    }
                }
                # confirm change in case PChange >= chi
                if (nrow(.x[t - 1, "Flag"]) > 0 &&
                    !is.na(.x[t, "PChange"]) &&
                    .x[t, "PChange"] >= chi) {
                    if (.x[t, "NF"] >= threshold) {
                        min_idx <- min(which(.x$Flag == "Flag"))
                        .x[min_idx:t, "Flag"] <- "Change"
                        return(.x)
                    }
                }
            }
        }
        return(.x)
    })
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <-  tidyr::nest(
        prob_nf, prob_nf = -"#.."
    )
    data[["prob_nf"]] <- prob_nf[["prob_nf"]]
    data
}

.radd_add_dummy <- function(data) {
    prob_nf <- tidyr::unnest(data, "prob_nf")
    prob_nf <- dplyr::select(
        prob_nf, dplyr::all_of(c("sample_id", "NF", "Index", "Flag", "PChange"))
    )
    prob_nf <- dplyr::group_by(prob_nf, .data[["sample_id"]])
    prob_nf <- dplyr::group_modify(prob_nf, ~ {
        tibble::add_row(
            .data = .x,
            NF = 0.5,
            Index = min(.x$Index) - 1,
            Flag = "0",
            PChange = NA,
            .before = 1
        )
    })
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <-  tidyr::nest(
        prob_nf, prob_nf = -"#.."
    )
    data[["prob_nf"]] <- prob_nf[["prob_nf"]]
    data
}

.radd_filter_changes <- function(data, threshold, start_date, end_date) {
    prob_nf <- tidyr::unnest(data, "prob_nf")
    prob_nf <- dplyr::select(
        prob_nf, dplyr::all_of(c("sample_id", "NF", "Index", "Flag", "PChange"))
    )
    data[["sample_id"]] <- unique(prob_nf[["sample_id"]])
    if (!.has(start_date)) {
        start_date <- .ts_start_date(.ts(data))
    }
    if (!.has(end_date)) {
        end_date <- .ts_end_date(.ts(data))
    }
    prob_nf <- dplyr::filter(
        prob_nf, Index >= start_date & Index <= end_date
    )
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <- tidyr::nest(
        prob_nf, prob_nf = -"#.."
    )
    data <- data[which(data[["sample_id"]] %in% prob_nf[["#.."]]), ]
    data[["sample_id"]] <- NULL
    data[["prob_nf"]] <- prob_nf[["prob_nf"]]
    data
}

.radd_calc_pnf <- function(data, pdf_fn, stats_layer) {
    samples_labels <- stats_layer[["label"]]
    bands <- .samples_bands(data)
    # We need to calculate for the first to update others
    band <- bands[[1]]
    prob_nf <- .radd_calc_pnf_band(
        data = data,
        pdf_fn = pdf_fn,
        stats_layer = stats_layer,
        band = band,
        labels = samples_labels
    )
    # We need to update de probability of non-forest
    for (b in setdiff(bands, band)) {
        prob_nf <<- .radd_calc_pnf_band(
            data = data,
            pdf_fn = pdf_fn,
            stats_layer = stats_layer,
            band = b,
            labels = samples_labels,
            pnf = prob_nf
        )
    }
    # Add Flag and Pchange columns
    prob_nf[, c("Flag", "PChange")] <- NA
    # Nest each NF probability
    prob_nf[["#.."]] <- prob_nf[["sample_id"]]
    prob_nf <- tidyr::nest(prob_nf, prob_nf = -"#..")
    data$prob_nf <- prob_nf$prob_nf
    # Return the probability of NF updated
    return(data)
}

.radd_calc_pnf_band <- function(data, pdf_fn, stats_layer, band, labels, pnf = NULL) {
    ts_band <- .ts_select_bands(.ts(data), bands = band)
    ts_band <- dplyr::group_by(ts_band, .data[["sample_id"]])
    prob_nf <- dplyr::group_modify(ts_band, ~ {
        # Estimate pdf for each samples labels
        # TODO: remove map and add two vectors
        pdf <- purrr::map_dfc(labels, function(label) {
            label_pdf <- pdf_fn(
                .x[[band]],
                mean = .radd_select_stats(stats_layer, label, band, "mean"),
                sd = .radd_select_stats(stats_layer, label, band, "sd")
            )
            tibble::tibble(label_pdf, .name_repair = ~ label)
        })
        pdf[pdf[["NF"]] < 1e-10000, "NF"] <- 0
        # Calculate conditional probability for NF
        pdf[pdf[["NF"]] > 0, "NF"] <- .radd_calc_prob(
            p1 = pdf[pdf[["NF"]] > 0, "NF"],
            p2 = pdf[pdf[["NF"]] > 0, "F"]
        )
        # Apply body weight function
        pdf <- .radd_apply_bwf(pdf)
        if (.has(pnf)) {
            pnf <- dplyr::filter(pnf, sample_id == .y$sample_id)
            pdf[, "NF"] <- .radd_calc_bayes(pdf[, "NF"], pnf[, "NF"])
        }
        # Return NF conditional probability
        pdf[, "NF"]
    })
    # Add Index column to probability of NF
    prob_nf[["Index"]] <- ts_band[["Index"]]
    prob_nf
}

.radd_calc_prob <- function(p1, p2) {
    p1 / (p1 + p2)
}

.radd_calc_bayes <- function(prior, post) {
    return((prior * post) / ((prior * post) + ((1 - prior) * (1 - post))))
}

.radd_apply_bwf <- function(tbl) {
    tbl[tbl[["NF"]] < 0, "NF"] <- 0
    tbl[tbl[["NF"]] > 1, "NF"] <- 1
    tbl
}

.radd_select_stats <- function(stats_layer, label, band, stats) {
    stats_layer <- dplyr::filter(stats_layer, label == !!label)
    band_name <- paste(band, stats, sep = "_")
    .as_dbl(dplyr::select(stats_layer, dplyr::matches(band_name)))
}

.pdf_fun <- function(dist_name) {
    switch(
        dist_name,
        "gaussian" = dnorm,
        "weibull" = dweibull
    )
}

.radd_calc_quantile <- function(tile, deseasonlize, impute_fn) {
    if (!.has(deseasonlize)) {
        return(matrix(NA))
    }

    tile_bands <- .tile_bands(tile, FALSE)
    quantile_values <- purrr::map(tile_bands, function(tile_band) {
        tile_paths <- .tile_paths(tile, bands = tile_band)
        r_obj <- .raster_open_rast(tile_paths)
        quantile_values <- .raster_quantile(
            r_obj, quantile = deseasonlize, na.rm = TRUE
        )
        quantile_values <- impute_fn(t(quantile_values))
        # Fill with zeros remaining NA pixels
        quantile_values <- C_fill_na(quantile_values, 0)
        # Apply scale
        band_conf <- .tile_band_conf(tile = tile, band = tile_band)
        scale <- .scale(band_conf)
        if (.has(scale) && scale != 1) {
            quantile_values <- quantile_values * scale
        }
        offset <- .offset(band_conf)
        if (.has(offset) && offset != 0) {
            quantile_values <- quantile_values + offset
        }
        unname(quantile_values)
    })
    do.call(cbind, quantile_values)
}

.radd_convert_date_yday <- function(tile_tl) {
    tile_yday <-  lubridate::yday(lubridate::date(tile_tl))
    tile_yday <- as.numeric(paste0(lubridate::year(tile_tl), tile_yday))
    tile_yday <- c(0, tile_yday)
    names(tile_yday) <- seq.int(
        from = 0, to = length(tile_yday) - 1, by = 1
    )
    tile_yday
}

.radd_create_stats <- function(samples, stats) {
    if (.has(samples)) {
        bands <- .samples_bands(samples)
        # Create mean and sd columns for each band
        samples <- dplyr::group_by(.ts(samples), .data[["label"]])
        samples <- dplyr::summarise(samples, dplyr::across(
            dplyr::matches(bands), list(mean = mean, sd = sd))
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
        # To convert splitted tibbles into matrix
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
