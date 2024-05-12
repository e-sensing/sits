sits_radd <- function(data, pdf, ...,
                      stats_layer = NULL,
                      chi = 0.9,
                      start_date = NULL,
                      end_date = NULL) {

    UseMethod("sits_radd", data)
}


sits_radd.sits <- function(data,
                           pdf = "gaussian",
                           ...,
                           stats_layer = NULL,
                           chi = 0.9,
                           start_date = NULL,
                           end_date = NULL) {
    # Training function
    train_fun <- function(data) {
        # Check 'pdf' parameter
        .check_chr_parameter(pdf)
        # Check 'chi' parameter
        .check_num_min_max(chi, min = 0.1, max = 1)
        # Check 'start_date' parameter
        .check_date_parameter(start_date)
        # Check 'end_date' parameter
        .check_date_parameter(end_date)

        # Get pdf function
        pdf_fn <- .pdf_fun(pdf)
        # Create stats layer
        if (!.has(stats_layer)) {
            stats_layer <- .radd_create_stats(data)
        }
        # Calculate probability for NF
        data <- .radd_calc_pnf(
            data = data,
            pdf_fn = pdf_fn,
            stats_layer = stats_layer
        )
        predict_fun <- function() {
            # Now we need to detected the changes
            data <- .radd_detect_events(
                data = data,
                threshold = 0.5,
                start_date = start_date,
                end_date = end_date
            )
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "radd_model", "sits_model", class(predict_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(data, train_fun)
    return(result)
}

sits_radd.raster_cube <- function(data,
                                  pdf = "gaussian",
                                  ...,
                                  stats_layer = NULL,
                                  chi = 0.9,
                                  impute_fn = impute_linear(),
                                  memsize = 8L,
                                  multicores = 2L,
                                  version = "v1",
                                  output_dir) {
    # Training function
    train_fun <- function(data) {
        # Check 'pdf' parameter
        .check_chr_parameter(pdf)
        # Check 'chi' parameter
        .check_num_min_max(chi, min = 0.1, max = 1)
        # Check 'start_date' parameter
        .check_date_parameter(start_date)
        # Check 'end_date' parameter
        .check_date_parameter(end_date)
        .check_memsize(memsize, min = 1, max = 16384)
        .check_multicores(multicores, min = 1, max = 2048)
        .check_output_dir(output_dir)
        version <- tolower(.check_version(version))

        # Get default proc bloat
        proc_bloat <- .conf("processing_bloat_cpu")


        # Get pdf function
        pdf_fn <- .pdf_fun(pdf)

        # Create stats layer
        # TODO: i will remove this line, if user does not provide
        # stats layer will give an error
        if (!.has(stats_layer)) {
            stats_layer <- .radd_create_stats(data)
        }

        # Check memory and multicores
        # Get block size
        block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
        # Check minimum memory needed to process one block
        # TODO: verify npaths param
        job_memsize <- .jobs_memsize(
            job_size = .block_size(block = block, overlap = 0),
            npaths = length(.tile_paths(data)),
            nbytes = 8,
            proc_bloat = proc_bloat
        )
        # Update multicores parameter
        multicores <- .jobs_max_multicores(
            job_memsize = job_memsize,
            memsize = memsize,
            multicores = multicores
        )
        # Update block parameter
        block <- .jobs_optimal_block(
            job_memsize = job_memsize,
            block = block,
            image_size = .tile_size(.tile(data)),
            memsize = memsize,
            multicores = multicores
        )
        # Terra requires at least two pixels to recognize an extent as valid
        # polygon and not a line or point
        block <- .block_regulate_size(block)
        # Prepare parallel processing
        .parallel_start(workers = multicores)
        on.exit(.parallel_stop(), add = TRUE)

        # Calculate the probability of Non-Forest
        # Process each tile sequentially
        probs_cube <- .cube_foreach_tile(data, function(tile) {
            # Classify the data
            probs_tile <- .radd_calc_tile(
                tile = tile,
                band = "probs",
                pdf_fn = pdf_fn,
                stats_layer = stats_layer,
                block = block,
                impute_fn = impute_fn,
                output_dir = output_dir,
                version = version,
                progress = TRUE
            )
            return(probs_tile)
        })

        # Calculate probability for NF
        data <- .radd_calc_pnf(
            data = data,
            pdf_fn = pdf_fn,
            stats_layer = stats_layer
        )
        predict_fun <- function(start_date = NULL, end_date = NULL) {
            # Now we need to detected the changes
            data <- .radd_detect_events(
                data = data,
                threshold = 0.5,
                start_date = start_date,
                end_date = end_date
            )
        }
        # Set model class
        predict_fun <- .set_class(
            predict_fun, "radd_model", "sits_model", class(predict_fun)
        )
        return(predict_fun)
    }
    # If samples is informed, train a model and return a predict function
    # Otherwise give back a train function to train model further
    result <- .factory_function(data, train_fun)
    return(result)

}

.radd_calc_tile <- function(tile,
                            band,
                            pdf_fn,
                            stats_layer,
                            block,
                            impute_fn,
                            output_dir,
                            version,
                            progress = TRUE) {

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
        probs_tile <- .tile_derived_from_file(
            file = out_file,
            band = band,
            base_tile = tile,
            labels = stats_layer[["label"]],
            derived_class = "probs_cube",
            update_bbox = TRUE
        )
        return(probs_tile)
    }
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = tile,
        overlap = 0,
        block = block
    )
    # Compute fractions probability
    probs_fractions <- 1 / length(stats_layer[["label"]])
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
            filter_fn = NULL
        )
        # Get mask of NA pixels
        na_mask <- C_mask_na(values)
        # Fill with zeros remaining NA pixels
        values <- C_fill_na(values, 0)
        # Used to check values (below)
        input_pixels <- nrow(values)






        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = progress)
}

