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
        predict_fun <- function() {
            # Check memory and multicores
            # Get block size
            block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
            # Check minimum memory needed to process one block
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
                    band = "class",
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
