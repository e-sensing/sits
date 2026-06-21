#' @title Encode time series or data cubes
#' @name sits_encode
#' @description
#' Encodes time series or raster data cubes using a pre-trained deep
#' learning encoder created by
#' \code{\link[sits]{sits_pre_train}}.
#'
#' The function dispatches on the class of \code{data} and supports two
#' main workflows:
#' \itemize{
#'   \item For time series input (objects of class \code{"sits"}), the
#'   method \code{\link[sits]{sits_encode.sits}} is used. The output is a
#'   tibble with the original samples augmented by their encoded
#'   representations.
#'   \item For raster data cubes (objects of class
#'   \code{"raster_cube"}), the method
#'   \code{\link[sits]{sits_encode.raster_cube}} is used. The output is an
#'   embeddings cube, with the same tiling as the input cube and one band
#'   per embedding dimension.
#' }
#'
#' @param data Input data to be encoded. Either a set of time series
#'   (tibble of class \code{"sits"}) or a regular raster data cube
#'   (tibble of class \code{"raster_cube"}).
#' @param encoder Torch model trained by
#'   \code{\link[sits]{sits_pre_train}}.
#' @param ... Additional arguments passed to the corresponding
#'   \code{sits_encode} method, depending on the class of \code{data}.
#'
#' @return
#' A tibble of encoded time series (class \code{"sits"}) or an embeddings
#' data cube (class \code{"embeddings_cube"}), depending on the input
#' type.
#'
#' @note
#' A typical \pkg{sits} encoding workflow includes:
#' \enumerate{
#'   \item Selecting an ARD image collection with
#'     \code{\link[sits]{sits_cube}}.
#'   \item Optionally copying data locally using
#'     \code{\link[sits]{sits_cube_copy}}.
#'   \item Creating a regular data cube with
#'     \code{\link[sits]{sits_regularize}}.
#'   \item Optionally deriving additional indices with
#'     \code{\link[sits]{sits_apply}}.
#'   \item Extracting time series with
#'     \code{\link[sits]{sits_get_data}}.
#'   \item Training an encoder using
#'     \code{\link[sits]{sits_pre_train}}.
#'   \item Encoding data with \code{sits_encode}.
#' }
#'
#' Currently supported pre-trained encoders include:
#' \itemize{
#'   \item Masked autoencoder:
#'     \code{\link[sits]{sits_mae}}.
#'   \item Barlow Twins self-supervised encoder:
#'     \code{\link[sits]{sits_barlow_twins_network}}.
#'   \item Contrastive (triplet-based) encoder:
#'     \code{\link[sits]{sits_contrastive_network}}.
#' }
#'
#' For detailed examples and usage, see the \pkg{sits} documentation at
#' \url{https://e-sensing.github.io/sitsbook/}.
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @export
sits_encode <- function(data, encoder, ...) {
    UseMethod("sits_encode", data)
}

#' @title Encode a set of time series
#' @name sits_encode.sits
#' @description
#' Encodes a \pkg{sits} time-series tibble using a pre-trained deep
#' learning encoder returned by \code{\link[sits]{sits_pre_train}}.
#'
#' This method is dispatched when \code{data} is a tibble of class
#' \code{"sits"}. The output preserves the input structure and replaces
#' the \code{time_series} column with the corresponding embeddings.
#'
#' @param data Set of time series (tibble of class \code{"sits"}).
#' @param encoder Encoder closure trained by
#'   \code{\link[sits]{sits_pre_train}} (class \code{"sits_encoder"}).
#' @param ... Additional arguments passed to lower-level encoding
#'   routines.
#' @param filter_fn Optional smoothing filter applied to each time series
#'   before encoding.
#' @param impute_fn Imputation function used to interpolate missing
#'   values in each time series (default:
#'   \code{\link[sits]{impute_linear}}).
#' @param multicores Integer. Number of CPU cores used for processing
#'   (minimum 1, maximum 2048).
#' @param gpu_memory Numeric. GPU memory available for encoding (in GB).
#' @param batch_size Integer. Batch size used when encoding on GPU.
#' @param progress Logical. If \code{TRUE}, show a progress bar.
#'
#' @return
#' A tibble of class \code{"sits"} with \code{time_series} containing the
#' embeddings produced by \code{encoder}.
#'
#' @note
#' The \code{filter_fn} parameter specifies a smoothing filter applied to
#' each time series to reduce noise. Supported options include the
#' Savitzky--Golay filter (\code{\link[sits]{sits_sgolay}}) and the
#' Whittaker filter (\code{\link[sits]{sits_whittaker}}). For consistent
#' results, apply the same filter during pre-training.
#'
#' The \code{impute_fn} parameter defines a one-dimensional imputation
#' function used to interpolate missing values. Users may provide custom
#' functions that follow the same contract.
#'
#' The encoding backend is selected automatically. When GPU execution is
#' available and the encoder supports it, \code{gpu_memory} and
#' \code{batch_size} control how predictors are partitioned and sent to
#' the GPU. It is recommended to leave at least 1 GB of free GPU memory
#' for model and runtime overhead.
#'
#' On systems with unified CPU/GPU memory (e.g., Apple M-series),
#' conservative settings for \code{gpu_memory} and \code{batch_size} may
#' be required to avoid memory contention.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Pre-train an encoder and encode a subset of samples
#'     enc <- sits_pre_train(
#'         samples = samples_modis_ndvi,
#'         encoder_method = sits_mae(mask_ratio = 0.5)
#'     )
#'
#'     point_ndvi <- sits_select(point_mt_6bands, bands = c("NDVI"))
#'     point_emb <- sits_encode(data = point_ndvi, encoder = enc)
#'     plot(point_emb)
#' }
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @export
sits_encode.sits <- function(data,
                             encoder,
                             ...,
                             filter_fn = NULL,
                             impute_fn = impute_linear(),
                             multicores = 2L,
                             gpu_memory = 4L,
                             batch_size = 2L^gpu_memory,
                             progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_encode_sits")
    # Pre-conditions
    .check_samples_embeddings(data)
    .check_is_sits_encoder(encoder)
    .check_model_has_stats(encoder)
    .check_model_has_bands(encoder, .samples_bands(data))
    .check_int_parameter(multicores, min = 1L, max = 2048L)
    progress <- .message_progress(progress)
    .check_function(impute_fn)
    .check_filter_fn(filter_fn)
    # save batch_size for later use
    sits_env[["batch_size"]] <- batch_size
    # Update multicores
    multicores2 <- multicores
    multicores <- .ml_update_multicores(encoder, multicores)
    if (multicores != multicores2) {
        .parallel_force_multicores(multicores)
        on.exit(.parallel_force_multicores()) # restore to default
    }
    # Do classification
    .encode_ts(
        samples = data,
        encoder = encoder,
        filter_fn = filter_fn,
        impute_fn = impute_fn,
        multicores = multicores,
        gpu_memory = gpu_memory,
        progress = progress
    )
}

#' @title Encode a regular raster cube
#' @name sits_encode.raster_cube
#' @description
#' Encodes a regular raster data cube using a pre-trained deep learning
#' encoder returned by \code{\link[sits]{sits_pre_train}}. This method is
#' dispatched when \code{data} is a tibble of class \code{"raster_cube"}.
#'
#' The output is an embeddings cube with the same tiling as the input
#' cube. Each tile is written as a multiband raster, where each band
#' corresponds to one embedding dimension produced by the encoder.
#'
#' @param data Regular raster data cube to be encoded (tibble of class
#'   \code{"raster_cube"}).
#' @param encoder Encoder closure returned by
#'   \code{\link[sits]{sits_pre_train}} (class \code{"sits_encoder"}).
#' @param ... Additional arguments passed to lower-level encoding
#'   routines.
#' @param roi Optional region of interest used to restrict processing.
#'   It may be provided as: (1) a path to a polygon shapefile; (2) an
#'   \code{sf} object with \code{POLYGON} or \code{MULTIPOLYGON} geometry;
#'   (3) a named bounding box vector in WGS84 with \code{xmin}, \code{xmax},
#'   \code{ymin}, \code{ymax}; or (4) a named lon/lat bounding box vector
#'   with \code{lon_min}, \code{lon_max}, \code{lat_min}, \code{lat_max}.
#' @param exclusion_mask Optional areas to exclude from encoding. It may
#'   be provided as a polygon shapefile path or an \code{sf} object.
#' @param filter_fn Optional smoothing filter applied to each pixel time
#'   series before encoding.
#' @param impute_fn Imputation function used to interpolate missing
#'   values in each pixel time series (default:
#'   \code{\link[sits]{impute_linear}}).
#' @param start_date Optional start date for temporal filtering
#'   (YYYY-MM-DD). Defaults to the cube start date.
#' @param end_date Optional end date for temporal filtering (YYYY-MM-DD).
#'   Defaults to the cube end date.
#' @param memsize Integer. Memory available for processing in GB
#'   (minimum 1).
#' @param multicores Integer. Number of CPU cores used for processing
#'   (minimum 1).
#' @param gpu_memory Integer. GPU memory available for encoding in GB
#'   (minimum 1).
#' @param batch_size Integer. Batch size used when encoding on GPU.
#' @param output_dir Directory where output files will be written.
#' @param verbose Logical. If \code{TRUE}, print processing time
#'   information.
#' @param progress Logical. If \code{TRUE}, show a progress bar.
#'
#' @return
#' An embeddings cube (tibble of class \code{"embeddings_cube"}) written
#' to \code{output_dir}, with one multiband raster per tile date.
#'
#' @note
#' The \code{roi} parameter restricts processing to a spatial subset of
#' the cube. If provided, tiles are spatially filtered and blocks outside
#' the ROI are skipped.
#'
#' The \code{filter_fn} parameter specifies a smoothing filter applied to
#' each pixel time series. Supported options include the Savitzky--Golay
#' filter (\code{\link[sits]{sits_sgolay}}) and the Whittaker filter
#' (\code{\link[sits]{sits_whittaker}}). For consistent results, apply
#' the same filter during pre-training.
#'
#' The \code{impute_fn} parameter defines a one-dimensional function used
#' to interpolate missing values. By default,
#' \code{\link[sits]{impute_linear}} is used, but custom functions may be
#' provided.
#'
#' The \code{memsize} and \code{multicores} parameters are used to
#' determine an optimal block size and safe parallelism level. When GPU
#' encoding is available and supported by the encoder, \code{gpu_memory}
#' and \code{batch_size} influence how predictor matrices are partitioned
#' for GPU execution.
#'
#' It is recommended to leave at least 1 GB of free GPU memory to
#' accommodate model and runtime overhead. On systems with unified
#' CPU/GPU memory (e.g., Apple M-series), conservative settings for
#' \code{gpu_memory} and \code{batch_size} may be required to avoid memory
#' contention.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Create a regular cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'
#'     # Pre-train an encoder and encode a cube
#'     enc <- sits_pre_train(
#'         samples = samples_modis_ndvi,
#'         encoder_method = sits_mae(mask_ratio = 0.5)
#'     )
#'     emb_cube <- sits_encode(
#'         data = cube,
#'         encoder = enc,
#'         output_dir = tempdir(),
#'     )
#'     plot(emb_cube)
#' }
#'
#' @author Alexandre Assuncao, \email{alexcarssuncao@@gmail.com}
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @export
sits_encode.raster_cube <- function(data,
                                    encoder, ...,
                                    roi = NULL,
                                    exclusion_mask = NULL,
                                    filter_fn = NULL,
                                    impute_fn = impute_linear(),
                                    start_date = NULL,
                                    end_date = NULL,
                                    memsize = 8L,
                                    multicores = 2L,
                                    gpu_memory = 4L,
                                    batch_size = 2L^gpu_memory,
                                    output_dir,
                                    verbose = FALSE,
                                    progress = TRUE) {
    # set caller for error messages
    .check_set_caller("sits_encode_raster")
    # preconditions
    .check_is_raster_cube(data)
    .check_cube_is_regular(data)
    .check_is_sits_encoder(encoder)
    .check_model_has_stats(encoder)
    .check_int_parameter(memsize, min = 1L)
    .check_int_parameter(multicores, min = 1L)
    .check_int_parameter(gpu_memory, min = 1L)
    .check_output_dir(output_dir)
    # preconditions - impute and filter functions
    .check_function(impute_fn)
    .check_filter_fn(filter_fn)
    # documentation mode? progress is FALSE
    progress <- .message_progress(progress)
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)
    # Spatial filter
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
        data <- .cube_filter_spatial(cube = data, roi = roi)
    }
    # Exclusion mask
    if (.has(exclusion_mask)) {
        exclusion_mask <- .mask_as_sf(exclusion_mask)
    }
    # Temporal filter
    start_date <- .default(start_date, .cube_start_date(data))
    end_date <- .default(end_date, .cube_end_date(data))
    data <- .cube_filter_interval(
        cube = data, start_date = start_date, end_date = end_date
    )
    # save batch_size for later use
    sits_env[["batch_size"]] <- batch_size

    # Retrieve the samples from the model
    samples <- .ml_samples(encoder)
    # Do the samples and tile match their timeline length?
    .check_match_timeline(samples = samples, tile = data)
    # Do the samples and tile match their bands?
    .check_match_bands(samples = samples, tile = data)

    # By default, base bands is null.
    base_bands <- NULL
    if (.cube_is_base(data)) {
        # Get base bands
        base_bands <- intersect(
            .ml_bands(encoder), .cube_bands(.cube_base_info(data))
        )
    }
    # get non-base bands
    bands <- setdiff(.ml_bands(encoder), base_bands)

    # Update multicores for models with internal parallel processing
    multicores2 <- multicores
    multicores <- .ml_update_multicores(encoder, multicores)
    if (multicores != multicores2) {
        .parallel_force_multicores(multicores)
        on.exit(.parallel_force_multicores()) # restore to default
    }

    # The following functions define optimal parameters for parallel processing
    # Get block size
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(data)))
    # Check minimum memory needed to process one block
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0),
        npaths = (
            length(.tile_paths(data, bands)) +
                length(.ml_labels(encoder)) +
                ifelse(
                    test = .cube_is_base(data),
                    yes = length(.tile_paths(.cube_base_info(data), base_bands)),
                    no = 0
                )
        ),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(.tile(data)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    started <- .parallel_start(
        workers = multicores, log = verbose,
        output_dir = output_dir
    )
    if (started) {
        on.exit(.parallel_stop(), add = TRUE)
    }
    # Show processing time information
    start_time <- .encode_verbose_start(verbose, block)
    on.exit(.encode_verbose_end(verbose, start_time), add = TRUE)
    # Classification
    # Process each tile sequentially
    emb_cube <- .cube_foreach_tile(data, function(tile) {
        # encode the data
        .encode_tile(
            tile = tile,
            out_bands = .encode_band_names(encoder),
            bands = bands,
            base_bands = base_bands,
            encoder = encoder,
            block = block,
            roi = roi,
            filter_fn = filter_fn,
            impute_fn = impute_fn,
            output_dir = output_dir,
            verbose = verbose,
            progress = progress
        )
    })
    .cube_set_class(emb_cube, c("embeddings_cube", class(emb_cube)))
}

#' @rdname sits_encode
#' @export
sits_encode.tbl_df <- function(data, encoder, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_encode_tbl_df"))
    }
    sits_encode(data, encoder, ...)
}
#' @rdname sits_encode
#' @export
sits_encode.derived_cube <- function(data, encoder, ...) {
    stop(.conf("messages", "sits_encode_derived_cube"))
}
#' @rdname sits_encode
#' @export
sits_encode.default <- function(data, encoder, ...) {
    data <- tibble::as_tibble(data)
    if (all(.conf("sits_cube_cols") %in% colnames(data))) {
        data <- .cube_find_class(data)
    } else if (all(.conf("sits_tibble_cols") %in% colnames(data))) {
        class(data) <- c("sits", class(data))
    } else {
        stop(.conf("messages", "sits_encode_default"))
    }
    sits_encode(data, encoder, ...)
}
