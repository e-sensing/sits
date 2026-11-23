#' @title Texture of one block of a feature
#' @name .texture_block
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  chunk           Chunk of the block, with its overlap
#' @param  feature         Subset of a data cube containing the input bands
#'                         used in the expression
#' @param  block_file      File of the block
#' @param  window_size     Size of the neighbourhood (if required)
#' @param  angles          The direction angles in radians related to the
#'                         central pixel and its neighbor.
#' @param  expr            Expression to be applied
#' @param  out_band        Output band
#' @param  in_bands        Input bands
#' @param  texture_fn      Factory returning the environment with the glcm_*
#'                         functions (\code{.texture_functions} for C++,
#'                         \code{.texture_torch_functions} for torch).
#'
#' @return                 File of the block
.texture_block <- function(chunk, feature, block_file, window_size, angles,
                           expr, out_band, in_bands, texture_fn) {
    # Get job block
    block <- .block(chunk)
    block_files <- block_file
    .debug_log(event = "start_block", key = "file", value = block_files)
    # Resume processing in case of failure
    if (all(.raster_is_valid(block_files))) {
        .debug_log(event = "end_block", key = "file", value = block_files)
        return(block_files)
    }
    # Get band configuration
    band_conf <- .tile_band_conf(tile = feature, band = out_band)
    if (.has_not(band_conf)) {
        band_conf <- .conf("default_values", "INT2S")
    }
    # Get texture options
    texture_conf <- .conf("texture_options")
    # Read bands data
    values <- .apply_data_read(
        tile = feature, block = block, in_bands = in_bands
    )
    # Fill with zeros remaining NA pixels
    values[[1L]] <- C_fill_na(as.matrix(values[[1L]]), 0.0)
    # Scale values
    scale <- .scale(band_conf)
    values <- values / scale
    max_value <- max(values)
    if (max_value > .max_value(band_conf)) {
        values[values > .max_value(band_conf)] <- .max_value(band_conf) - 1
    }
    min_value <- min(values)
    if (min_value < .min_value(band_conf)) {
        values[values < .min_value(band_conf)] <- .min_value(band_conf) + 1
    }
    # Normalize input values
    values <- .texture_normalize(
        values = values,
        source = c(.min_value(band_conf), .max_value(band_conf)),
        dest = c(.min_value(texture_conf), .max_value(texture_conf))
    )
    # Evaluate expression here
    # Band and kernel evaluation
    .debug_log(
        event = "start_block_kernel", key = "file", value = block_files
    )
    values <- eval(
        expr = expr[[out_band]],
        envir = values,
        enclos = texture_fn(
            window_size = window_size,
            angles = angles,
            img_nrow = block[["nrows"]],
            img_ncol = block[["ncols"]],
            n_grey = .max_value(texture_conf)
        )
    )
    .debug_log(
        event = "end_block_kernel", key = "file", value = block_files
    )
    # Prepare fractions to be saved
    offset <- .offset(band_conf)
    if (.has(offset) && offset != 0.0) {
        values <- values - offset
    }
    # Job crop block
    crop_block <- .block(.chunks_no_overlap(chunk))
    # Prepare and save results as raster
    .debug_log(
        event = "start_block_write", key = "file", value = block_files
    )
    .raster_write_block(
        files = block_files, block = block, bbox = .bbox(chunk),
        values = values, data_type = .data_type(band_conf),
        missing_value = .miss_value(band_conf),
        crop_block = crop_block
    )
    .debug_log(
        event = "end_block_write", key = "file", value = block_files
    )
    .debug_log(event = "end_block", key = "file", value = block_files)
    # Returned block files for each fraction
    block_files
}
#' @title Merge the texture blocks of a feature
#' @name .texture_merge
#' @keywords internal
#' @noRd
#'
#' @param  feature      Subset of a data cube containing the input bands
#' @param  out_band     Output band
#' @param  out_file     File of the texture feature
#' @param  block_files  Files of the blocks
#' @return              Feature with the texture band
.texture_merge <- function(feature, out_band, out_file, block_files) {
    # a merge sent again after a dead worker finds its leftovers
    .file_merge_clean(out_file)
    # Get band configuration
    band_conf <- .tile_band_conf(tile = feature, band = out_band)
    if (.has_not(band_conf)) {
        band_conf <- .conf("default_values", "INT2S")
    }
    # Merge blocks into a new eo_cube tile
    .debug_log(event = "start_tile_merge", key = "file", value = out_file)
    feature <- .tile_eo_merge_blocks(
        files = out_file,
        bands = out_band,
        band_conf = band_conf,
        base_tile = feature,
        block_files = block_files,
        multicores = 1L,
        update_bbox = FALSE
    )
    .debug_log(event = "end_tile_merge", key = "file", value = out_file)
    feature
}
#' @title Jobs of the texture of one feature
#' @name .texture_feature_jobs
#' @keywords internal
#' @noRd
#'
#' @param  feature      Subset of a data cube containing the input bands
#' @param  out_band     Output band
#' @param  block        Individual block that will be processed
#' @param  overlap      Overlap between tiles (if required)
#' @param  output_dir   Directory where image will be save
#' @return Tibble with the columns role, out_file and tile: the blocks,
#'         with the chunk columns and block_file, and then the merge, with
#'         block_files; or one resume job when a valid file of the feature
#'         exists
.texture_feature_jobs <- function(feature, out_band, block, overlap,
                                  output_dir) {
    # Output file
    out_file <- .file_eo_name(
        tile = feature, band = out_band,
        date = .tile_start_date(feature), output_dir = output_dir
    )
    # Resume feature
    if (all(.raster_is_valid(out_file, output_dir = output_dir))) {
        # the recovery message is shown by this process, not by a worker
        .check_recovery()
        # Create tile based on template
        feature <- .tile_eo_from_files(
            files = out_file, fid = .fi_fid(.fi(feature)),
            bands = out_band, date = .tile_start_date(feature),
            base_tile = feature, update_bbox = FALSE
        )
        return(tibble::tibble(
            role = "resume", out_file = out_file, tile = list(feature)
        ))
    }
    # Remove remaining incomplete fractions files
    unlink(out_file)
    # Create chunks as jobs
    chunks <- .tile_chunks_create(
        tile = feature, overlap = overlap, block = block
    )
    blocks <- .chunks_jobs_create(
        chunks = chunks, out_file = out_file, output_dir = output_dir
    )
    blocks[["tile"]] <- list(feature)
    merge <- tibble::tibble(
        role = "merge", out_file = out_file, tile = list(feature),
        block_files = list(blocks[["block_file"]])
    )
    dplyr::bind_rows(blocks, merge)
}
#' @title Jobs of the texture of a cube
#' @name .texture_jobs
#' @keywords internal
#' @noRd
#' @description One group per feature (tile and date), because the dates of
#'   a tile share its name, which \code{.cube_jobs_create()} uses as group.
#' @param  features_cube  Cube with one feature per row
#' @param  ...            Arguments of \code{.texture_feature_jobs()}
#' @return Tibble with the jobs of every feature, in the order of the cube
.texture_jobs <- function(features_cube, ...) {
    slider::slide_dfr(features_cube, function(feature) {
        jobs <- .texture_feature_jobs(feature, ...)
        tibble::add_column(
            jobs,
            group = paste(
                .tile_name(feature), .tile_start_date(feature), sep = "_"
            ),
            .before = 1L
        )
    })
}
#' @title Job of the texture: one block, the merge or the resume of a feature
#' @name .texture_job
#' @keywords internal
#' @noRd
#' @param  job          Row of the jobs of \code{.texture_feature_jobs()}
#' @param  window_size  Size of the neighbourhood (if required)
#' @param  angles       The direction angles in radians
#' @param  expr         Expression to be applied
#' @param  out_band     Output band
#' @param  in_bands     Input bands
#' @param  texture_fn   Factory of the glcm_* functions
#' @return File of the block, or the feature
.texture_job <- function(job, window_size, angles, expr, out_band, in_bands,
                         texture_fn) {
    feature <- job[["tile"]][[1L]]
    switch(job[["role"]],
        # the row of a block job has the columns of its chunk
        block = .texture_block(
            chunk = job, feature = feature, block_file = job[["block_file"]],
            window_size = window_size, angles = angles, expr = expr,
            out_band = out_band, in_bands = in_bands, texture_fn = texture_fn
        ),
        merge = .texture_merge(
            feature = feature, out_band = out_band,
            out_file = job[["out_file"]],
            block_files = job[["block_files"]][[1L]]
        ),
        resume = feature
    )
}
#' @title Log the start of the texture of a feature
#' @name .texture_log_start
#' @keywords internal
#' @noRd
#' @param  job  First job of the feature
#' @return No value, called for side effect
.texture_log_start <- function(job) {
    .debug_log(event = "start_tile", key = "file", value = job[["out_file"]])
    # a resumed feature has no blocks
    if (job[["role"]] != "resume") {
        .debug_log(
            event = "start_tile_blocks", key = "file", value = job[["out_file"]]
        )
    }
}
#' @title Log the end of the blocks of a feature
#' @name .texture_log_last
#' @keywords internal
#' @noRd
#' @param  job  Last job of the feature
#' @return No value, called for side effect
.texture_log_last <- function(job) {
    # a merge is sent once the blocks of its feature are done
    if (job[["role"]] == "merge") {
        .debug_log(
            event = "end_tile_blocks", key = "file", value = job[["out_file"]]
        )
    }
}
#' @title Log the end of the texture of a feature
#' @name .texture_log_end
#' @keywords internal
#' @noRd
#' @param  job    Last job of the feature
#' @param  value  Feature with the texture band
#' @return No value, called for side effect
.texture_log_end <- function(job, value) {
    .debug_log(event = "end_tile", key = "file", value = job[["out_file"]])
}
#' @title Backend of the texture: GLCM functions and sizing
#' @name .texture_backend
#' @keywords internal
#' @noRd
#' @description Canonical contract of a pipeline with backends. Other
#'   pipelines follow it when they migrate.
#'   1. A backend is `list(name, fns, size)`; `size(block, overlap, npaths,
#'   window_size, image_size, memsize, multicores)` returns `list(block,
#'   multicores, limit)`. One backend per device ("cpu": C++, "gpu": torch),
#'   chosen by `.texture_use_gpu()`.
#'   2. Every backend sizes against RAM (`memsize`); a GPU backend also
#'   against GPU memory (`gpu_memsize`). For each budget a worker needs
#'   block memory plus the fixed memory of a worker. Block memory is
#'   `pixels x npaths x nbytes x bloat` (`.jobs_block_memsize()`): the base
#'   is the bands in doubles for RAM, a float64 image (box metrics) or the
#'   long codes of the pairs (ASM, energy) for the GPU; `bloat` counts the
#'   copies of the base, measured with one variable at a time.
#'   3. Order of the criteria. (a) Workers asked: `multicores`, else the
#'   open cluster, else 2 (`.jobs_request_workers()`); a smaller open
#'   cluster gives its size and a warning. (b) Each budget gives
#'   `floor(budget / need of one worker)`; the workers are the smallest of
#'   the request and the budgets, and
#'   `limit` names each budget that cut them. (c) Each budget gives its
#'   optimal block for `(budget - workers x fixed) / workers`; the block
#'   with fewer pixels wins; a `block_size` of the caller replaces it, with
#'   the workers still sized for that block. (d) A block that does not fit
#'   a budget is an error that names that budget. (b) to (d) are
#'   `.jobs_size()`; the warning of (b) is `.jobs_warn_workers()`.
#'   4. `.parallel_start()` opens a cluster of the workers when none is
#'   open; an open cluster is limited with `.parallel_force_multicores()`,
#'   never enlarged, and the limit is reset on exit.
#'   5. Config, per algorithm and per backend, read with `.conf_backend()`;
#'   a missing value falls back to a general key or to 0:
#'   `texture_options -> cpu -> ram_bloat` (`processing_bloat_cpu`),
#'   `ram_worker_memsize` (0); `texture_options -> gpu -> ram_bloat`
#'   (`processing_bloat_gpu`), `ram_worker_memsize` (0),
#'   `gpu_worker_memsize` (0), `gpu_bloat_box` and `gpu_bloat_pairs`
#'   (`processing_bloat_gpu`). Each value names its source in a comment of
#'   `config_internals.yml`.
#' @param  gpu_memsize  Memory of the GPU in GB, used only on the GPU
#' @param  metrics      Names the expression calls, to tell the GLCM
#'                      functions that need the pairs of each window
#' @return List with name ("gpu" or "cpu"), fns (factory of the glcm_*
#'         functions) and size(), which returns list(block, multicores,
#'         limit), limit naming the budgets that cut the workers
#'         ("memsize", "gpu_memsize" or both) or NULL
.texture_backend <- function(gpu_memsize, metrics = character()) {
    if (.texture_use_gpu()) {
        list(
            name = "gpu",
            fns = .texture_torch_functions,
            size = function(block, overlap, npaths, window_size, image_size,
                            memsize, multicores) {
                block_size <- .block_size(block = block, overlap = overlap)
                # RAM of the host: the bands in doubles
                ram <- .jobs_budget(
                    name = "memsize",
                    memsize = memsize,
                    block_memsize = .jobs_block_memsize(
                        block_size = block_size,
                        npaths = npaths,
                        nbytes = 8L,
                        proc_bloat = .conf_backend(
                            "texture_options", "gpu", "ram_bloat",
                            .conf("processing_bloat_gpu")
                        )
                    ),
                    worker_memsize = .conf_backend(
                        "texture_options", "gpu", "ram_worker_memsize", 0
                    )
                )
                # GPU memory: float64 images of the block for the box
                # metrics; long codes of the pairs of each window for the
                # ASM and the energy
                with_pairs <- any(
                    c("glcm_asm", "glcm_energy") %in% metrics
                )
                # each worker also holds a CUDA context besides its block
                gpu <- .jobs_budget(
                    name = "gpu_memsize",
                    memsize = gpu_memsize,
                    block_memsize = .jobs_block_memsize(
                        block_size = block_size,
                        npaths = if (with_pairs) {
                            2L * window_size * (window_size - 1L)
                        } else {
                            1L
                        },
                        nbytes = 8L,
                        proc_bloat = .conf_backend(
                            "texture_options", "gpu",
                            if (with_pairs) {
                                "gpu_bloat_pairs"
                            } else {
                                "gpu_bloat_box"
                            },
                            .conf("processing_bloat_gpu")
                        )
                    ),
                    worker_memsize = .conf_backend(
                        "texture_options", "gpu", "gpu_worker_memsize", 0
                    )
                )
                .jobs_size(
                    budgets = list(ram, gpu), block = block,
                    image_size = image_size, multicores = multicores
                )
            }
        )
    } else {
        list(
            name = "cpu",
            fns = .texture_functions,
            size = function(block, overlap, npaths, window_size, image_size,
                            memsize, multicores) {
                ram <- .jobs_budget(
                    name = "memsize",
                    memsize = memsize,
                    block_memsize = .jobs_block_memsize(
                        block_size = .block_size(
                            block = block, overlap = overlap
                        ),
                        npaths = npaths,
                        nbytes = 8L,
                        proc_bloat = .conf_backend(
                            "texture_options", "cpu", "ram_bloat",
                            .conf("processing_bloat_cpu")
                        )
                    ),
                    worker_memsize = .conf_backend(
                        "texture_options", "cpu", "ram_worker_memsize", 0
                    )
                )
                .jobs_size(
                    budgets = list(ram), block = block,
                    image_size = image_size, multicores = multicores
                )
            }
        )
    }
}

#' @title Normalize values based on a min and max range
#' @name .texture_normalize
#' @description This code is based on scales package.
#' @noRd
#' @param values Numeric matrix or vector
#' @param source A vector with the minimum and maximum values of the
#'  source values.
#' @param dest   A vector with the minimum and maximum of the destination
#'  values.
#' @return a vector with the adjusted block size
.texture_normalize <- function(values, source, dest) {
    values <- (values - source[1L]) / diff(source) * diff(dest) + dest[1L]
    values
}

#' @title Get block size
#' @name .texture_blocksize
#' @noRd
#' @param cube sits cube
#' @return a vector with the adjusted block size
.texture_blocksize <- function(cube) {
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    glcm_block_size <- .conf(c("texture_options", "block_size"))
    block[["nrows"]] <- min(block[["nrows"]], glcm_block_size)
    block[["ncols"]] <- min(block[["ncols"]], glcm_block_size)
    block
}

#' @title Use the GPU for texture?
#' @name .texture_use_gpu
#' @keywords internal
#' @noRd
#' @return TRUE if torch can use a GPU (CUDA or MPS) and
#'         SITS_FORCE_CPU is not TRUE.
.texture_use_gpu <- function() .torch_gpu_available()

#' @title Kernel function for window operations in spatial neighbourhoods
#' @name .texture_functions
#' @noRd
#' @param window_size size of local window
#' @param img_nrow    image size in rows
#' @param img_ncol    image size in cols
#' @return glcm measures
.texture_functions <- function(window_size, angles, img_nrow, img_ncol, n_grey) {
    result_env <- list2env(list(
        glcm_contrast = function(m) {
            C_glcm_contrast(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_dissimilarity = function(m) {
            C_glcm_dissimilarity(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_homogeneity = function(m) {
            C_glcm_homogeneity(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_energy = function(m) {
            C_glcm_energy(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_asm = function(m) {
            C_glcm_asm(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_mean = function(m) {
            C_glcm_mean(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_variance = function(m) {
            C_glcm_variance(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_std = function(m) {
            C_glcm_std(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        },
        glcm_correlation = function(m) {
            C_glcm_correlation(
                x = .as_int(unlist(m)), angles = angles, nrows = img_nrow,
                ncols = img_ncol, window_size = window_size, n_grey = n_grey
            )
        }
    ), parent = parent.env(environment()), hash = TRUE)

    result_env
}
