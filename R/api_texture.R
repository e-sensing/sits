#' @title Texture of one block of a feature
#' @name .texture_block
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @keywords internal
#' @noRd
#' @description Reads one block of a feature, scales it to the grey levels of
#'   the texture and evaluates the expression with the GLCM functions of the
#'   backend. The result is written to the block file; a block with a valid file
#'   is skipped, so a failed run can resume.
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
#'                         functions (\code{.texture_cpp_functions} for C++,
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
    # the metrics are written as they come out of the kernel, and the file
    # type tells the reader to read them with scale 1
    band_conf <- .conf("default_values", "FLT4S")
    # Get texture options
    texture_conf <- .conf("texture_options")
    # Read bands data: physical values, with the scale of each band applied;
    # the GLCM functions bin the value of their argument to grey levels
    values <- .apply_data_read(
        tile = feature, block = block, in_bands = in_bands
    )
    # Fill with zeros remaining NA pixels
    values[[1L]] <- C_fill_na(as.matrix(values[[1L]]), 0.0)
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Joins the block files of a feature into the file of its texture
#'   band and returns the feature with that band. Leftovers of an earlier merge
#'   are removed first, so a merge sent again does not read partial files.
#' @param  feature      Subset of a data cube containing the input bands
#' @param  out_band     Output band
#' @param  out_file     File of the texture feature
#' @param  block_files  Files of the blocks
#' @return              Feature with the texture band
.texture_merge <- function(feature, out_band, out_file, block_files) {
    # a merge sent again after a dead worker finds its leftovers
    .file_merge_clean(out_file)
    # the blocks were written as FLT4S
    band_conf <- .conf("default_values", "FLT4S")
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Lists what one feature needs: one job per block and one merge. A
#'   feature whose file is valid gets a single resume job instead, so a
#'   finished feature is not computed again.
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Runs one job of the texture. The role of the job tells what to
#'   do: compute a block, merge the blocks of a feature, or return a feature
#'   that was already done.
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Writes the debug log events that open the texture of a feature.
#'   A resumed feature has no blocks, so it gets no block event.
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Writes the debug log event that closes the blocks of a feature.
#'   It does so when the job is the merge, which is sent only after the blocks
#'   are done.
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Writes the debug log event that closes the texture of a feature.
#' @param  job    Last job of the feature
#' @param  value  Feature with the texture band
#' @return No value, called for side effect
.texture_log_end <- function(job, value) {
    .debug_log(event = "end_tile", key = "file", value = job[["out_file"]])
}
#' @title RAM budget of a texture backend
#' @name .texture_ram_budget
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Computes how much RAM the texture needs. The memory of one
#'   block is its pixels times its bands times 8 bytes, multiplied by
#'   `ram_bloat`, the number of copies the backend makes. Each worker also
#'   holds `ram_worker_memsize`. The call holds, once, the memory of the
#'   main process and of the merges.
#' @param  backend     Name of the backend block: "cpu" or "gpu"
#' @param  block_size  Size of the base block with its overlap
#' @param  memsize     Memory of the budget in GB
#' @param  npaths      Number of paths (bands) of a pixel
#' @param  multicores  Workers asked
#' @return A `.jobs_budget()`
.texture_ram_budget <- function(backend, block_size, memsize, npaths,
                                multicores) {
    .jobs_budget(
        name = "memsize",
        memsize = memsize,
        block_memsize = .jobs_block_memsize(
            block_size = block_size,
            npaths = npaths,
            nbytes = 8L,
            proc_bloat = .conf_backend(
                c("texture_options", "memory"), backend, "ram_bloat",
                .conf(paste0("processing_bloat_", backend))
            )
        ),
        worker_memsize = .conf_backend(
            c("texture_options", "memory"), backend, "ram_worker_memsize", 0
        ),
        call_memsize = .jobs_call_memsize(
            c("texture_options", "memory"), backend, multicores
        )
    )
}
#' @title Backend of the texture: GLCM functions and sizing
#' @name .texture_backend
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Canonical contract of a pipeline with backends. Other
#'   pipelines follow it when they migrate.
#'   1. A backend is `list(name, fns, size)`; `size(block, overlap, npaths,
#'   window_size, image_size, memsize, multicores)` returns `list(block,
#'   multicores, limit)`. One backend per device ("cpu": C++, "gpu": torch),
#'   chosen by `.texture_use_gpu()`.
#'   2. Every backend sizes against RAM (`memsize`); a GPU backend also
#'   against GPU memory (`gpu_memsize`). The base block is the one of the
#'   file (`.texture_blocksize()`, limited by the `block_size` key). Any
#'   budget may take a call term, the memory the call holds once; the
#'   texture gives one to RAM only: what the call adds to the main process
#'   and to its merges at once (`.jobs_call_memsize()`), taken before the
#'   workers are counted. For each budget a worker needs block memory plus
#'   the fixed memory of a worker. Block memory is
#'   `pixels x npaths x nbytes x bloat` (`.jobs_block_memsize()`). The base
#'   is the bands in doubles for RAM, an image at 8 bytes a pixel (box
#'   metrics) or the long codes of the pairs (ASM, energy) for the GPU.
#'   `bloat` counts the copies of the base, measured with one variable at
#'   a time.
#'   3. Order of the criteria. (a) Workers asked: `multicores`, else the
#'   open cluster, else 2 (`.jobs_request_workers()`); a smaller open
#'   cluster gives its size and a warning. (b) Each budget gives
#'   `floor((budget - call) / need of one worker)`. The workers are the
#'   smallest of the request and the budgets, and `limit` names each budget
#'   that cut them. (c) Each budget gives its optimal block for
#'   `(budget - call - workers x fixed) / workers`. The block with fewer
#'   pixels wins; a `block_size` of the caller replaces it, with the
#'   workers still sized for that block. (d) A block, a worker and the call
#'   that do not fit a budget are an error that names it. (b) to (d) are
#'   `.jobs_size()`; the warning of (b) is `.jobs_warn_workers()`.
#'   4. `.parallel_start()` opens a cluster of the workers when none is
#'   open. With one worker and no cluster, the worker is the main process;
#'   with `SITS_DEBUG=TRUE`, no cluster is opened either. An open cluster
#'   is limited with `.parallel_force_multicores()`, never enlarged, and
#'   the limit is reset to NULL on exit.
#'   5. Config, per algorithm and per backend, read with `.conf_backend()`;
#'   a missing value falls back to a general key or to 0:
#'   `texture_options -> memory -> cpu -> ram_bloat` (`processing_bloat_cpu`),
#'   `ram_worker_memsize` (0), `ram_main_memsize` (0) and
#'   `ram_merge_memsize` (0); `texture_options -> memory -> merges` (0), the
#'   largest count of merges measured at once, for both backends, not a
#'   bound the queue enforces; `texture_options -> memory -> gpu -> ram_bloat`
#'   (`processing_bloat_gpu`), `ram_worker_memsize` (0),
#'   `ram_main_memsize` (0), `ram_merge_memsize` (0),
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
                ram <- .texture_ram_budget(
                    "gpu", block_size, memsize, npaths, multicores
                )
                # GPU memory: float32 images of the block for the box metrics,
                # counted at 8 bytes, as measured in float64, until measured
                # again; long codes of the pairs for the ASM and the energy
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
                            c("texture_options", "memory"), "gpu",
                            if (with_pairs) {
                                "gpu_bloat_pairs"
                            } else {
                                "gpu_bloat_box"
                            },
                            .conf("processing_bloat_gpu")
                        )
                    ),
                    worker_memsize = .conf_backend(
                        c("texture_options", "memory"), "gpu",
                        "gpu_worker_memsize", 0
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
            fns = .texture_cpp_functions,
            size = function(block, overlap, npaths, window_size, image_size,
                            memsize, multicores) {
                ram <- .texture_ram_budget(
                    "cpu", .block_size(block = block, overlap = overlap),
                    memsize, npaths, multicores
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
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description This code is based on scales package.
#' @param values Numeric matrix or vector
#' @param source A vector with the minimum and maximum values of the
#'  source values.
#' @param dest   A vector with the minimum and maximum of the destination
#'  values.
#' @return Values rescaled to the destination range
.texture_normalize <- function(values, source, dest) {
    values <- (values - source[1L]) / diff(source) * diff(dest) + dest[1L]
    values
}
#' @title Bin values to the grey levels of the texture
#' @name .texture_grey_levels
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Maps values to grey levels from `grey_min` to `n_grey`,
#'   between `min_value` and `max_value`; values outside are cut to them. The
#'   GLCM counts pairs of whole grey levels, and the C++ kernel has no level
#'   `n_grey`, so the maximum falls in the level below, as it did before.
#' @param values    Values of the argument of a GLCM function
#' @param min_value Value of the lowest grey level
#' @param max_value Value of the highest grey level
#' @param n_grey    Number of grey levels
#' @param grey_min  Lowest grey level
#' @return Values from `grey_min` to `n_grey - 1`
.texture_grey_levels <- function(values, min_value, max_value, n_grey,
                                 grey_min = 0.0) {
    values <- pmin(pmax(values, min_value), max_value)
    values <- .texture_normalize(
        values = values, source = c(min_value, max_value),
        dest = c(grey_min, n_grey)
    )
    pmin(values, n_grey - 1.0)
}
#' @title GLCM functions that bin their argument
#' @name .texture_binned_functions
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Wraps the factory of the GLCM functions of a backend, so each
#'   function bins the value of its argument, an expression of physical band
#'   values, before the kernel. One range for every block, tile and date
#'   keeps the textures of a mosaic comparable.
#' @param texture_fn Factory of the GLCM functions of the backend
#' @param min_value  Value of the lowest grey level
#' @param max_value  Value of the highest grey level
#' @return A factory with the arguments of `texture_fn`
.texture_binned_functions <- function(texture_fn, min_value, max_value) {
    force(texture_fn)
    force(min_value)
    force(max_value)
    function(window_size, angles, img_nrow, img_ncol, n_grey) {
        fns <- texture_fn(
            window_size = window_size, angles = angles, img_nrow = img_nrow,
            img_ncol = img_ncol, n_grey = n_grey
        )
        grey_min <- .min_value(.conf("texture_options"))
        binned <- lapply(as.list(fns), function(fn) {
            force(fn)
            function(m) {
                fn(.texture_grey_levels(
                    values = unlist(m), min_value = min_value,
                    max_value = max_value, n_grey = n_grey, grey_min = grey_min
                ))
            }
        })
        list2env(binned, parent = fns)
    }
}

#' @title Get block size
#' @name .texture_blocksize
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Gives the block size of the file of the cube, limited by the
#'   `block_size` key of `texture_options`. Reading the texture in blocks of
#'   the file size avoids reading the same file region twice.
#' @param cube sits cube
#' @return Block with nrows and ncols, at most the `block_size` key
.texture_blocksize <- function(cube) {
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(cube)))
    glcm_block_size <- .conf(c("texture_options", "block_size"))
    block[["nrows"]] <- min(block[["nrows"]], glcm_block_size)
    block[["ncols"]] <- min(block[["ncols"]], glcm_block_size)
    block
}

#' @title Use the GPU for texture?
#' @name .texture_use_gpu
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Tells if the texture runs on the GPU. It does when torch can use
#'   a CUDA GPU and the user did not force the CPU.
#' @return TRUE if torch can use a CUDA GPU and SITS_FORCE_CPU is not
#'         TRUE; MPS is not used, since it was never measured.
.texture_use_gpu <- function() {
    .torch_gpu_available() && .torch_cuda_enabled()
}
