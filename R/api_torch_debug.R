#' @title Get torch GPU memory statistics
#' @name .torch_memory_stats
#' @keywords internal
#' @noRd
#'
#' @description
#' Reads the memory managed by the torch CUDA caching allocator for the
#' current device. Values are reported in MB:
#' * allocated: memory occupied by live tensors;
#' * reserved: memory held by the allocator (allocated + cached);
#' * peak_allocated: maximum allocated memory since the process started.
#'
#' Only CUDA is supported, since R torch does not expose memory statistics
#' for MPS. In every other case (CPU, MPS, or a failure when reading the
#' statistics) the values are \code{NA}.
#'
#' Note that the statistics cover torch allocations only. They do not include
#' the CUDA context overhead reported by \code{nvidia-smi}.
#'
#' @return Named numeric vector with \code{allocated}, \code{reserved}, and
#'         \code{peak_allocated} in MB.
.torch_memory_stats <- function() {
    stats <- c(
        allocated = NA_real_,
        reserved = NA_real_,
        peak_allocated = NA_real_
    )
    if (!.torch_is_functional() || !torch::cuda_is_available()) {
        return(stats)
    }
    tryCatch({
        mem <- torch::cuda_memory_stats()
        to_mb <- function(x) as.numeric(x) / 1024^2
        c(
            allocated = to_mb(mem[["allocated_bytes"]][["all"]][["current"]]),
            reserved = to_mb(mem[["reserved_bytes"]][["all"]][["current"]]),
            peak_allocated = to_mb(
                mem[["allocated_bytes"]][["all"]][["peak"]]
            )
        )
    }, error = function(e) {
        stats
    })
}

#' @title Log torch GPU memory usage
#' @name .torch_debug_log
#' @keywords internal
#' @noRd
#'
#' @description
#' Writes the current GPU memory statistics to the sits debug log, using
#' \code{.debug_log()}. One row is written for each of the keys
#' \code{gpu_allocated_mb}, \code{gpu_reserved_mb}, and
#' \code{gpu_peak_allocated_mb}. The call has no cost when debug mode is off
#' and does nothing when no CUDA device is in use.
#'
#' @param event  Event name (e.g., \code{"smooth_band_unfold"}).
#'
#' @return Invisibly returns \code{NULL}.
.torch_debug_log <- function(event) {
    # Exit before touching torch if debug is disabled
    if (!.debug()) {
        return(invisible(NULL))
    }
    stats <- .torch_memory_stats()
    # No CUDA memory information available
    if (all(is.na(stats))) {
        return(invisible(NULL))
    }
    keys <- c(
        allocated = "gpu_allocated_mb",
        reserved = "gpu_reserved_mb",
        peak_allocated = "gpu_peak_allocated_mb"
    )
    purrr::iwalk(keys, function(key, name) {
        .debug_log(
            event = event,
            key = key,
            value = round(stats[[name]], digits = 1L)
        )
    })
    invisible(NULL)
}
