#' @title Detection disturbance in combined time series or data cubes
#' @name sits_radd
#' @author Felipe Carvalho, \email{lipecaso@@gmail.com}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' This function implements the algorithm described by Johanes Reiche
#' referenced below.
#'
#' @references Reiche J, De Bruin S, Hoekman D, Verbesselt J, Herold M,
#' "A Bayesian approach to combine Landsat and ALOS PALSAR time
#' series for near real-time deforestation detection.",
#' Remote Sensing, 7, 2015 DOI: 10.3390/rs70504973.
#'
#'
#' @param  data              Data cube (tibble of class "raster_cube")
#' @param  mean_stats        A tibble with mean value of each band.
#' @param sd_stats           A tibble with the standard deviation
#'                           value of each band.
#' @param  ...               Other parameters for specific functions.
#' @param  impute_fn         Imputation function to remove NA.
#' @param  roi               Region of interest (either an sf object, shapefile,
#'                           or a numeric vector with named XY values
#'                           ("xmin", "xmax", "ymin", "ymax") or
#'                           named lat/long values
#'                           ("lon_min", "lat_min", "lon_max", "lat_max").
#' @param  start_date        Start date for the detection
#'                           (Date in YYYY-MM-DD format).
#' @param  end_date          End date for the dectection
#'                           (Date im YYYY-MM-DD format).
#' @param  memsize           Memory available for classification in GB
#'                           (integer, min = 1, max = 16384).
#' @param  multicores        Number of cores to be used for classification
#'                           (integer, min = 1, max = 2048).
#' @param  deseasonlize      A numeric value with the quantile percentage to
#'                           deseasonlize time series using spatial
#'                           normalization.
#' @param threshold          A numeric value with threshold of the probability
#'                           of Non-Forest above which the first observation
#'                           is flagged. Default = 0.5.
#' @param bwf                A numeric vector with the block weighting function
#'                           to truncate the Non-Forest probability.
#'                           Default = (0.1, 0.9).
#' @param  chi               A numeric with threshold of the probability
#'                           change at which the change is confirmed.
#'                           Default = 0.5.
#' @param  output_dir        Valid directory for output file.
#'                           (character vector of length 1).
#' @param  version           Version of the output
#'                           (character vector of length 1).
#' @param  verbose           Logical: print information about processing time?
#' @param  progress          Logical: Show progress bar?
#'
#' @return                   Time series with detection dates for
#'                           each point (tibble of class "sits")
#'                           or a data cube with the detection day of the year
#'                           for each pixel
#'                           (tibble of class "radd_cube").
#'
#' @note
#'    The \code{roi} parameter defines a region of interest. It can be
#'    an sf_object, a shapefile, or a bounding box vector with
#'    named XY values (\code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}) or
#'    named lat/long values (\code{lon_min}, \code{lon_max},
#'    \code{lat_min}, \code{lat_max})
#'
#'    Parameter \code{memsize} controls the amount of memory available
#'    for classification, while \code{multicores}  defines the number of cores
#'    used for processing. We recommend using as much memory as possible.
#'    Please refer to the sits documentation available in
#'    <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @export
sits_radd <- function(data,
                      mean_stats,
                      sd_stats, ...,
                      chi = 0.9,
                      start_date = NULL,
                      end_date = NULL) {
    UseMethod("sits_radd", data)
}


# sits_radd.sits <- function(data,
#                            mean_stats,
#                            sd_stats, ...,
#                            chi = 0.9,
#                            start_date = NULL,
#                            end_date = NULL) {
#     # Training function
#     train_fun <- function(data) {
#         # Check 'pdf' parameter
#         .check_chr_parameter(pdf)
#         # Check 'chi' parameter
#         .check_num_min_max(chi, min = 0.1, max = 1)
#         # Check 'start_date' parameter
#         .check_date_parameter(start_date)
#         # Check 'end_date' parameter
#         .check_date_parameter(end_date)
#
#         # Get pdf function
#         pdf_fn <- .pdf_fun(pdf)
#         # Create stats layer
#         if (!.has(stats_layer)) {
#             stats_layer <- .radd_create_stats(data)
#         }
#         # Calculate probability for NF
#         data <- .radd_calc_pnf(
#             data = data,
#             pdf_fn = pdf_fn,
#             stats_layer = stats_layer
#         )
#         predict_fun <- function() {
#             # Now we need to detected the changes
#             data <- .radd_detect_events(
#                 data = data,
#                 threshold = 0.5,
#                 start_date = start_date,
#                 end_date = end_date
#             )
#         }
#         # Set model class
#         predict_fun <- .set_class(
#             predict_fun, "radd_model", "sits_model", class(predict_fun)
#         )
#         return(predict_fun)
#     }
#     # If samples is informed, train a model and return a predict function
#     # Otherwise give back a train function to train model further
#     result <- .factory_function(data, train_fun)
#     return(result)
# }

#' @rdname sits_radd
#' @export
sits_radd.raster_cube <- function(data,
                                  mean_stats,
                                  sd_stats, ...,
                                  impute_fn = identity,
                                  roi = NULL,
                                  start_date = NULL,
                                  end_date = NULL,
                                  memsize = 8L,
                                  multicores = 2L,
                                  deseasonlize = 0.95,
                                  threshold = 0.5,
                                  bwf = c(0.1, 0.9),
                                  chi = 0.9,
                                  output_dir,
                                  version = "v1",
                                  progress = TRUE) {
    # Training function
    train_fun <- function(data) {
        # Preconditions
        .check_num_min_max(chi, min = 0.1, max = 1)
        .check_output_dir(output_dir)
        version <- .check_version(version)
        .check_progress(progress)
        # TODO: check mean and sd stats
        mean_stats <- unname(as.matrix(mean_stats[, -1]))
        sd_stats <- unname(as.matrix(sd_stats[, -1]))

        # version is case-insensitive in sits
        version <- tolower(version)

        # Get default proc bloat
        proc_bloat <- .conf("processing_bloat_cpu")

        # Get pdf function
        pdf_fn <- .pdf_fun("gaussian")

        # Spatial filter
        if (.has(roi)) {
            roi <- .roi_as_sf(roi)
            data <- .cube_filter_spatial(cube = data, roi = roi)
        }

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

        predict_fun <- function() {

            # Prepare parallel processing
            .parallel_start(workers = multicores)
            on.exit(.parallel_stop(), add = TRUE)

            # Calculate the probability of Non-Forest
            # Process each tile sequentially
            probs_cube <- .cube_foreach_tile(data, function(tile) {
                # Classify the data
                probs_tile <- .radd_calc_tile(
                    tile = tile,
                    band = "radd",
                    roi = roi,
                    pdf_fn = pdf_fn,
                    mean_stats = mean_stats,
                    sd_stats = sd_stats,
                    deseasonlize = deseasonlize,
                    threshold = threshold,
                    chi = chi,
                    bwf = bwf,
                    block = block,
                    impute_fn = impute_fn,
                    start_date = start_date,
                    end_date = end_date,
                    output_dir = output_dir,
                    version = version,
                    progress = progress
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
