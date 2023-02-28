#' @title Estimate ensemble prediction based on list of probs cubes
#'
#' @name  sits_combine_predictions
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param  cubes             List of probability data cubes.
#' @param  type              Method to measure uncertainty. See details.
#' @param  ...               Parameters for specific functions.
#' @param  weights           Weights for averaging
#' @param  uncert_cubes      Uncertainty cubes to be used as local weights.
#' @param  multicores        Number of cores to run the function.
#' @param  memsize           Maximum overall memory (in GB) to run the
#'                           function.
#' @param  output_dir        Output directory for image files.
#' @param  version           Version of resulting image.
#'                           (in the case of multiple tests)
#' @return A combined probability cube
#'
#' @description Calculate an ensemble predictor based a list of probability
#' cubes. The function combines the output of two or more classifier
#' to derive a value which is based on weights assigned to each model.
#' The supported types of ensemble predictors are 'average' and
#' 'uncertainty'.
#'
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "tile", "band", "date")
#'     )
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # classify a data cube using rfor model
#'     probs_rfor_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # create an XGBoost model
#'     xgb_model <- sits_train(samples_modis_ndvi, sits_xgboost())
#'     # classify a data cube using xgboost model
#'     probs_xgb_cube <- sits_classify(data = cube, ml_model = xgb_model)
#'     # create a list of predictions to be combined
#'     pred_cubes <- list(probs_rfor_cube, probs_xgb_cube)
#'     # combine predictions
#'     comb_probs_cube <- sits_combine_predictions(cubes = pred_cubes)
#'     # plot the resulting combined prediction cube
#'     plot(comb_probs_cube)
#' }
#' @export
sits_combine_predictions <- function(cubes, type = "average", ...,
                                     memsize = 8,
                                     multicores = 2,
                                     output_dir = getwd(),
                                     version = "v1") {
    # check if list of probs cubes have the same organization
    .check_probs_cube_lst(cubes)
    # Check memsize
    .check_memsize(memsize)
    # Check multicores
    .check_multicores(multicores)
    # Check output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # Check version
    .check_version(version)

    # Check memory and multicores
    # Get block size
    base_cube <- cubes[[1]]
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(base_cube)))
    # Check minimum memory needed to process one block
    job_memsize <- .jobs_memsize(
        job_size = .block_size(block = block),
        npaths = length(cubes) * nrow(base_cube) * length(sits_labels(base_cube)),
        nbytes = 8,
        proc_bloat = .conf("processing_bloat")
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
        image_size = .tile_size(.tile(base_cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    .comb_use_method(cubes = cubes,
                     block = block,
                     memsize = memsize,
                     multicores = multicores,
                     output_dir = output_dir,
                     version = version, ...)
}

.comb_uncert <- function(cubes, ...,
                         uncert_cubes,
                         multicores = 2,
                         memsize = 4,
                         output_dir = getwd(),
                         version = "v1") {
    # check if probs cubes and uncert cubes are valid and match
    # is every cube a probs cube
    purrr::map(cubes, .check_is_probs_cube)
    # check if cubes match
    .check_cube_list_match(cubes)
    # all uncert cubes are valid
    purrr::map(uncert_cubes, .check_cube_is_uncert_cube)
    .check_cube_list_match(uncert_cubes)
    # check if probs and uncert cubes match
    .check_cubes_match(cubes[[1]], uncert_cubes[[1]])

    # get number of labels
    n_labels <- length(sits_labels(cubes[[1]]))
    # average probability calculation
    .do_uncert <- function(prob_values_lst, unc_values_lst) {
        values_out <- weighted_uncert_probs(prob_values_lst,
                                           unc_values_lst)
        return(values_out)
    }

    # compute which block size is many tiles to be computed
    block_size <- .comb_estimate_block_size(
        cubes  = cubes,
        multicores = multicores,
        memsize = memsize,
        uncert = TRUE
    )

    # start parallel processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # process each brick layer (each time step) individually

    blocks_tile_lst <- purrr::map(seq_len(nrow(cubes[[1]])), function(i) {
        tile <- cubes[[1]][i,]
        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            tile       = tile,
            cube_class = "probs_cube",
            band_name  = "probs",
            labels     = .tile_labels(tile),
            start_date = .tile_start_date(tile),
            end_date   = .tile_end_date(tile),
            bbox       = .bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        # prepare output filename
        out_file <- .tile_path(tile_new)

        # if file exists skip it (resume feature)
        if (file.exists(out_file)) {
            if (all(.raster_bbox(.raster_open_rast(out_file))
                    == sits_bbox(tile_new))) {
                message(paste0(
                    "Recovery mode: probability image file found in '",
                    dirname(out_file), "' directory. ",
                    "(If you want a new probability image, please ",
                    "change the directory in the 'output_dir' or the ",
                    "value of 'version' parameter)"
                ))
                return(NULL)
            }
        }

        # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .comb_compute_blocks(
            xsize = .tile_ncols(tile),
            ysize = .tile_nrows(tile),
            block_y_size = block_size[["block_y_size"]],
            overlapping_y_size = 0
        )
        tile_lst <- purrr::map(cubes, function(cube){
            return(cube[i,])
        } )
        tile_unc_lst <- purrr::map(uncert_cubes, function(cube_unc){
            return(cube_unc[i,])
        })
        # open probability files
        in_file_lst <- purrr::map(tile_lst, .tile_path)
        # open uncert files
        in_file_unc_lst <- purrr::map(tile_unc_lst, .tile_path)

        # create an r object that contains the entire input file
        r_obj <- .raster_open_rast(in_file_lst[[1]])

        # process blocks in parallel
        block_files_lst <- .sits_parallel_map(blocks, function(block) {

            # open brick
            prob_values_lst <- purrr::map(in_file_lst, function(in_file) {
                prob_values <- .raster_read_rast(files = in_file,
                                                 block = block)
                return(prob_values)
            })
            unc_values_lst <- purrr::map(in_file_unc_lst,
                                         function(in_unc_file){
                unc_values <- .raster_read_rast(files = in_unc_file,
                                                block = block)
                return(unc_values)
            })
            # process it
            values_out <- .do_uncert(prob_values_lst = prob_values_lst,
                                     unc_values_lst = unc_values_lst)

            # create an empty raster with the coordinates that fit the block
            crop_obj <- .raster_crop_metadata(r_obj, block = block)
            # create a raster where the values will be saved
            raster_out <- .raster_rast(
                r_obj = crop_obj,
                nlayers = n_labels
            )
            # copy values to raster
            raster_out <- .raster_set_values(
                r_obj = raster_out,
                values = values_out
            )
            # set output block file name
            block_file <- .file_block_name(
                pattern = "chunk_combine_unc_out_",
                block = block,
                output_dir = output_dir
            )
            # write data to file
            .raster_write_rast(
                r_obj = raster_out,
                file = block_file,
                data_type = .conf("probs_cube_data_type"),
                overwrite    = TRUE
            )
            return(block_file)
        })

        block_files <- unlist(block_files_lst)
        return(invisible(block_files))
    })

    # process each brick layer (each time step) individually
    result_cube <- .sits_parallel_map(seq_along(blocks_tile_lst), function(i) {

        # get tile from cube
        tile <- cubes[[1]][i, ]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            tile       = tile,
            cube_class = "probs_cube",
            band_name  = "probs",
            labels     = .tile_labels(tile),
            start_date = .tile_start_date(tile),
            end_date   = .tile_end_date(tile),
            bbox       = .bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        # prepare output filename
        out_file <- .tile_path(tile_new)

        # if file exists skip it (resume feature)
        if (file.exists(out_file)) {
            return(tile_new)
        }

        block_files <- blocks_tile_lst[[i]]

        # Merge final result
        .raster_merge_blocks(
            out_files = out_file,
            base_file = .tile_path(tile),
            block_files = block_files,
            data_type = .conf("probs_cube_data_type"),
            missing_value = .conf("probs_cube_missing_value"),
            multicores = 1
        )

        # Remove blocks
        on.exit(unlink(block_files), add = TRUE)

        return(tile_new)
    })

    # bind rows
    result_cube <- dplyr::bind_rows(result_cube)

    class(result_cube) <- unique(c("probs_cube", class(cubes[[1]])))

    return(result_cube)
}
