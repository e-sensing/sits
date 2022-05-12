#' @title Build a labelled image from a probability cube
#'
#' @name  sits_label_classification
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers with probabilities,
#'              and label them based on the maximum probability for each pixel.
#'
#' @param  cube              Classified image data cube.
#' @param  multicores        Number of workers to label the classification in
#'                           parallel.
#' @param  memsize           maximum overall memory (in GB) to label the
#'                           classification.
#' @param  output_dir        Output directory for classified files.
#' @param  version           Version of resulting image
#'                           (in the case of multiple runs).
#' @return A data cube
#' @note
#' Please refer to the sits documentation available in
#' <https://e-sensing.github.io/sitsbook/> for detailed examples.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # select a set of samples
#'     samples_ndvi <- sits_select(samples_modis_4bands, bands = c("NDVI"))
#'     # create a random forest model
#'     rfor_model <- sits_train(samples_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6",
#'         data_dir = data_dir,
#'         delim = "_",
#'         parse_info = c("X1", "X2", "tile", "band", "date")
#'     )
#'     # classify a data cube
#'     probs_cube <- sits_classify(data = cube, ml_model = rfor_model)
#'     # plot the probability cube
#'     plot(probs_cube)
#'     # smooth the probability cube using Bayesian statistics
#'     bayes_cube <- sits_smooth(probs_cube)
#'     # plot the smoothed cube
#'     plot(bayes_cube)
#'     # label the probability cube
#'     label_cube <- sits_label_classification(bayes_cube)
#'     # plot the labelled cube
#'     plot(label_cube)
#' }
#' @export
sits_label_classification <- function(cube,
                                      multicores = 2,
                                      memsize = 4,
                                      output_dir = ".",
                                      version = "v1") {

    # set caller to show in errors
    .check_set_caller("sits_label_classification")

    # precondition - check if cube has probability data
    .check_that(
        x = inherits(cube, "probs_cube"),
        msg = "input is not probability cube"
    )
    # precondition 2 - multicores
    .check_num(
        x = multicores,
        min = 1,
        len_min = 1,
        len_max = 1,
        is_integer = TRUE,
        msg = "invalid 'multicores' parameter"
    )

    # precondition - memory
    .check_num(
        x = memsize,
        exclusive_min = 0,
        len_min = 1,
        len_max = 1,
        msg = "invalid 'memsize' parameter"
    )

    # precondition 4 - output dir
    .check_file(
        x = output_dir,
        msg = "invalid output dir"
    )

    # precondition 5 - version
    .check_chr(
        x = version,
        len_min = 1,
        msg = "invalid version"
    )

    # mapping function to be executed by workers cluster
    .do_map <- function(chunk) {

        # read raster
        data <- .raster_get_values(r_obj = chunk)
        # get layer of max probability
        data <- apply(data, 1, which.max)
        # create cube labels
        res <- .raster_rast(r_obj = chunk, nlayers = 1)
        # copy values
        res <- .raster_set_values(r_obj = res, values = data)
        return(res)
    }

    # compute which block size is many tiles to be computed
    block_size <- .smth_estimate_block_size(
        cube = cube,
        multicores = multicores,
        memsize = memsize
    )

    # start parallel processes
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop())

    # process each brick layer (each time step) individually
    blocks_tile_lst <- slider::slide(cube, function(tile) {

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            cube       = tile,
            cube_class = "classified_image",
            band_name  = "class",
            labels     = .cube_labels(tile),
            start_date = .file_info_start_date(tile),
            end_date   = .file_info_end_date(tile),
            bbox       = .cube_tile_bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        # prepare output filename
        out_file <- .file_info_path(tile_new)

        # if file exists skip it (resume feature)
        if (file.exists(out_file)) {
            return(NULL)
        }

        # get cube size
        size <- .cube_size(tile)

        # for now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .smth_compute_blocks(
            xsize = size[["ncols"]],
            ysize = size[["nrows"]],
            block_y_size = block_size[["block_y_size"]],
            overlapping_y_size = 0
        )

        # open probability file
        in_file <- .file_info_path(tile)

        # process blocks in parallel
        block_files_lst <- .sits_parallel_map(blocks, function(block) {

            # open brick
            b <- .raster_open_rast(in_file)

            # crop adding overlaps
            chunk <- .raster_crop(r_obj = b, block = block)

            # process it
            raster_out <- .do_map(chunk = chunk)
            block_file <- .smth_filename(
                tile = tile_new,
                output_dir = output_dir,
                block = block
            )

            # save chunk
            .raster_write_rast(
                r_obj = raster_out,
                file = block_file,
                format = "GTiff",
                data_type = .raster_data_type(
                    .config_get("class_cube_data_type")
                ),
                gdal_options = .config_gtiff_default_options(),
                overwrite = TRUE
            )

            return(block_file)
        })

        block_files <- unlist(block_files_lst)

        return(invisible(block_files))
    })

    # process each brick layer (each time step) individually
    res_cube_lst <- .sits_parallel_map(seq_along(blocks_tile_lst), function(i) {

        # get tile from cube
        tile <- cube[i, ]

        # create metadata for raster cube
        tile_new <- .cube_derived_create(
            cube       = tile,
            cube_class = "classified_image",
            band_name  = "class",
            labels     = .cube_labels(tile),
            start_date = .file_info_start_date(tile),
            end_date   = .file_info_end_date(tile),
            bbox       = .cube_tile_bbox(tile),
            output_dir = output_dir,
            version    = version
        )

        # prepare output filename
        out_file <- .file_info_path(tile_new)

        # if file exists skip it (resume feature)
        if (file.exists(out_file)) {
            return(tile_new)
        }

        tmp_blocks <- blocks_tile_lst[[i]]

        # apply function to blocks
        on.exit(unlink(tmp_blocks))

        # merge to save final result
        suppressWarnings(
            .raster_merge(
                in_files = tmp_blocks,
                out_file = out_file,
                format = "GTiff",
                gdal_datatype =
                    .raster_gdal_datatype(.config_get("class_cube_data_type")),
                gdal_options =
                    .config_gtiff_default_options(),
                overwrite = TRUE
            )
        )

        return(tile_new)
    })

    # bind rows
    result_cube <- dplyr::bind_rows(res_cube_lst)

    class(result_cube) <- unique(c("classified_image", class(result_cube)))

    return(result_cube)
}
