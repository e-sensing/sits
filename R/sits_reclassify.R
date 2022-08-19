#' @title Reclassify a classified cube
#'
#' @name sits_reclassify
#'
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Apply a set of named expressions to reclassify a classified image.
#' The expressions should use character values to refer to labels in
#' logical expressions.
#'
#' @param cube       Classified image cube to be reclassified.
#' @param mask       Classified image cube with additional information
#'                   to be used in expressions.
#' @param mask_na_values Set cube pixels to NA where corresponding pixels in
#'                   mask are NA.
#' @param memsize    Memory available for classification (in GB).
#' @param multicores Number of cores to be used for classification.
#' @param output_dir Directory where files will be saved.
#' @param progress   Show progress bar?
#' @param ...        Named expressions to be evaluated (see details).
#'
#' @details
#' \code{sits_reclassify()} allow any valid R expression to compute
#' reclassification. User should refer to \code{cube} and \code{mask}
#' to construct logical expressions.
#' Users can use can use any R expression that evaluates to logical.
#' \code{TRUE} values will be relabeled to expression name.
#' Updates are done in asynchronous manner, that is, all expressions
#' are evaluated using original classified values. Expressions are
#' evaluated sequentially and resulting values are assigned to
#' output cube. Last expressions has precedence over first ones.
#'
#' @return A classified image cube.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # Open mask map
#'     data_dir <- system.file("extdata/raster/prodes", package = "sits")
#'     prodes2021 <- sits_cube(
#'         source = "USGS",
#'         collection = "LANDSAT-C2L2-SR",
#'         data_dir = data_dir,
#'         parse_info = c("x1", "tile", "start_date", "end_date",
#'                        "band", "version"),
#'         bands = "class",
#'         labels = c("Forest", "Water", "NonForest",
#'                    "NonForest2", "NoClass", "d2007", "d2008",
#'                    "d2009", "d2010", "d2011", "d2012",
#'                    "d2013", "d2014", "d2015", "d2016",
#'                    "d2017", "d2018", "r2010", "r2011",
#'                    "r2012", "r2013", "r2014", "r2015",
#'                    "r2016", "r2017", "r2018", "d2019",
#'                    "r2019", "d2020", "NoClass", "r2020",
#'                    "Clouds2021", "d2021", "r2021")
#'     )
#'
#'     # Open classification map
#'     data_dir <- system.file("extdata/raster/classif", package = "sits")
#'     ro_class <- sits_cube(
#'         source = "MPC",
#'         collection = "SENTINEL-2-L2A",
#'         data_dir = data_dir,
#'         parse_info = c("x1", "tile", "start_date", "end_date",
#'                        "band", "version"),
#'         bands = "class",
#'         labels = c("ClearCut_Fire", "ClearCut_BareSoil",
#'                    "ClearCut_Veg", "Forest")
#'     )
#'
#'     # Reclassify cube
#'     ro_mask <- sits_reclassify(
#'         cube = ro_class,
#'         mask = prodes2021,
#'         "Old_Deforestation" =
#'             mask %in% c("d2007", "d2008", "d2009",
#'                         "d2010", "d2011", "d2012",
#'                         "d2013", "d2014", "d2015",
#'                         "d2016", "d2017", "d2018",
#'                         "r2010", "r2011", "r2012",
#'                         "r2013", "r2014", "r2015",
#'                         "r2016", "r2017", "r2018",
#'                         "d2019", "r2019", "d2020",
#'                         "r2020", "r2021"),
#'         "Water_Mask" = mask == "Water",
#'         "NonForest_Mask" = mask %in% c("NonForest", "NonForest2"),
#'         mask_na_values = TRUE,
#'         memsize = 1,
#'         multicores = 1,
#'         output_dir = getwd(),
#'         progress = TRUE
#'     )
#'
#'     plot(ro_mask, palette = "Geyser")
#' }
#' @rdname sits_reclassify
#' @export
sits_reclassify <- function(cube, mask, ...,
                            mask_na_values = TRUE,
                            memsize = 1,
                            multicores = 2,
                            output_dir = getwd(),
                            progress = TRUE) {

    # set caller
    .check_set_caller("sits_reclassify")

    # pre-conditions
    .check_that(
        x = inherits(cube, "classified_image"),
        local_msg = "value should be a classified_image",
        msg = "invalid 'cube' parameter"
    )

    # pre-conditions
    .check_that(
        x = inherits(mask, "classified_image"),
        local_msg = "value should be a classified_image",
        msg = "invalid 'mask' parameter"
    )

    # capture expressions
    exprs <- as.list(substitute(list(...), environment()))[-1]
    .check_lst(
        x = exprs,
        min_len = 1,
        msg = "invalid expression parameter"
    )

    # Check output_dir
    output_dir <- path.expand(output_dir)
    .check_file(output_dir,
                msg = "invalid output directory"
    )

    # get input labels
    labels_cube <- sits_labels(cube)
    labels_mask <- sits_labels(mask)

    # get output labels
    labels_out <- unique(c(labels_cube, names(exprs)))

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    # get all jobs
    jobs <- .rclasf_get_jobs(cube = cube)

    # start parallel process
    tiles_fid <- .sits_parallel_map(jobs, function(job) {
        # Get parameters from each job
        tile_name <- job[[1]]
        fi_idx <- job[[2]]
        # Get tile to be processed
        tile <- .cube_filter(cube = cube, tile = tile_name)
        # Get file_info
        fi <- .file_info(tile)
        fi_row <- fi[fi_idx,]
        # Output file name
        out_file <- path.expand(file.path(
            output_dir,
            basename(fi_row[["path"]])
        ))
        # Compute the size of blocks to be computed
        # (it should be the whole cube)
        block_size <- .rclasf_estimate_block_size(
            cube = tile,
            multicores = multicores,
            memsize = memsize
        )
        # For now, only vertical blocks are allowed, i.e. 'x_blocks' is 1
        blocks <- .rclasf_compute_blocks(
            xsize = .file_info_ncols(tile),
            ysize = .file_info_nrows(tile),
            block_y_size = block_size[["block_y_size"]]
        )
        # Save each output block and return paths
        blocks_path <- purrr::map(blocks, function(block) {
            # Define the file name of the raster file to be written
            block_file <- paste0(
                tools::file_path_sans_ext(out_file),
                "_block_", block[["first_row"]], "_",
                block[["nrows"]], ".tif"
            )
            # Read a block
            r_obj <- .raster_crop(
                r_obj = .raster_open_rast(file = fi_row[["path"]]),
                file = block_file,
                format = "GTiff",
                data_type = .raster_data_type(
                    .config_get("class_cube_data_type")
                ),
                gdal_options = .config_gtiff_default_options(),
                overwrite = TRUE,
                block = block
            )
            # Create a mask file based on block
            mask_obj <- .raster_rast(
                r_obj = r_obj,
                nlayers = 1
            )
            # Set init values to NA
            temp_obj <- .raster_set_values(
                r_obj = mask_obj,
                values = NA
            )
            # Block mask file name
            block_mask_file <- paste0(
                tools::file_path_sans_ext(block_file),
                "_mask.tif"
            )
            # Write empty block mask file as template
            .raster_write_rast(
                r_obj = temp_obj,
                file = block_mask_file,
                format = "GTiff",
                data_type = .config_get("class_cube_data_type"),
                gdal_options = .config_gtiff_default_options(),
                overwrite = TRUE,
                missing_value = 0
            )
            # Remove block mask file on function exit
            on.exit(unlink(block_mask_file), add = TRUE)
            # Filter mask cube by start_date end_date and get all files
            mask_files <- slider::slide(mask, function(tile_mask) {
                fi_mask <- .file_info(tile_mask)
                fi_mask_inter <- dplyr::filter(
                    fi_mask,
                    (.data[["start_date"]] <= fi_row[["end_date"]]
                     & .data[["end_date"]] >= fi_row[["start_date"]])
                )
                return(fi_mask_inter[["path"]])
            }) %>% unlist()
            # Check if there is any file intersecting tile's interval
            .check_chr(
                x = mask_files,
                allow_empty = FALSE,
                len_min = 1,
                msg = "mask cube interval does not match cube interval"
            )
            # Warp mask cube into mask block file
            gdalUtilities::gdalwarp(
                srcfile = mask_files,
                dstfile = block_mask_file,
                r = "near",
                multi = TRUE,
                wo = c("NUM_THREADS=ALL_CPUS")
            )
            # Open warped file to process expressions
            mask_obj <- .raster_open_rast(file = block_mask_file)
            # Evaluate expressions
            values <- .rclasf_eval(
                r_obj = r_obj,
                labels_cube = labels_cube,
                mask_obj = mask_obj,
                labels_mask = labels_mask,
                exprs = exprs,
                labels_out = labels_out,
                mask_na_values = mask_na_values
            )
            # Set values
            r_obj <- .raster_set_values(
                r_obj = r_obj,
                values = values
            )
            # Write empty mask file as template
            .raster_write_rast(
                r_obj = r_obj,
                file = block_file,
                format = "GTiff",
                data_type = .config_get("class_cube_data_type"),
                gdal_options = .config_gtiff_default_options(),
                overwrite = TRUE,
                missing_value = 0
            )
            # Clean memory
            gc()
            return(block_file)
        })
        # Merge result
        blocks_path <- unlist(blocks_path)
        # Join predictions
        .raster_merge(
            in_files = blocks_path,
            out_file = out_file,
            format = "GTiff",
            gdal_datatype = .raster_gdal_datatype(
                .config_get("class_cube_data_type")
            ),
            gdal_options = .config_gtiff_default_options(),
            overwrite = TRUE,
            progress = progress
        )
        # Remove block files on function exit
        on.exit(unlink(blocks_path), add = TRUE)
        # Prepare result updating path
        fi_row[["path"]] <- out_file
        # Update tile
        tile[["labels"]] <- list(labels_out)
        tile[["file_info"]] <- fi_row
        return(tile)
    }, progress = progress)
    # Bind all tiles_fid
    result <- tiles_fid %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(
            .data[["source"]], .data[["collection"]],
            .data[["satellite"]], .data[["sensor"]],
            .data[["tile"]], .data[["xmin"]], .data[["xmax"]],
            .data[["ymin"]], .data[["ymax"]], .data[["crs"]],
            .data[["labels"]]
        ) %>%
        dplyr::summarise(
            file_info = list(dplyr::bind_rows(.data[["file_info"]])),
            .groups = "drop"
        )
    # Set class
    class(result) <- class(cube)
    return(result)
}

.rclasf_eval <- function(r_obj,
                         labels_cube,
                         mask_obj,
                         labels_mask,
                         exprs,
                         labels_out,
                         mask_na_values) {

    # Get evaluation environment
    env <- list2env(list(
        # Read values and convert to character
        cube = labels_cube[.raster_get_values(r_obj)],
        mask = labels_mask[.raster_get_values(mask_obj)]
    ))

    # Get cube matrix
    values <- env[["cube"]]

    # Evaluate expressions
    for (label in names(exprs)) {
        expr <- exprs[[label]]
        # evaluate
        result <- eval(expr, envir = env)
        # check
        .check_lgl_type(
            x = result,
            msg = paste0("invalid expression ", expr)
        )
        # update states
        values[result] <- label
    }

    # Produce final matrix
    values <- match(values, labels_out)

    # Mask NA values
    if (mask_na_values) {
        values[is.na(env[["mask"]])] <- NA
    }

    return(values)
}


#' @title List all jobs to be computed
#'
#' @name .rclasf_get_jobs
#' @keywords internal
#'
#' @param cube   Data cube.
#'
#' @return       List of combination among tiles and dates.
#'
.rclasf_get_jobs <- function(cube) {

    tile_fid <- unlist(slider::slide(cube, function(tile) {
        fi <- .file_info(tile)
        purrr::map(seq_len(nrow(fi)), function(i)
            list(tile[["tile"]], i))
    }), recursive = FALSE)

    return(tile_fid)
}

# function to compute blocks grid
.rclasf_compute_blocks <- function(xsize,
                                   ysize,
                                   block_y_size) {
    r1 <- seq(1, ysize - 1, by = block_y_size)
    r2 <- c(r1[-1] - 1, ysize)
    nr1 <- r2 - r1 + 1

    # define each block as a list element
    blocks <- mapply(
        list,
        first_row      = r1,
        nrows          = nr1,
        first_col      = 1,
        ncols          = xsize,
        SIMPLIFY       = FALSE
    )

    return(blocks)
}
#' @title Estimate the number of blocks
#' @name .rclasf_estimate_block_size
#' @keywords internal
#'
#' @param cube         input data cube
#' @param multicores   number of processes to split up the data
#' @param memsize      maximum overall memory size (in GB)
#'
#' @return  returns a list with following information:
#'             - multicores theoretical upper bound;
#'             - block x_size (horizontal) and y_size (vertical)
#'
.rclasf_estimate_block_size <- function(cube, multicores, memsize) {

    # precondition 1 - check if cube has probability data
    .check_that(
        x = inherits(cube, "classified_image"),
        msg = "input is not a classified cube"
    )

    # Get info to compute memory
    size <- .cube_size(cube[1,])
    n_bands <- 1
    n_times <- nrow(.file_info(cube))
    bloat_mem <- .config_processing_bloat()
    n_bytes <- 8

    # total memory needed to do all work in GB
    image_size <- size[["ncols"]] * size[["nrows"]]
    needed_memory <- image_size * 1E-09 * n_bands * n_times *
        bloat_mem * n_bytes

    # minimum block size
    min_block_x_size <- size["ncols"] # for now, only vertical blocking
    min_block_y_size <- 1

    # compute factors
    memory_factor <- needed_memory / memsize

    blocking_factor <- image_size / (min_block_x_size * min_block_y_size)

    # stop if blocking factor is less than memory factor!
    # reason: the provided memory is not enough to process the data by
    # breaking it into small chunks
    .check_that(
        x = memory_factor <= blocking_factor,
        msg = "provided memory not enough to run the job"
    )

    # update multicores to the maximum possible processes given the available
    # memory and blocking factor
    multicores <- min(floor(blocking_factor / memory_factor), multicores)

    # compute blocking allocation that maximizes the
    # block / (memory * multicores) ratio, i.e. maximize parallel processes
    # and returns the following information:
    # - multicores theoretical upper bound;
    # - block x_size (horizontal) and y_size (vertical)
    blocks <- list(
        # theoretical max_multicores = floor(blocking_factor / memory_factor),
        block_x_size = floor(min_block_x_size),
        block_y_size = min(
            floor(blocking_factor / memory_factor / multicores),
            size[["nrows"]]
        )
    )

    return(blocks)
}
