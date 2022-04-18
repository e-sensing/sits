#' @title Create a multiple endmember spectral mixture analyses fractions images
#'
#' @description A multiple endmember spectral mixture analyses...
#'
#' @param cube       ...
#' @param endmembers ...
#' @param memsize    ...
#' @param multicores ...
#' @param output_dir ...
#' @param progress   ...
#'
#' @return a cube ...
sits_spectral_unmixing <- function(cube,
                                   endmembers_spectra,
                                   memsize = 1,
                                   multicores = 2,
                                   output_dir = getwd(),
                                   progress = TRUE) {

    .check_set_caller("sits_spectral_unmixing")

    .check_that(
        inherits(endmembers_spectra, c("tbl_df", "tbl", "data.frame", "character")),
        msg = "invalid endmembers parameter."
    )

    if (inherits(endmembers_spectra, "character"))
        endmembers_spectra <- .mesma_get_data(
            endmembers = endmembers_spectra,
            file_ext = tolower(tools::file_ext(endmembers_spectra))
        )

    endmembers_spectra <- dplyr::rename_with(endmembers_spectra, toupper)

    # Pre-condition
    .check_chr_contains(
        colnames(endmembers_spectra),
        contains = c("TYPE", .cube_bands(cube, add_cloud = FALSE)),
        msg = "invalid endmembers columns"
    )

    # Pre-condition
    .check_that(
        nrow(endmembers_spectra) > 1,
        msg = "at least two endmembers fractions must be provided."
    )

    # Pre-condition
    .check_that(
        nrow(endmembers_spectra) < .cube_bands(cube, add_cloud = FALSE),
        msg = "Endmembers must be less than the number of spectral bands."
    )

    # Check output_dir
    output_dir <- path.expand(output_dir)
    .check_file(output_dir, msg = "invalid output directory")

    # Take only columns take intersects between two tibbles
    reference_spectra_bands <- intersect(
        setdiff(colnames(endmembers_spectra), "TYPE"),
        .cube_bands(cube, add_cloud = FALSE)
    )

    reference_spectra <- as.matrix(endmembers_spectra[, reference_spectra_bands])
    endmembers_fractions <- endmembers_spectra[["TYPE"]]

    cube_selected <- sits_select(cube, reference_spectra_bands)

    jobs <- .mesma_get_jobs_lst(
        cube = cube_selected,
        endmembers = endmembers_fractions
    )

    # Already processed?
    if (length(jobs) == 0) {
        return(cube)
    }

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    images_lst <- .sits_parallel_map(jobs, function(job) {

        # Get parameters from each job
        tile_name <- job[[1]]
        fid <- job[[2]]

        # Filter tile
        tile <- dplyr::filter(cube_selected, .data[["tile"]] == !!tile_name)

        in_bands <- .cube_bands(cube_selected)

        # File_info filtered by bands
        in_fi_fid <- .file_info(
            cube = tile,
            bands = in_bands,
            fid = fid
        )

        # output fractions files
        output_filenames <- .create_filename(
            "cube", tile_name, c(endmembers_fractions, "probs"),
            in_fi_fid[["date"]],
            ext = ".tif",
            output_dir = output_dir
        )

        names(output_filenames) <- c(endmembers_fractions, "probs")

        # Does output file exists?
        if (all(file.exists(output_filenames))) {
            # TODO: if any output_filenames exists
            # returning a message to the user because some files
            # in not in directory
            return(output_filenames)
        }

        # Divide the input data in blocks
        blocks <- .apply_raster_blocks(
            nbands = length(in_bands),
            sub_image = .sits_raster_sub_image_default(tile),
            memsize = memsize,
            multicores = multicores
        )

        # Save each output value
        blocks_path <- purrr::map(blocks, function(b) {

            # Load bands data
            in_values <- purrr::map_dfc(in_bands, function(band) {

                # Transform file_info columns as bands and values as paths
                in_files <- in_fi_fid %>%
                    dplyr::select(dplyr::all_of(c("band", "path"))) %>%
                    tidyr::pivot_wider(
                        names_from = "band",
                        values_from = "path"
                    )

                # Get the missing values, minimum values and scale factors
                missing_value <- .cube_band_missing_value(tile, band = band)
                minimum_value <- .cube_band_minimum_value(tile, band = band)
                maximum_value <- .cube_band_maximum_value(tile, band = band)

                # Scale the data set
                scale_factor <- .cube_band_scale_factor(tile, band = band)
                offset_value <- .cube_band_offset_value(tile, band = band)

                # Read the values
                values <- .raster_read_stack(in_files[[band]], block = b)

                # # Correct NA, minimum, maximum, and missing values
                # TODO
                # is it necessary correcting for the spectra mixture model?
                # values[values == missing_value] <- NA
                # values[values < minimum_value] <- NA
                # values[values > maximum_value] <- NA

                # compute scale and offset
                # TODO
                # is it necessary scaling for the spectra mixture model?
                #values <- scale_factor * values + offset_value
                values <- as.data.frame(values)
                return(values)
            })

            # Set band names
            names(in_values) <- in_bands
            in_values <- as.matrix(in_values)

            # Evaluate expressions, scale and offset values
            out_values <- nnls_solver(
                x = in_values,
                A = reference_spectra
            )

            # Apply scale and offset
            # TODO:
            # is this will be scaled?
            # out_values <- out_values /
            #     .config_get("raster_cube_scale_factor") -
            #     .config_get("raster_cube_offset_value")

            # Compute block spatial parameters
            params <- .cube_params_block(tile, block = b)

            # New raster
            r_obj <- .raster_new_rast(
                nrows = params[["nrows"]],
                ncols = params[["ncols"]],
                xmin = params[["xmin"]],
                xmax = params[["xmax"]],
                ymin = params[["ymin"]],
                ymax = params[["ymax"]],
                nlayers = length(endmembers_fractions) + 1,
                crs = params[["crs"]]
            )

            # Set values
            r_obj <- .raster_set_values(r_obj, out_values)

            names(r_obj) <- c(endmembers_fractions, "probs")

            filenames_block <- purrr::map_chr(names(r_obj), function(em_fraction) {

                output_filenames_frac <-
                    output_filenames[names(output_filenames) == em_fraction]

                # Define the file name of the raster file to be written
                filename_block <- paste0(
                    tools::file_path_sans_ext(output_filenames_frac),
                    "_block_", b[["first_row"]], "_", b[["nrows"]], ".tif"
                )

                # Write values
                .raster_write_rast(
                    r_obj = r_obj[[em_fraction]],
                    file = filename_block,
                    format = "GTiff",
                    data_type = "FLT4S", # TODO: change this
                    gdal_options = .config_gtiff_default_options(),
                    overwrite = TRUE
                )

                # Clean memory
                gc()

                names(filename_block) <- em_fraction
                return(filename_block)
            })

            names(filenames_block) <- names(r_obj)
            return(filenames_block)
        })


        # Merge result
        blocks_path <- unlist(blocks_path)

        # Join predictions
        if (!is.null(blocks_path)) {

            output_file_fractions <- purrr::map_chr(c(endmembers_fractions, "probs"), function(em_frac) {

                blocks_fracs_path <- blocks_path[names(blocks_path) == em_frac]
                output_frac_path <- output_filenames[names(output_filenames) == em_frac]
                .raster_merge(
                    in_files = blocks_fracs_path,
                    out_file = output_frac_path,
                    format = "GTiff",
                    gdal_datatype = "FLT4S", # TODO: change this
                    gdal_options = .config_gtiff_default_options(),
                    overwrite = TRUE,
                    progress = progress
                )

                return(output_frac_path)
            })


        }

        return(output_file_fractions)
    })

    # Create local cube from files in output directory
    local_cube <- sits_cube(
        source = .cube_source(cube),
        collection = .cube_collection(cube),
        data_dir = output_dir,
        parse_info = c("x1", "tile", "band", "date"),
        multicores = multicores,
        progress = progress
    )

    return(local_cube)
}

.mesma_get_data <- function(endmembers, file_ext) {

    class(file_ext) <- file_ext

    UseMethod(".mesma_get_data", file_ext)
}

.mesma_get_data.csv <- function(endmembers, file_ext) {

    read.csv(endmembers)
}

.mesma_get_data.shp <- function(endmembers, file_ext) {

    sf::st_read(endmembers)
}

#' @title Finds the missing bands in a cube
#'
#' @name .apply_missing_band
#' @keywords internal
#'
#' @param cube       Data cube.
#' @param endmembers   ...
#'
#' @return           List of combination among tiles, endmembers, and dates
#'                   that are missing from the cube.
#'
.mesma_get_jobs_lst <- function(cube, endmembers) {

    # Define function to show in case of error
    .check_set_caller(".mesma_get_jobs_lst")

    tile_datetime <- unlist(slider::slide(cube, function(tile) {
        tl <- sits_timeline(tile)
        fi <- .file_info(tile)

        fi_band <- fi[fi[["band"]] %in% endmembers, ]
        missing_dates <- tl[!tl %in% unique(fi_band[["date"]])]
        fi <- fi[fi[["date"]] %in% missing_dates, ]

        if (nrow(fi) == 0) {
            return(NULL)
        }
        purrr::cross2(.cube_tiles(tile), unique(fi[["fid"]]))
    }), recursive = FALSE)

    return(tile_datetime)
}
