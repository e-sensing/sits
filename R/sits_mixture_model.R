#' @title Multiple endmember spectral mixture analysis
#'
#' @name sits_mixture_model
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Create a multiple endmember spectral mixture analyses fractions
#' images. To calculate the fraction of each endmember, the non-negative least
#' squares (NNLS) solver is used. The NNLS implementation was made by Jakob
#' Schwalb-Willmann in RStoolbox package (licensed as GPL>=3).
#'
#' @references \code{RStoolbox} package (https://github.com/bleutner/RStoolbox/)
#'
#' @param cube                A sits data cube.
#' @param endmembers_spectra  Reference endmembers spectra in a tibble format.
#'                            (see details below).
#' @param memsize             Memory available for mixture model (in GB).
#' @param multicores          Number of cores to be used for generate the
#'                            mixture model.
#' @param output_dir          Directory for output file.
#' @param rmse_band           A boolean indicating whether the error associated
#'                            with the linear model should be generated.
#'                            If true, a new band with the errors for each pixel
#'                            is generated using the root mean square
#'                            measure (RMSE). Default is TRUE.
#' @param remove_outliers     A boolean indicating whether values larger and
#'                            smaller than the limits in the image metadata, and
#'                            missing values should be marked as NA. This
#'                            parameter can be used when the cloud component is
#'                            added to the mixture model. Default is TRUE.
#' @param progress            Show progress bar? Default is TRUE.
#'
#' @note The \code{endmembers_spectra} parameter should be a tibble, csv or
#' a shapefile. \code{endmembers_spectra} must have the following columns:
#' \code{type}, which defines the endmembers that will be
#' created and the columns corresponding to the bands that will be used in the
#' mixture model.
#'
#' @examples
#' if (sits_run_examples()) {
#'    # --- Create a cube based on a local MODIS data
#'    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'
#'    modis_cube <- sits_cube(
#'        source = "BDC",
#'        collection = "MOD13Q1-6",
#'        data_dir = data_dir,
#'        delim = "_"
#'    )
#'
#'    endmembers_spectra <- tibble::tibble(
#'        type = c("vegetation", "not-vegetation"),
#'        NDVI = c(8500, 3400)
#'    )
#'
#'    mixture_cube <- sits_mixture_model(
#'        cube = modis_cube,
#'        endmembers_spectra = endmembers_spectra,
#'        memsize = 4,
#'        multicores = 2,
#'        output_dir = tempdir()
#'    )
#' }
#'
#' @return a sits cube with the generated fractions.
#'
#' @export
sits_mixture_model <- function(cube,
                               endmembers_spectra,
                               memsize = 1,
                               multicores = 2,
                               output_dir = getwd(),
                               rmse_band = TRUE,
                               remove_outliers = TRUE,
                               progress = TRUE) {

    .check_set_caller("sits_mixture_model")

    .check_that(
        inherits(endmembers_spectra, c("tbl_df", "data.frame", "character"))
    )

    if (inherits(endmembers_spectra, "character"))
        endmembers_spectra <- .mesma_get_data(
            endmembers = endmembers_spectra,
            file_ext = tolower(tools::file_ext(endmembers_spectra))
        )

    # Ensure that all columns are in uppercase
    endmembers_spectra <- dplyr::rename_with(endmembers_spectra, toupper)

    # Pre-condition
    .check_chr_contains(
        x = colnames(endmembers_spectra),
        contains = "TYPE",
        msg = paste("The reference endmembers spectra should be provided."),
    )

    # Pre-condition
    .check_chr_within(
        x = colnames(endmembers_spectra),
        within = c("TYPE", .cube_bands(cube, add_cloud = FALSE)),
        msg = "invalid 'endmembers_spectra' columns"
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

    # Take only bands that intersects between two tibbles
    ref_spectral_bands <- intersect(
        .cube_bands(cube, add_cloud = FALSE),
        setdiff(colnames(endmembers_spectra), "TYPE")
    )

    # Scale the reference spectra
    reference_spectra <- .mesma_scale_endmembers(cube, endmembers_spectra)

    output_fracs <- endmembers_spectra[["TYPE"]]
    if (rmse_band)
        output_fracs <- c(output_fracs, "rmse")

    cube_filtered <- sits_select(cube, ref_spectral_bands)

    jobs <- .mesma_get_jobs_lst(
        cube = cube_filtered,
        fractions = output_fracs
    )

    # Already processed?
    if (length(jobs) == 0) {
        return(cube)
    }

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    .sits_parallel_map(jobs, function(job) {

        # Get parameters from each job
        tile_name <- job[[1]]
        fid <- job[[2]]

        # Filter tile
        tile <- dplyr::filter(cube_filtered, tile == !!tile_name)

        in_bands <- .cube_bands(cube_filtered)

        # File_info filtered by bands
        in_fi_fid <- .file_info(
            cube = tile,
            bands = in_bands,
            fid = fid
        )

        output_files <- .file_path(
            "cube", tile_name, output_fracs,
            unique(in_fi_fid[["date"]]),
            ext = ".tif",
            output_dir = output_dir
        )

        names(output_files) <- output_fracs

        # Does output file exists?
        if (all(file.exists(output_files))) {
            message(
                paste(
                    "Recovery mode. Detected fractions bands in",
                    "the provided directory."
                )
            )
            return(output_files)
        }

        # Divide the input data in blocks
        blocks <- .mesma_raster_blocks(
            nbands = length(in_bands),
            sub_image = .sits_raster_sub_image_default(tile),
            memsize = memsize,
            multicores = multicores
        )

        # Save each output value
        block_files <- purrr::map(blocks, function(b) {

            # Load bands data
            in_values <- purrr::map_dfc(in_bands, function(band) {

                # Transform file_info columns as bands and values as paths
                in_files <- in_fi_fid %>%
                    dplyr::select(dplyr::all_of(c("band", "path"))) %>%
                    tidyr::pivot_wider(
                        names_from = "band",
                        values_from = "path"
                    )

                # Scale the data set
                scale_factor <- .cube_band_scale_factor(tile, band = band)
                offset_value <- .cube_band_offset_value(tile, band = band)

                # Read the values
                values <- .raster_read_rast(in_files[[band]], block = b)

                if (remove_outliers) {
                    # Get the missing values, minimum values and scale factors
                    missing_value <- .cube_band_missing_value(tile, band = band)
                    minimum_value <- .cube_band_minimum_value(tile, band = band)
                    maximum_value <- .cube_band_maximum_value(tile, band = band)

                    # Correct NA, minimum, maximum, and missing values
                    values[values == missing_value] <- NA
                    values[values < minimum_value] <- NA
                    values[values > maximum_value] <- NA
                }
                # compute scale and offset
                values <- scale_factor * values + offset_value
                values <- as.data.frame(values)
                return(values)
            })

            # Set band names
            names(in_values) <- in_bands
            in_values <- as.matrix(in_values)

            # Apply the non-negative least squares solver
            out_values <- nnls_solver(
                x = in_values,
                A = reference_spectra
            )

            # Apply scale and offset
            out_values <- out_values /
                .config_get("raster_cube_scale_factor") -
                .config_get("raster_cube_offset_value")

            # Remove the rmse value in the last column
            if (!rmse_band)
                out_values <- out_values[, -ncol(out_values)]

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
                nlayers = length(output_fracs),
                crs = params[["crs"]]
            )

            # Set values
            r_obj <- .raster_set_values(r_obj, out_values)
            names(r_obj) <- output_fracs

            filenames_block <- purrr::map_chr(names(r_obj), function(frac) {

                output_files_frac <-
                    output_files[names(output_files) == frac]

                # Define the file name of the raster file to be written
                filename_block <- paste0(
                    tools::file_path_sans_ext(output_files_frac),
                    "_block_", b[["row"]], "_", b[["nrows"]], ".tif"
                )

                # Write values
                .raster_write_rast(
                    r_obj = r_obj[[frac]],
                    file = filename_block,
                    data_type = .config_get("raster_cube_data_type"),
                    overwrite = TRUE
                )

                # Clean memory
                gc()

                names(filename_block) <- frac
                return(filename_block)
            })

            names(filenames_block) <- names(r_obj)
            return(filenames_block)
        })

        # Merge result
        block_files <- unlist(block_files)

        # Join predictions
        if (is.null(block_files)) {
            return(NULL)
        }

        output_file_fracs <- purrr::map_chr(output_fracs, function(frac) {

            blocks_fracs_path <- block_files[names(block_files) == frac]
            output_frac_path <- output_files[names(output_files) == frac]

            # Merge final result
            .raster_merge_blocks(
                base_file = .file_info_path(tile),
                block_files = blocks_fracs_path,
                out_file = output_frac_path,
                data_type = .config_get("raster_cube_data_type"),
                missing_value = .config_get("raster_cube_missing_value"),
                multicores = 1
            )

            # Remove blocks
            on.exit(unlink(blocks_fracs_path), add = TRUE)

            return(output_frac_path)
        })


        return(output_file_fracs)
    }, progress = progress)

    # Create local cube from files in output directory
    local_cube <- sits_cube(
        source = .cube_source(cube),
        collection = .cube_collection(cube),
        data_dir = output_dir,
        bands = output_fracs,
        parse_info = c("X1", "tile", "band", "date"),
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

    utils::read.csv(endmembers)
}

.mesma_get_data.shp <- function(endmembers, file_ext) {

    sf::st_read(endmembers)
}

#' @title Create a list of jobs
#'
#' @name .apply_missing_band
#' @keywords internal
#'
#' @param cube       Data cube.
#' @param fractions  Name of output fractions.
#'
#' @return           List of combination among tiles, endmembers, and dates
#'                   that are missing from the cube.
.mesma_get_jobs_lst <- function(cube, fractions) {

    # Define function to show in case of error
    .check_set_caller(".mesma_get_jobs_lst")

    tile_datetime <- unlist(slider::slide(cube, function(tile) {
        tl <- sits_timeline(tile)
        fi <- .file_info(tile)

        fi_band <- fi[fi[["band"]] %in% fractions, ]
        missing_dates <- tl[!tl %in% unique(fi_band[["date"]])]
        fi <- fi[fi[["date"]] %in% missing_dates, ]

        if (nrow(fi) == 0) {
            return(NULL)
        }
        purrr::cross2(.cube_tiles(tile), unique(fi[["fid"]]))
    }), recursive = FALSE)

    return(tile_datetime)
}

#' @title Scale the endmembers spectra
#'
#' @name .mesma_scale_endmembers
#' @keywords internal
#'
#' @param cube                Data cube.
#' @param endmembers_spectra  Tibble with endmembers spectra values.
#'
#' @return a matrix with spectral values scaled.
.mesma_scale_endmembers <- function(cube, endmembers_spectra) {

    endmembers_bands <- setdiff(colnames(endmembers_spectra), "TYPE")
    em_spec <- dplyr::select(
        endmembers_spectra,
        dplyr::all_of(!!endmembers_bands)
    )

    em_spec <- dplyr::mutate(
        em_spec,
        dplyr::across(
            .cols = endmembers_bands,
            ~ .x * .cube_band_scale_factor(cube[1,], band = dplyr::cur_column())
        )
    )

    return(as.matrix(em_spec))
}
#' @title Define a reasonable block size to process an image subset
#' @name .mesma_raster_blocks
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the size of the block of an image to be read.
#' For example, a Raster Brick with 500 rows and 500 columns
#' and 400 time instances will have a total pixel size
#' of 800 Mb if pixels are 64-bit.
#'
#' @param  nbands     Number of bands to open.
#' @param  sub_image  Bounding box of the ROI.
#' @param  memsize    Memory available for classification (in GB).
#' @param  multicores Number of cores to process the time series.
#' @return            List with three attributes: n (number of blocks),
#'                    rows (list of rows to begin),
#'                    nrows (number of rows to read at each iteration).
#'
.mesma_raster_blocks <- function(nbands, sub_image, memsize, multicores) {

    # Get the number of blocks
    nblocks <- .mesma_raster_blocks_estimate(
        nbands = nbands,
        sub_image = sub_image,
        memsize = memsize,
        multicores = multicores
    )

    blocks <- .sits_raster_block_list(
        nblocks = nblocks,
        sub_image = sub_image
    )

    return(blocks)
}

#' @title Estimate the number of blocks
#' @name .mesma_raster_blocks_estimate
#' @keywords internal
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Defines the number of blocks of a Raster Brick
#'              to be read into memory.
#'
#' @param  nbands     Number of bands to open.
#' @param  sub_image  Area of interest in the image.
#' @param  memsize    Memory available for classification (in GB).
#' @param  multicores Number of cores to process the time series.
#' @return            Number of blocks to be read.
.mesma_raster_blocks_estimate <- function(nbands,
                                          sub_image,
                                          memsize,
                                          multicores) {

    # Number of bytes per pixel
    nbytes <- 8
    # Estimated processing bloat
    proc_bloat <- as.numeric(.config_processing_bloat())
    if (proc_bloat == 0) proc_bloat <- multicores

    # Number of rows and cols
    nrows <- sub_image[["nrows"]]
    ncols <- sub_image[["ncols"]]
    # Single instance size
    output_data_size <- nrows * ncols * nbytes
    # Total size including all bands
    input_data_size <- output_data_size * nbands

    # Number of output instances is the same as input
    # Estimated size of the data for apply
    class_data_size <- (input_data_size + output_data_size) * proc_bloat

    # Number of passes to read the full data sets
    nblocks <- ceiling(class_data_size * 1e-09 / memsize * multicores)

    return(nblocks)
}
