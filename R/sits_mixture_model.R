#' @title Multiple endmember spectral mixture analysis
#'
#' @name sits_mixture_model
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos,   \email{efelipecarlos@@gmail.com}
#' @author Rolf Simoes,     \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez,   \email{alber.ipia@@inpe.br}
#'
#' @description Create a multiple endmember spectral mixture analyses fractions
#' images. We use the non-negative least squares (NNLS) solver to calculate the
#' fractions of each endmember. The NNLS was implemented by Jakob
#' Schwalb-Willmann in RStoolbox package (licensed as GPL>=3).
#'
#' @references \code{RStoolbox} package (https://github.com/bleutner/RStoolbox/)
#'
#' @param cube                A sits data cube.
#' @param endmembers          Reference spectral endmembers.
#'                            (see details below).
#' @param memsize             Memory available for the mixture model (in GB).
#' @param multicores          Number of cores to be used for generate the
#'                            mixture model.
#' @param output_dir          Directory for output images.
#' @param rmse_band           A boolean indicating whether the error associated
#'                            with the linear model should be generated.
#'                            If true, a new band with errors for each pixel
#'                            is generated using the root mean square
#'                            measure (RMSE). Default is TRUE.
#' @param remove_outliers     A boolean indicating whether values larger and
#'                            smaller than the limits of the image metadata,
#'                            and missing values should be marked as NA.
#'                            Default is TRUE.
#' @param progress            Show progress bar? Default is TRUE.
#' @return a sits cube with the fractions of each endmember.
#'         The sum of all fractions is restricted to 1 (scaled from 0 to 10000),
#'         corresponding to the abundance of the endmembers in the pixels.
#'
#' @details
#'
#' The \code{endmembers} parameter should be a tibble, csv or
#' a shapefile. \code{endmembers} parameter must have the following columns:
#' \code{type}, which defines the endmembers that will be
#' created and the columns corresponding to the bands that will be used in the
#' mixture model. See the \code{example} in this documentation for more details.
#'
#' If you want to generate cloud endmembers,
#' it is useful to set the parameter \code{remove_outliers} to \code{FALSE}.
#' Some image products have cloud values that exceed the limits set by the
#' metadata, and therefore these values are removed if this option
#' is \code{TRUE}.
#'
#' @examples
#' if (sits_run_examples()) {
#'    # Creating a sentinel-2 AWS cube
#'    s2_cube <- sits_cube(
#'        source = "AWS",
#'        collection = "SENTINEL-S2-L2A-COGS",
#'        tiles = "20LKP",
#'        bands = c("B02", "B03", "B04", "B8A", "B11", "B12", "CLOUD"),
#'        start_date = "2019-06-13",
#'        end_date = "2019-06-30"
#'    )
#'
#'    # Cube regularization for 16 days and 160 meters
#'    reg_cube <- sits_regularize(
#'        cube = s2_cube,
#'        period = "P16D",
#'        res = 160,
#'        multicores = 2,
#'        output_dir = tempdir()
#'    )
#'
#'    # Create the endmembers fractions tibble
#'    em <- tibble::tribble(
#'           ~type, ~B02, ~B03, ~B04, ~B8A, ~B11, ~B12,
#'        "forest",  200,  352,  189, 2800, 1340,  546,
#'          "land",  400,  650,  700, 3600, 3500, 1800,
#'         "water",  700, 1100, 1400,  850,   40,   26,
#'    )
#'
#'    # Generate the mixture model
#'    mm <- sits_mixture_model(
#'        cube = reg_cube,
#'        endmembers = em,
#'        memsize = 4,
#'        multicores = 2,
#'        output_dir = tempdir()
#'    )
#' }
#'
#' @export
sits_mixture_model <- function(cube,
                               endmembers,
                               memsize = 1,
                               multicores = 2,
                               output_dir = getwd(),
                               rmse_band = TRUE,
                               remove_outliers = TRUE,
                               progress = TRUE) {
    # check documentation mode
    progress <- .check_documentation(progress)

    # precondition - test if cube is regular
    .check_is_regular(cube)
    #precondition - endmembers
    .check_inherits(x = endmembers, inherits = c("data.frame", "character"))
    # precondition - memsize
    .check_memsize(memsize)
    # precondition - multicores
    .check_multicores(multicores)
    # precondition - output dir
    output_dir <- path.expand(output_dir)
    .check_output_dir(output_dir)
    # precondition - rmse_band
    .check_lgl_type(rmse_band)
    # precondition - remove_outiliers
    .check_lgl_type(remove_outliers)
    # precondition - progress
    .check_lgl_type(progress)

    endmembers <- .mm_format_endmembers(endmembers, cube)
    .check_endmembers_parameter(endmembers, cube)

    # In case cube bands  is different from endmembers bands
    in_bands <- .mm_intersect_bands(endmembers, cube)
    cube <- .cube_select(cube, in_bands)

    output_fracs <- .mm_get_fractions_name(endmembers, rmse_band)

    steps <- .mm_get_steps_lst(cube, output_fracs)

    # Already processed?
    if (length(steps) == 0) {
        return(cube)
    }

    # Prepare parallelization
    .sits_parallel_start(workers = multicores, log = FALSE)
    on.exit(.sits_parallel_stop(), add = TRUE)

    .sits_parallel_map(steps, function(step) {

        # Get parameters from each job
        tile_name <- step[[1]]
        fid <- step[[2]]

        # Filter tile
        tile <- dplyr::filter(cube, tile == !!tile_name)

        # File_info filtered by bands
        in_fi_fid <- .file_info(cube = tile, bands = in_bands, fid = fid)

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
        # Create jobs
        # Get job size
        job_size <- .raster_file_blocksize(
            .raster_open_rast(.fi_path(.fi(tile)))
        )
        # Compute how many jobs to process
        # Add roi parameter? its make sense?
        jobs <- .jobs_create(
            job_size = job_size, block_overlap = 0,
            ncols = .tile_ncols(tile), nrows = .tile_nrows(tile),
            xmin = .xmin(tile), xmax = .xmax(tile),
            ymin = .ymin(tile), ymax = .ymax(tile),
            crs = .crs(tile), roi = NULL
        )

        # Save each output value
        block_files <- purrr::map(jobs, function(job) {

            block <- .block(job)
            # Read bands data
            values <- purrr::map_dfc(in_bands, function(band) {

                values <- .fi_read_block(fi = in_fi_fid, band = band,
                                         block = block)

                # Scale the data set
                scale_factor <- .cube_band_scale_factor(tile, band = band)
                offset_value <- .cube_band_offset_value(tile, band = band)

                missing_value <- .cube_band_missing_value(tile, band = band)
                if (!is.null(missing_value)) {
                    values[values == missing_value] <- NA
                }

                minimum_value <- .cube_band_minimum_value(tile, band = band)
                maximum_value <- .cube_band_maximum_value(tile, band = band)

                # Correct NA, minimum, maximum, and missing values

                values[values < minimum_value] <- NA
                values[values > maximum_value] <- NA
                # compute scale and offset
                values <- scale_factor * values + offset_value
                values <- as.data.frame(values)
                return(values)
            })

            # Scale the reference spectra
            em_scaled <- .mm_scale_endmembers(endmembers, cube)
            # Apply the non-negative least squares solver
            values <- nnls_solver(
                x = as.matrix(values),
                A = em_scaled
            )

            # Apply scale and offset
            values <- values /
                .config_get("raster_cube_scale_factor") -
                .config_get("raster_cube_offset_value")

            # Remove the rmse value in the last column
            if (!rmse_band) {
                values <- values[, -ncol(values)]
            }

            # Compute block spatial parameters
            params <- .cube_params_block(tile, block = job)

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
            r_obj <- .raster_set_values(r_obj, values)
            names(r_obj) <- output_fracs

            filenames_block <- purrr::map_chr(names(r_obj), function(frac) {

                output_files_frac <-
                    output_files[names(output_files) == frac]

                # Define the file name of the raster file to be written
                filename_block <- paste0(
                    tools::file_path_sans_ext(output_files_frac),
                    "_block_", job[["row"]], "_", job[["nrows"]], ".tif"
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

.mm_format_endmembers <- function(endmembers, cube) {
    if (!inherits(endmembers, "character")) {
        endmembers <- .mesma_get_data(
            endmembers = endmembers,
            file_ext = tolower(tools::file_ext(endmembers))
        )
    }
    # Ensure that all columns are in uppercase
    endmembers <- dplyr::rename_with(endmembers, toupper)

    return(endmembers)
}

.mm_get_fractions_name <- function(endmembers, rmse_band) {
    output_fracs <- endmembers[["TYPE"]]
    if (rmse_band) {
        output_fracs <- c(output_fracs, "RMSE")
    }
}

.mm_intersect_bands <- function(endmembers, cube) {
    intersect(
        setdiff(colnames(endmembers), "TYPE"),
        .cube_bands(cube, add_cloud = FALSE)
    )
}

#' @title Create a list of steps
#'
#' @name .mm_get_steps_lst
#' @keywords internal
#'
#' @param cube             Data cube.
#' @param output_fractions Name of output fractions.
#'
#' @return           List of combination among tiles, endmembers, and dates
#'                   that are missing from the cube.
.mm_get_steps_lst <- function(cube, output_fractions) {
    tile_datetime <- unlist(slider::slide(cube, function(tile) {
        tl <- .tile_timeline(tile)
        fi <- .fi(tile)

        fi_band <- fi[fi[["band"]] %in% output_fractions, ]
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
#' @param endmembers Tibble with endmembers spectra values.
#' @param cube       Data cube.
#'
#' @return a matrix with spectral values scaled.
.mm_scale_endmembers <- function(endmembers, cube) {

    endmembers_bands <- setdiff(colnames(endmembers), "TYPE")
    em_spec <- dplyr::select(endmembers, dplyr::all_of(!!endmembers_bands))

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
