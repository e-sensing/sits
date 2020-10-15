#' @title Clean data cube to improve quality
#' @name  sits_cloud_remove
#'
#' @param cube       input data cube
#' @param data_dir   data directory where output data is written
#' @param name       name of the output data cube
#' @param impute_fn  imputing function to be applied to replace NA
#' @param memsize    size of memory
#' @param multicores number of cores
#'
#' @return           new data cube with interpolated cloud data
#'
#' @export
#'
sits_cloud_remove <- function(cube,
                              data_dir,
                              name,
                              impute_fn = sits_impute_linear(),
                              memsize = 8,
                              multicores = 2){

	# precondition - is there cloud information available
	cloud_band <- .sits_config_cloud_band(cube)
	assertthat::assert_that(cloud_band %in% sits_bands(cube),
							msg = "cloud information band not available in cube")

	# precondition
	assertthat::assert_that(assertthat::is.dir(data_dir),
	                        msg = "invalid data directory")


	# estimate the blocks to be read
	blocks <- .sits_clouds_blocks_estimate(cube = cube,
	                                       memsize = memsize)

	# interpolate the cloud bricks
	files <- .sits_clouds_interpolate(cube       = cube,
	                                  data_dir   = data_dir,
	                                  blocks     = blocks,
	                                  impute_fn  = impute_fn,
	                                  multicores = multicores)

	# find out what are the bands of the cube
	bands <- sits_bands(cube)
	bands <- bands[bands != cloud_band]

	# create the output cube
	cube_new <- .sits_raster_brick_cube(satellite = cube$satellite,
	                                    sensor    = cube$sensor,
	                                    name      = name,
	                                    timeline  = sits_timeline(cube),
	                                    bands     = bands,
	                                    files     = files)

	return(cube_new)

}
#' @title Estimate the number of blocks to correct for clouds
#' @name .sits_clouds_blocks_estimate
#' @keywords internal
#'
#' @param cube    input data cube
#' @param memsize size of memory available
#'
#' @return        list with:
#'                n (number of blocks),
#'                row (vector of starting rows)
#'                nrow (vector with number of rows for each block)
#'                size (vector with size of each block)

.sits_clouds_blocks_estimate <- function(cube, memsize) {

	# total number of instances
	n_instances <- length(sits_timeline(cube))

	# number of bytes per pixel
	nbytes <-  8
	# estimated memory bloat
	bloat <- as.numeric(.sits_config_memory_bloat())

	# number of rows and cols
	nrows <- cube$nrows
	ncols <- cube$ncols

	# single instance size 2 bands (input - output) and the cloud band)
	single_data_size <- as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)*3

	# estimated full size of the data
	full_size <- as.numeric(n_instances)*single_data_size

	# estimated size of memory required
	mem_required <- (full_size + as.numeric(.sits_mem_used()))*bloat

	# number of passes to read the full data sets
	nblocks <- ceiling(mem_required/(memsize*1e+09))

	# list of blocks with number of rows to be read for each block
	#
	# Cloud processing uses the whole image
	sub_image <- .sits_raster_sub_image_default(cube)

	# calculate the blocks
	blocks <- .sits_raster_block_list(nblocks = nblocks,
									  sub_image = sub_image)

	return(blocks)

}

#' @title Create the bricks that will be the output of the cloud estimation procedure
#' @name .sits_clouds_interpolate
#' @keywords internal
#'
#' @param cube        input data cube
#' @param data_dir    directory where data is to be stored
#' @param blocks      block information
#' @param impute_fn   imputation function to remove NA
#' @param multicores  number of cores to use
#'
#' @return            a tibble with date, band and path information
#'
.sits_clouds_interpolate <- function(cube, data_dir, blocks,
                                    impute_fn, multicores) {

    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting cloud conversion at %s", start_time))

	# define the bands
	cloud_band   <- .sits_config_cloud_band(cube)
	bands        <- sits_bands(cube)
	bands_no_cloud <- bands[bands != cloud_band]

	# define the cloud band
    obj_cld <- .sits_cube_terra_obj_band(cube, cloud_band)
    cld_index <- .sits_config_cloud_valid_values(cube)

    # process the bands
	file.lst <- purrr::map(bands_no_cloud, function(band){
	    message(paste0("Removing clouds from band ", band))
	    start_task_time <- lubridate::now()

	    # define the input band
	    obj_band <- .sits_cube_terra_obj_band(cube, band)
	    # read the blocks
	    values.lst <- purrr::map(c(1:blocks$n), function(b) {
	        # measure performance
	        start_block_time <- lubridate::now()
	        # define the extent
	        extent <- c(blocks$row[b], blocks$nrows[b],
	                    blocks$col, blocks$ncols)
	        names(extent) <- (c("row", "nrows", "col", "ncols"))
	        # read the cloud data
	        terra::readStart(obj_cld)
	        clouds.mx <- terra::readValues(x      = obj_cld,
	                                       row    = extent["row"],
	                                       nrows  = extent["nrows"],
	                                       col    = extent["col"],
	                                       ncols  = extent["ncols"],
	                                       mat = TRUE)
	        terra::readStop(obj_cld)

	        # read the values
	        terra::readStart(obj_band)
	        values.mx    <- terra::readValues(x      = obj_band,
	                                          row    = extent["row"],
	                                          nrows  = extent["nrows"],
	                                          col    = extent["col"],
	                                          ncols  = extent["ncols"],
	                                          mat = TRUE)
	        terra::readStop(obj_band)

	        # preprocess the input data
	        values.mx <- .sits_raster_preprocess_data(cube,
	                                                  values.mx,
	                                                  band,
	                                                  clouds.mx,
	                                                  cld_index,
	                                                  impute_fn,
	                                                  multicores)
	        rm(clouds.mx)
	        gc()

	        task <- paste0("process block ", b, " of band ", band)
	        .sits_processing_estimate_task_time(task, start_block_time)

	        return(values.mx)
	    })

	    new_values.mx <- do.call(rbind, values.lst)
	    rm(values.lst)
	    gc()

	    band_files <- dplyr::filter(cube$file_info[[1]], band == band)
	    nlyrs <- nrow(band_files)

	    brick <- terra::rast(nrows  = cube$nrows,
	                         ncols  = cube$ncols,
	                         nlyrs  = nlyrs,
	                         xmin   = cube$xmin,
	                         xmax   = cube$xmax,
	                         ymin   = cube$ymin,
	                         ymax   = cube$ymax,
	                         crs    = cube$crs)

	    start_date <- band_files[1,]$date
	    end_date   <- band_files[nlyrs,]$date

	    filename <- paste0(data_dir, "/", cube$satellite, "_", cube$sensor, "_",
	                       start_date,"_", end_date, "_",
	                       band, "_CLD_REM", ".tif")

	    terra::writeStart(brick,
	                       filename = filename,
	                       wopt     = list(filetype  = "GTiff",
	                                       datatype = "INT2U"),
	                       overwrite = TRUE)
	    terra::writeValues(x = brick,
	                       v = new_values.mx,
	                       start = 1,
	                       row = 1,
	                       nrows = cube$nrows,
	                       col = 1,
	                       ncols = cube$ncols)
	    terra::writeStop(brick)

	    task <- paste0("Removed clouds from band ", band)
	    .sits_processing_estimate_task_time(task, start_task_time)
	    return(filename)
	})

	# report on time used for processing
	task <- paste0("Removed clouds from all bands")
	.sits_processing_estimate_task_time(task, start_time)

    # return the file info
	files <- unlist(file.lst)
	return(files)
}
