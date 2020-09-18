#' @title Clean data cube to improve quality
#' @name  sits_cloud_remove
#'
#' @param cube       input data cube
#' @param ...        other parameters to be passed for specific types
#'
#' @export
#'
sits_cloud_remove <- function(cube, ...) {

	class_type <- .sits_config_cube_specific(cube$type)
	class(cube) <- c(class_type, class(cube))
	# Dispatch
	UseMethod("sits_cube", cube)
}

#' @title Clean data cube to improve quality for S2 L2A AWS
#' @name  sits_cloud_remove.s2_l2a_aws
#'
#' @param cube       input data cube
#' @param data_dir   data directory where output data is written
#' @param bands      bands to be processed
#' @param interval   time between the output images
#' @param memsize    size of memory
#' @param multicores number of cores
#' @param ...        other parameters to be passed for specific types
#'
#' @export
#'
sits_cloud_remove.s2_l2a_aws <- function(cube, data_dir, bands = NULL,
										 interval = "16 day",
										 memsize = 8,
										 multicores = 2, ...){

	# precondition - is there cloud information available
	cloud_band <- .sits_config_cloud_band(cube$satellite)
	assertthat::assert_that(cloud_band %in% sits_bands(cube),
							msg = "cloud information band not available in cube")

	if (!purrr::is_null(bands)) {
		assertthat::assert_that(all(bands %in% sits_bands(cube)),
								msg = "bands not available in cube")
		bands <- c(bands, cloud_band)
	}
	else
		bands <- sits_bands(cube)

	# precondition
	assertthat::assert_that(lubridate::is.duration(lubridate::as.duration(interval)),
							msg = "invalid interval specification")

	# estimate the blocks to be read
	blocks <- .sits_cloud_blocks_estimate(cube = cube, memsize = memsize)

	# create the output bricks
	bricks <- .sits_cloud_create_output(cube, bands, interval = "16 day", data_dir)

	# interpolate the cloud bricks
	file_info_out <- .sits_interpolate_clouds(cube       = cube,
											  data_dir   = data_dir,
											  bricks     = bricks,
											  blocks     = blocks,
										      multicores = multicores)

}
#'
#' @title Interpolate a series of bands based on the cloud information
#' @name  .sits_interpolate_clouds
#' @keywords internal
#'
#' @param  cube          input data cube
#' @param  data_dir      data directory where output data is written
#' @param  bricks        list of brick objects pointing to output files
#' @param  blocks        list of blocks to read
#' @param  multicores    number of cores to process
#'
#' @return file_info  file information on the output images
#'
.sits_interpolate_clouds <- function(cube, data_dir, bricks, blocks, multicores) {

	# find out the new timeline




}
#' @title Estimate the number of blocks to correct for clouds
#' @name .sits_cloud_blocks_estimate
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

.sits_cloud_blocks_estimate <- function(cube, memsize) {

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

	.sits_log_debug(paste0("max memory required for cloud removal(GB) - ",
						   round(mem_required/1e+09, digits = 3)))

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
#' @name .sits_cloud_create_output
#' @keywords internal
#'
#' @param cube        input data cube
#' @param bands       bands to be included in the output
#' @param interval    temporal interval between images of the cube
#' @param data_dir    directory where data is to be stored
#'
#' @return            A list of brick objects pointing to output files
#'
.sits_cloud_create_output <- function(cube, bands = NULL, interval = "16 day", data_dir) {

	# precondition
	if (purrr::is_null(bands))
		bands <- sits_bands(cube)
	assertthat::assert_that(all(bands %in% sits_bands),
							msg = "requested bands not available in the data cube")
	# precondition
	assertthat::assert_that(lubridate::is.duration(lubridate::as.duration(interval)),
							 msg = "invalid interval specification")
	# precondition
	assertthat::assert_that(assertthat::is.dir(data_dir),
							 msg = "invalid data directory")

	# define the bands
	cloud_band   <- .sits_config_cloud_band(cube$satellite)
	bands_no_cloud <- bands[bands != cloud_band]

	# create the new timeline
	timeline     <-  sits_timeline(cube)
	new_indexes  <- .sits_timeline_indexes_interval(timeline = timeline,
												    interval = interval)
	new_timeline <- timeline[new_indexes]
	start_date   <- new_timeline[1]
	end_date     <- new_timeline[length(new_timeline)]

	# create the output bricks
	n_objs <- length(bands_no_cloud)
	bricks <- vector("list", n_objs)

	bricks <- purrr::map(bricks, function(brick){
		brick <- terra::rast(nrows  = cube$nrows,
		                     ncols  = cube$ncols,
		                     nlyrs  = n_objs,
		                     xmin   = cube$xmin,
		                     xmax   = cube$xmax,
		                     ymin   = cube$ymin,
		                     ymax   = cube$ymax,
		                     crs    = cube$crs)

		return(brick)
	})

	# initiate writing
	bricks <- purrr::map2(bricks, c(1:n_objs), function(brick, i){
		file_name <- paste0(data_dir, "/", bands_no_cloud[i], "_", start_date,"_", end_date, ".tif")
		brick <- suppressWarnings(raster::writeStart(brick,
													 filename  = file_name,
													 format    = "GTiff",
													 datatype  = "INT2U",
													 overwrite = TRUE))

		return(brick)
	})
	return(bricks)
}
