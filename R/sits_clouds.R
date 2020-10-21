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


	# total number of instances
	n_instances <- length(sits_timeline(cube))

	# estimate the blocks to be read
	blocks <- .sits_clouds_blocks_estimate(cube = cube,
	                                       n_bands = 3,
	                                       n_instances = n_instances,
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

	class(cube_new) <- c("raster_cube", class(cube_new))

	return(cube_new)

}
#' @title Estimate the number of blocks to correct for clouds
#' @name .sits_clouds_blocks_estimate
#' @keywords internal
#'
#' @param cube        input data cube
#' @param n_bands     number of bands in memory for processing
#' @param n_instances size of timeline
#' @param memsize     size of memory available
#'
#' @return        list with:
#'                n (number of blocks),
#'                row (vector of starting rows)
#'                nrow (vector with number of rows for each block)
#'                size (vector with size of each block)

.sits_clouds_blocks_estimate <- function(cube, n_bands, n_instances, memsize) {
	# number of bytes per pixel
	nbytes <-  8
	# estimated memory bloat
	bloat <- as.numeric(.sits_config_memory_bloat())

	# number of rows and cols
	nrows <- cube$nrows
	ncols <- cube$ncols

	# single instance depends on the number of bands
	single_data_size <- as.numeric(nrows)*as.numeric(ncols)*as.numeric(nbytes)*n_bands

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

	# find out the number of layers
	band_files <- dplyr::filter(cube$file_info[[1]], band == bands_no_cloud[1])
	nlyrs <- nrow(band_files)

	# define the cloud band
    obj_cld <- .sits_cube_terra_obj_band(cube, cloud_band)
    cld_index <- .sits_config_cloud_valid_values(cube)

    # process the bands
	file.lst <- purrr::map(bands_no_cloud, function(band){
	    message(paste0("Removing clouds from band ", band))
	    start_task_time <- lubridate::now()

	    # define the input band
	    obj_band <- .sits_cube_terra_obj_band(cube, band)

	    # define the output band
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

	    filename <- paste0(data_dir, "/",
	                       cube$satellite, "_",
	                       cube$sensor, "_",
	                       start_date,"_", end_date, "_",
	                       band, "_CLD_REM", ".tif")


	    terra::writeStart(brick,
	                      filename = filename,
	                      wopt     = list(filetype  = "GTiff",
	                                      datatype = "INT2U"),
	                      overwrite = TRUE)
	    # read the blocks
	    blocks.lst <- purrr::map(c(1:blocks$n), function(b) {
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

	        terra::writeValues(brick,
	                           values.mx,
	                           start = extent["row"],
	                           nrows  = extent["nrows"])

	        task <- paste0("process block ", b, " of band ", band)
	        .sits_processing_estimate_task_time(task, start_block_time)

	        rm(values.mx)
	        gc()
	        return(b)
	    })
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

#' @title Clean data cube to improve quality
#' @name  sits_cloud_cbers
#'
#' @param cube          input data cube
#' @param cld_band_name name of the cloud band to be produced
#' @param t1            controls the difference btw visible and infrared bands
#' @param t2            controls the brightness properties of cloud
#' @param t3            controls the dark property of cloud shadows.
#' @param t4            remove influence of water in the cloud shadow detection
#' @param t5            size of window to search for clouds when detecting shadows
#' @param t6            size of window of median filter to remove outliers
#' @param memsize       size of memory
#' @param multicores    number of cores
#'
#' @description This algorithm is an implementation of the paper by Zhai et al,
#' "Cloud/shadow detection based on spectral indices for multispectral
#'  optical remote sensing imagery", ISPRS Journal of Photogrammetry and Remote Sensing,
#'  volume 144, October 2018, Pages 235-253.
#'
#'  The authors recommend the following typical values for the parameters:
#'  SITS supports the following models:
#' \itemize{
#'  \item{t1: } - (1,...,10) - default = 1
#'  \item{t2: } - (0.1,..,0.5) - default = 0.11
#'  \item{t3: } - (0.25,..,0.75) - default = 0.50
#'  \item{t4: } - (0.5,..,0.90) - default = 0.75
#'  \item{t5: } - (30,..,90) - default = 40
#'  \item{t6: } - (3,..,11) - default = 5
#'  }
#'
#'  Please see Zhai et al.'s paper for more detail.
#'
#' @return           new data cube with cloud data
#' @export
#'
sits_cloud_cbers <- function(cube, cld_band_name = "CMASK",
                             t1 = 1, t2 = 0.11, t3 = 0.50,
                             t4 = 0.75, t5 = 40, t6 = 5,
                             memsize = 8, multicores = 2){
    # preconditions
    assertthat::assert_that(cube$satellite == "CBERS-4",
                            msg = "sits_cloud_cbers works only with CBERS-4 data");
    assertthat::assert_that(cube$sensor == "AWFI",
                            msg = "sits_cloud_cbers works only with AWFI data");
    assertthat::assert_that(all(c("B13", "B14", "B15", "B16") %in% sits_bands(cube)),
                            msg = "sits_cloud_cbers requires bands 13 to 16");


    # estimate the blocks to be read
    blocks <- .sits_clouds_blocks_estimate(cube = cube,
                                           n_bands = 8,
                                           n_instances = 1,
                                           memsize = memsize)

    # get the file info
    file_info <- cube$file_info[[1]]

    # get the timeline
    timeline <- sits_timeline(cube)

    # get the data dir
    data_dir <- dirname(file_info[1,]$path)

    # iterate through the timeline
    cld_files.lst <- purrr::map(timeline, function (t){
        # measure performance
        start_interval_time <- lubridate::now()

        band_files <- dplyr::filter(file_info, date == t)
        # retrive information about the bands
        b13 <- dplyr::filter(band_files, band == "B13")$path
        b14 <- dplyr::filter(band_files, band == "B14")$path
        b15 <- dplyr::filter(band_files, band == "B15")$path
        b16 <- dplyr::filter(band_files, band == "B16")$path

        # retrieve the objects associated to the bands
        t_b13 <- terra::rast(b13)
        t_b14 <- terra::rast(b14)
        t_b15 <- terra::rast(b15)
        t_b16 <- terra::rast(b16)

        # define the cld band
        cld_band <- terra::rast(nrows  = cube$nrows,
                                ncols  = cube$ncols,
                                nlyrs  = 1,
                                xmin   = cube$xmin,
                                xmax   = cube$xmax,
                                ymin   = cube$ymin,
                                ymax   = cube$ymax,
                                crs    = cube$crs)
        # define the file name
        vec <- t_b13@ptr$names %>%
            strsplit(split = "_") %>%
            unlist()

        vec[length(vec)] <- cld_band_name
        # compose the cloud file name
        cld_band_file <- vec %>%
            paste(sep = "_", collapse = "_") %>%
            paste0(".tif")

        # include the data directory
        cld_band_file <- paste0(data_dir, "/", cld_band_file)

        # create the output file
        terra::writeStart(cld_band,
                          filename = cld_band_file,
                          wopt     = list(filetype  = "GTiff",
                                          datatype = "INT1U"),
                          overwrite = TRUE)

        # read the blocks
        blocks.lst <- purrr::map(c(1:blocks$n), function(b) {
            # measure performance
            start_block_time <- lubridate::now()
            # define the extent
            extent <- c(blocks$row[b], blocks$nrows[b],
                        blocks$col, blocks$ncols)
            names(extent) <- (c("row", "nrows", "col", "ncols"))
            # read the input bands
            terra::readStart(t_b13)
            b13.mx <- matrix(terra::readValues(x      = t_b13,
                                               row    = extent["row"],
                                               nrows  = extent["nrows"],
                                               col    = extent["col"],
                                               ncols  = extent["ncols"]),
                             nrow = extent["nrows"],
                             ncol = extent["ncols"])
            terra::readStop(t_b13)

            # read the input bands
            terra::readStart(t_b14)
            b14.mx <- matrix(terra::readValues(x      = t_b14,
                                               row    = extent["row"],
                                               nrows  = extent["nrows"],
                                               col    = extent["col"],
                                               ncols  = extent["ncols"]),
                             nrow = extent["nrows"],
                             ncol = extent["ncols"])
            terra::readStop(t_b14)

            # read the input bands
            terra::readStart(t_b15)
            b15.mx <- matrix(terra::readValues(x      = t_b15,
                                               row    = extent["row"],
                                               nrows  = extent["nrows"],
                                               col    = extent["col"],
                                               ncols  = extent["ncols"]),
                             nrow = extent["nrows"],
                             ncol = extent["ncols"])
            terra::readStop(t_b15)

            # read the input bands
            terra::readStart(t_b16)
            b16.mx <- matrix(terra::readValues(x      = t_b16,
                                               row    = extent["row"],
                                               nrows  = extent["nrows"],
                                               col    = extent["col"],
                                               ncols  = extent["ncols"]),
                             nrow = extent["nrows"],
                             ncol = extent["ncols"])
            terra::readStop(t_b16)

            cld_detect_block <- function(b_b13.mx, b_b14.mx, b_b15.mx, b_b16.mx) {
                # interpolate NA
                block.mx <- cbers4_cld_detect(b_b13.mx, b_b14.mx,
                                              b_b15.mx, b_b16.mx,
                                              t1, t2, t3, t4, t5, t6)
            }
            # use multicores to speed up filtering
            if (multicores > 1) {
                b13.lst <- .sits_raster_split_data(b13.mx, multicores)
                b14.lst <- .sits_raster_split_data(b14.mx, multicores)
                b15.lst <- .sits_raster_split_data(b15.mx, multicores)
                b16.lst <- .sits_raster_split_data(b16.mx, multicores)
                clouds.lst  <- parallel::mcmapply(cld_detect_block, b13.lst,
                                                b14.lst, b15.lst, b16.lst,
                                                mc.cores = multicores)
                clouds.mx <- do.call(rbind, clouds.lst)
                rm(b13.lst)
                rm(b14.lst)
                rm(b15.lst)
                rm(b16.lst)
                rm(clouds.lst)
                gc()
            }
            else
                clouds.mx <- cbers4_cld_detect(b13.mx, b14.mx,
                                               b15.mx, b16.mx,
                                               t1, t2, t3, t4, t5, t6)
            rm(b13.mx)
            rm(b14.mx)
            rm(b15.mx)
            rm(b16.mx)
            gc()

            terra::writeValues(cld_band,
                               clouds.mx,
                               start = extent["row"],
                               nrows  = extent["nrows"])

            task <- paste0("process block ", b, " for time ", t)
            .sits_processing_estimate_task_time(task, start_block_time)

        }) # blocks

        # finish writing
        terra::writeStop(cld_band)

        task <- paste0("process cld_band for time ", t)
        .sits_processing_estimate_task_time(task, start_interval_time)

        return(cld_band_file)
    })
    # add the information to the file info

    rows.lst <- purrr::map2(cld_files.lst, timeline, function(f, t){
        row <- tibble::tibble(date = lubridate::as_date(t), band = cld_band_name,
                              path = f)
        return(row)
    })
    file_info <- dplyr::bind_rows(file_info, rows.lst)

    cube$file_info[[1]] <- file_info

    return(cube)
}
