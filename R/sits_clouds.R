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
#' @examples
#' # define a data cube of CBERS-4 AWFI data
#' data_dir <- system.file("extdata/raster/cbers", package = "sits")
#'
#' cbers_022024 <- sits_cube(type = "RASTER",
#'                           name = "cbers_022024",
#'                           satellite = "CBERS-4",
#'                           sensor = "AWFI",
#'                           resolution = "64m",
#'                           data_dir = data_dir,
#'                           parse_info = c("X1", "X2","band", "date"))
#'
#' cbers_022024_no_clds <- sits_cloud_remove(cube = cbers_022024,
#'                                           data_dir = tempdir(),
#'                                           name = "cbers_022024_no_cld")
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
	nrows <- as.numeric(cube$nrows)
	ncols <- as.numeric(cube$ncols)

	# single instance depends on the number of bands
	single_data_size <- nrows*ncols*nbytes*n_bands

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

#' @title Create the output of the cloud estimation procedure
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
.sits_clouds_interpolate <- function(cube,
                                     data_dir,
                                     blocks,
                                     impute_fn,
                                     multicores) {

    # get initial time for classification
    start_time <- lubridate::now()
    message(sprintf("Starting cloud conversion at %s", start_time))

	# define the bands
	cloud_band   <- .sits_config_cloud_band(cube)
	bands        <- sits_bands(cube)
	# ensure that the cloud band is available
	assertthat::assert_that(cloud_band %in% sits_bands(cube),
	                        msg = ".sits_clouds_interpolate: no cloud band")
	# define the bands that are not associate to clouds
	bands_no_cloud <- bands[bands != cloud_band]

	# get the file information from the cube
	file_info <- cube$file_info[[1]]

    # process the bands
	file.lst <- purrr::map(bands_no_cloud, function(bnd){
	    message(paste0("Removing clouds from band ", bnd))
	    start_task_time <- lubridate::now()

	    # find out the information about the band
	    info_band <- dplyr::filter(file_info, band == bnd)
	    # what is the number of layers?
	    num_layers <- nrow(info_band)

	    # what are the start and end date?
	    start_date <- info_band[1,]$date
	    end_date   <- info_band[num_layers,]$date

	    # what are the files associated to the band?
	    bnd_files <- dplyr::filter(file_info, band == bnd)$path

	    # read the blocks
	    values.lst <- purrr::map(c(1:blocks$n), function(b) {
	        # measure performance
	        start_block_time <- lubridate::now()
	        # define the extent
	        extent <- c(blocks$row[b], blocks$nrows[b],
	                    blocks$col, blocks$ncols)
	        names(extent) <- (c("row", "nrows", "col", "ncols"))

	        # preprocess the input data
	        values.mx <- .sits_raster_preprocess_data(cube       = cube,
	                                                  band_cube  = bnd,
	                                                  extent     = extent,
	                                                  impute_fn  = impute_fn,
	                                                  multicores = multicores)


	        task <- paste0("process block ", b, " of band ", bnd)
	        .sits_processing_estimate_task_time(task, start_block_time)

	        return(values.mx)
	    })

	    # join the values to make up the output band
	    # create a data.table joining the values
	    values_cld_free_DT <- data.table::as.data.table(do.call(rbind,values.lst))

	    # define the output filename
	    filename <- paste0(data_dir, "/",
	                       cube$satellite, "_",
	                       cube$sensor, "_",
	                       start_date,"_", end_date, "_",
	                       bnd, "_CLD_REM", ".tif")

	    # write the probabilities to a raster file
	    cube_class <- .sits_raster_api_write(cube       = cube,
	                                         num_layers = num_layers,
	                                         values     = values_cld_free_DT,
	                                         filename   = filename,
	                                         datatype   = "INT2U")

	    task <- paste0("Removed clouds from band ", bnd)
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
#' @param cld_band_name indication of the cloud band to be produced
#' @param data_dir      directory where cloud band will be written
#' @param t1            controls the difference btw visible and infrared bands
#' @param t2            controls the brightness properties of cloud
#' @param t3            controls the dark property of cloud shadows.
#' @param t4            remove influence of water in the cloud shadow detection
#' @param t5            size of window to search for clouds near shadows
#' @param t6            size of window of median filter to remove outliers
#' @param memsize       size of memory
#' @param multicores    number of cores
#'
#' @description This algorithm is an implementation of the paper by Zhai et al,
#' "Cloud/shadow detection based on spectral indices for multispectral
#'  optical remote sensing imagery", ISPRS Journal of Photogrammetry
#'  and Remote Sensing, volume 144, October 2018, Pages 235-253.
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
sits_cloud_cbers <- function(cube,
                             cld_band_name = "CMASK",
                             data_dir = NULL,
                             t1 = 1, t2 = 0.11, t3 = 0.50,
                             t4 = 0.75, t5 = 40, t6 = 5,
                             memsize = 8, multicores = 2){
    # preconditions
    assertthat::assert_that(cube$satellite == "CBERS-4",
                      msg = "sits_cloud_cbers works only with CBERS-4 data")
    assertthat::assert_that(cube$sensor == "AWFI",
                      msg = "sits_cloud_cbers works only with AWFI data")
    assertthat::assert_that(all(c("B13", "B14", "B15", "B16") %in%
                                    sits_bands(cube)),
                    msg = "sits_cloud_cbers requires bands 13 to 16")

    assertthat::assert_that(make.names(cld_band_name) == cld_band_name,
                        msg = "sits_cloud_cbers: invalid cloud band name")


    # estimate the blocks to be read
    blocks <- .sits_clouds_blocks_estimate(cube = cube,
                                           n_bands = 8,
                                           n_instances = 1,
                                           memsize = memsize)

    # get the file info
    file_info <- cube$file_info[[1]]

    # get the timeline
    timeline <- sits_timeline(cube)

    # iterate through the timeline
    cld_files.lst <- purrr::map(timeline, function(t){
        # measure performance
        start_interval_time <- lubridate::now()

        # read the blocks
        values.lst <- purrr::map(c(1:blocks$n), function(b) {
            # define the extent
            extent <- c(blocks$row[b], blocks$nrows[b],
                        blocks$col, blocks$ncols)
            names(extent) <- c("row", "nrows", "col", "ncols")

            values <- .sits_clouds_thres_estimate(cube = cube,
                                                  extent = extent,
                                                  ref_date = t)
        })
        # estimate the values based on the list
        param_values <- .sits_clouds_values_combine(values.lst)


        # read the blocks and estimate clouds
        clouds.lst <- purrr::map(c(1:blocks$n), function(b) {

            # define the extent
            extent <- c(blocks$row[b], blocks$nrows[b],
                        blocks$col, blocks$ncols)
            names(extent) <- c("row", "nrows", "col", "ncols")

            clouds_ext.DT <- .sits_clouds_shds_estimate(cube = cube,
                                                        extent = extent,
                                                        ref_date = t,
                                                        values = param_values,
                                                        t1 = t1, t2 = t2, t3 = t3,
                                                        t4 = t4, t5 = t5, t6 = t6,
                                                        multicores = multicores)
        })
        clouds_DT <- do.call(rbind,clouds.lst)

        # name of the cloud band file
        ref_file <- dplyr::filter(file_info, band == "B13" & date == t)$path
        if (grepl("B13", ref_file))
            cld_band_file <- stringr::str_replace(ref_file, "B13", cld_band_name)
        else if (grepl("BAND13", ref_file))
            cld_band_file <- stringr::str_replace(ref_file, "BAND13", cld_band_name)
        else
            stop("CBERS band name should be either B13 or BAND13")

        if (!purrr::is_null(data_dir))
            cld_band_file <- paste0(data_dir, "/", basename(cld_band_file))

        # write the probabilities to a raster file
        cube_class <- .sits_raster_api_write(cube       = cube,
                                             num_layers = 1,
                                             values     = clouds_DT,
                                             filename   = cld_band_file,
                                             datatype   = "INT1U")


        task <- paste0("process cld_band for time ", t)
        .sits_processing_estimate_task_time(task, start_interval_time)

        return(cld_band_file)
    })
    # add the information to the file info

    rows.lst <- purrr::map2(cld_files.lst, timeline, function(f, t){
        row <- tibble::tibble(date = lubridate::as_date(t),
                              band = cld_band_name,
                              path = f)
        return(row)
    })
    file_info <- dplyr::bind_rows(file_info, rows.lst)

    cube$file_info[[1]] <- file_info

    return(cube)
}

#' @title Retrieve the values associated to cloud detection for an extent
#'
#' @name  .sits_clouds_thres_estimate
#' @keywords internal
#'
#' @param  cube           Data cube
#' @param  extent         Image extent to be read.
#' @param  ref_date       Reference date to be processed
#'
#' @return                Vector of values for cloud detection
#'
.sits_clouds_thres_estimate <- function(cube,
                                        extent,
                                        ref_date) {

    # file info
    file_info <- cube$file_info[[1]]

    # get the minimum, maximum and missing values
    minimum_value <- cube$minimum_values[[1]][1]
    maximum_value <- cube$maximum_values[[1]][1]
    missing_value <- cube$missing_values[[1]][1]

    # store the raster objects in a list
    bands <- c("B13", "B14", "B15", "B16")
    b_files.lst <- purrr::map(bands, function(b){
        b_files <- dplyr::filter(file_info, band == b & date == as.Date(ref_date))$path
    })

    # read the values
    bands.lst <- purrr::map(b_files.lst, function(b_fil){
        values <- .sits_raster_api_read_extent(b_fil, extent)

        # correct for minimum, maximum, and missing values
        values[values < minimum_value] <- NA
        values[values > maximum_value] <- NA
        values[values == missing_value] <- NA
        return(values)
        })
    names(bands.lst) <- bands

    # obtain the values for the algorithm
    params <- cbers4_cld_values(bands.lst$"B13",
                                bands.lst$"B14",
                                bands.lst$"B15",
                                bands.lst$"B16")

    names(params) <- c("m1", "m2", "n_valid_mean",
                       "mean_b16", "min_b16", "n_valid_b16",
                       "mean_b13", "min_b13", "n_valid_b13")

    return(params)
}
#' @title Estimate values associated to cloud detection for a whole CBERS image
#'
#' @name  .sits_clouds_values_combine
#' @keywords internal
#'
#' @param  values.lst         List of values estimated for each extent
#' @return                    Combined vector of estimated values
#'
.sits_clouds_values_combine <- function(values.lst){
    assertthat::assert_that(length(values.lst) > 0,
                            msg = "invalid set of values for cloud estimate")

    len <- length(values.lst)
    values <- vector(mode = "integer", length = 9)
    names(values) = names(values.lst[[1]])
    for (i in 1:len) {
        val_ls <- values.lst[[i]]

        # values for mean band
        r1 <- values["n_valid_mean"]/(values["n_valid_mean"] + val_ls["n_valid_mean"])
        r2 <- val_ls["n_valid_mean"]/(values["n_valid_mean"] + val_ls["n_valid_mean"])

        values["m1"] <- ceiling(values["m1"]*r1 + val_ls["m1"]*r2)
        values["m2"] <- ceiling(values["m2"]*r1 + val_ls["m2"]*r2)
        values["n_valid_mean"] <- values["n_valid_mean"] + val_ls["n_valid_mean"]

        # values for band 16
        r3 <- values["n_valid_b16"]/(values["n_valid_b16"] + val_ls["n_valid_b16"])
        r4 <- val_ls["n_valid_b16"]/(values["n_valid_b16"] + val_ls["n_valid_b16"])

        values["mean_b16"] <- ceiling(values["mean_b16"]*r3 + val_ls["mean_b16"]*r4)
        values["min_b16"] <- ceiling(values["min_b16"]*r3 + val_ls["min_b16"]*r4)
        values["n_valid_b16"] <- values["n_valid_b16"] + val_ls["n_valid_b16"]

        # values for band 13
        r5 <- values["n_valid_b13"]/(values["n_valid_b13"] + val_ls["n_valid_b13"])
        r6 <- val_ls["n_valid_b13"]/(values["n_valid_b13"] + val_ls["n_valid_b13"])

        values["mean_b13"] <- ceiling(values["mean_b13"]*r5 + val_ls["mean_b13"]*r6)
        values["min_b13"] <- ceiling(values["min_b13"]*r5 + val_ls["min_b13"]*r6)
        values["n_valid_b13"] <- values["n_valid_b13"] + val_ls["n_valid_b13"]
    }
    return(values)
}

#' @title Estimate clouds and shadows for a CBERS AWFI image
#'
#' @name  .sits_clouds_shds_estimate
#' @keywords internal
#'
#' @param  cube           Data cube
#' @param  extent         Image extent to be read
#' @param  ref_date       Reference date for estimation
#' @param  values         Vector of values for cloud detection
#' @param  t1             controls the difference btw visible and infrared bands
#' @param  t2             controls the brightness properties of cloud
#' @param  t3             controls the dark property of cloud shadows.
#' @param  t4             remove influence of water in the cloud shadow detection
#' @param  t5             size of window to search for clouds near shadows
#' @param  t6             size of window of median filter to remove outliers
#' @param  multicores     number of cores
#'
#' @return Matrix of cloud and shadow values
#'

.sits_clouds_shds_estimate <- function(cube,
                                       extent,
                                       ref_date,
                                       values,
                                       t1, t2, t3, t4, t5, t6,
                                       multicores){


    # file info
    file_info <- cube$file_info[[1]]

    # get the minimum, maximum and missing values
    minimum_value <- cube$minimum_values[[1]][1]
    maximum_value <- cube$maximum_values[[1]][1]
    missing_value <- cube$missing_values[[1]][1]

    # store the raster objects in a list
    bands <- c("B13", "B14", "B15", "B16")
    b_files.lst <- purrr::map(bands, function(b){
        b_files <- dplyr::filter(file_info, band == b & date == as.Date(ref_date))$path
    })


    # read the values
    bands.lst <- purrr::map(b_files.lst, function(b_files){
        values <- .sits_raster_api_read_extent(b_files, extent)
        # correct for minimum, maximum, and missing values
        values[values < minimum_value] <- NA
        values[values > maximum_value] <- NA
        values[values == missing_value] <- NA
        return(values)
    })

    names(bands.lst) <- bands
    # transform data.table into matrix
    b13.mx <- bands.lst$"B13"
    b14.mx <- bands.lst$"B14"
    b15.mx <- bands.lst$"B15"
    b16.mx <- bands.lst$"B16"

    cld_detect_block <- function(b_b13.mx, b_b14.mx, b_b15.mx, b_b16.mx)
    {
        # interpolate NA
        block.mx <- cbers4_cld_detect(b_b13.mx, b_b14.mx, b_b15.mx, b_b16.mx,
                                      t1, t2, t3, t4, t5, t6, values)
    }
    # use multicores to speed up filtering
    if (multicores > 1) {
        b13.lst <- .sits_raster_split_data(b13.mx, multicores)
        b14.lst <- .sits_raster_split_data(b14.mx, multicores)
        b15.lst <- .sits_raster_split_data(b15.mx, multicores)
        b16.lst <- .sits_raster_split_data(b16.mx, multicores)
        clouds.lst  <- parallel::mcmapply(cld_detect_block, b13.lst,
                                          b14.lst, b15.lst, b16.lst,
                                          SIMPLIFY = FALSE,
                                          mc.cores = multicores)
        clouds.mx <- do.call(rbind, clouds.lst)
    }
    else
        clouds.mx <- cbers4_cld_detect(b13.mx, b14.mx, b15.mx, b16.mx)

    # transform into data table
    clouds.DT <- data.table::as.data.table(clouds.mx)

    return(clouds.DT)
}



