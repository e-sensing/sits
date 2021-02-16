#' @title Save the images based on an aggregation method.
#' @name .sits_gc_compose
#' @keywords internal
#'
#' @param cube        Data cube from where data is to be retrieved.
#' @param name        Name of the new data cube
#' @param img_col     A \code{object} 'image_collection' containing information
#'  about the images metadata.
#' @param cv_list     A \code{list} 'cube_view' with values from cube.
#' @param cloud_mask  A \code{logical} corresponds to the use of the cloud band
#'  for aggregation.
#' @param path_db     A \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#' @param path_images A \code{character} with the path where the aggregated
#'  images will be writed.
#' @param cloud_mask  A \code{logical} corresponds to the use of the cloud band
#'  for aggregation.
#' @param ...         Additional parameters that can be included. See
#'  '?gdalcubes::write_tif'.
#' @param version     A \code{character} with version of the output files.
#'
#' @return  A data cube with information used in its creation.
.sits_gc_compose <- function(cube, name, cv_list, img_col, path_db, path_images,
                             cloud_mask, ..., version = "v1") {

    # verifies the path to save the images
    assertthat::assert_that(dir.exists(path_images),
                            msg = paste("The provided dir does not exist.",
                                        "Please provided a valid path.")

    )

    # create a clone cube
    cube_gc <- .sits_cube_clone(
        cube = cube,
        name = name,
        ext = "",
        output_dir = path_images,
        version = version)

    # delete file info column
    cube_gc$file_info <- NULL

    # create file info column
    file_info <- tibble::tibble(res  = character(),
                                band = character(),
                                date = lubridate::as_date(""),
                                path = character())
    # add file info and path db columns
    cube_gc <- tibble::add_column(cube_gc, file_info = list(file_info),
                                  cube_database = path_db)

    for (i in seq_len(nrow(cube_gc))) {
        c_tile <- cube_gc[i,]

        for (band in c_tile$bands[[1]]) {
            # create a raster_cube object from gdalcubes
            cube_brick <-
                .sits_gc_brick(cube, img_col, cv_list[[i]], cloud_mask)

            message(paste("Writing images of band", band, "of tile",
                          c_tile$tile))

            # write the aggregated cubes
            path_write <- gdalcubes::write_tif(
                gdalcubes::select_bands(cube_brick, band),
                dir = path_images,
                prefix = paste("cube", c_tile$tile, band, "", sep = "_"),
                write_json_descr = TRUE, ...)

            # retrieving image date
            images_date <- .sits_gc_date(path_write)
            res <- dplyr::filter(cube[i,]$file_info[[1]], band == band)$res[[1]]

            # set file info values
            cube_gc[i,]$file_info[[1]] <- tibble::add_row(
                cube_gc[i,]$file_info[[1]],
                path = path_write,
                date = images_date,
                band = rep(band, length(path_write)),
                res  = rep(res, length(path_write)))

            # set timeline dates
            cube_gc[i,]$timeline[[i]][[1]] <- c(images_date)
        }
    }

    return(cube_gc)
}
#' @title Extracted date from aggregated cubes
#' @name .sits_gc_date
#' @keywords internal
#'
#' @param dir_images A \code{character}  corresponds to the path on which the
#'  images will be saved.
#'
#' @return a \code{character} vector with the dates extracted.
.sits_gc_date <- function(dir_images) {

    # get image name
    image_name <- basename(dir_images)

    date_files <-
        purrr::map_chr(strsplit(image_name, "_"), function(split_path) {
            tools::file_path_sans_ext(split_path[[4]])
        })

    # check type of date interval
    if (length(strsplit(date_files, "-")[[1]]) == 1)
        date_files <- lubridate::fast_strptime(date_files, "%Y")
    else if (length(strsplit(date_files, "-")[[1]]) == 2)
        date_files <- lubridate::fast_strptime(date_files, "%Y-%m")
    else
        date_files <- lubridate::fast_strptime(date_files, "%Y-%m-%d")

    # transform to date object
    date_files <- lubridate::as_date(date_files)

    return(date_files)
}
#' @title Create a raster_cube object
#' @name .sits_gc_brick
#' @keywords internal
#'
#' @param cube       Data cube from where data is to be retrieved.
#' @param img_col    A \code{object} 'image_collection' containing information
#'  about the images metadata.
#' @param cube_view  A \code{object} 'cube_view' with values from cube.
#' @param cloud_mask A \code{logical} corresponds to the use of the cloud band
#'  for aggregation.
#'
#' @return a \code{object} 'raster_cube' from gdalcubes containing information
#'  about the cube brick metadata.
.sits_gc_brick <- function(cube, img_col, cube_view, cloud_mask) {

    # defining the chunk size
    c_size <- c(t = 1, rows = floor(cube$nrows/4), cols = floor(cube$ncols/4))

    mask_band <- NULL
    if (cloud_mask)
        mask_band <- .sits_gc_cloud_mask(cube)

    # create a brick of raster_cube object
    cube_brick <- gdalcubes::raster_cube(img_col, cube_view, mask = mask_band,
                                         chunking = c_size)

    return(cube_brick)
}
#' @title Create an object image_mask with information about mask band
#' @name .sits_gc_cloud_mask
#' @keywords internal
#'
#' @param cube Data cube from where data is to be retrieved.
#'
#' @return A \code{object} 'image_mask' from gdalcubes containing information
#'  about the mask band.
.sits_gc_cloud_mask <- function(cube) {

    # checks if the cube has a cloud band
    assertthat::assert_that("SCL" %in% unique(cube$file_info[[1]]$band),
                            msg = paste("It was not possible to use the cloud",
                                        "mask, please include the cloud band",
                                        "in your cube. For S2LA_AWS, use",
                                        "'SCL' band.")
    )

    # create a image mask object
    mask_values <- gdalcubes::image_mask("SCL", values = c(3,8,9))

    return(mask_values)
}
#' @title Create an image_collection object
#' @name .sits_gc_database
#' @keywords internal
#'
#' @param cube      Data cube from where data is to be retrieved.
#' @param path_db   A \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#'
#' @return a \code{object} 'image_collection' containing information about the
#'  images metadata.
.sits_gc_database <- function(cube, path_db) {

    # error if a cube other than S2_L2A_AWS is provided
    assertthat::assert_that(!(cube[1,]$type != "S2_L2A_AWS"),
                            msg = paste("For now, only 'S2_L2A_AWS' cubes",
                                        "can be aggregated.")
    )

    # joining the bands of all tiles
    full_images <- dplyr::bind_rows(cube$file_info)

    # retrieving the s2_la_aws format
    format_col <- system.file("extdata/gdalcubes/s2la_aws.json",
                              package = "sits")

    message("Creating database of images...")
    # create image collection cube
    ic_cube <- gdalcubes::create_image_collection(files    = full_images$path,
                                                  format   = format_col,
                                                  out_file = path_db)
    return(ic_cube)
}
#' @title Create a cube_view object
#' @name .sits_gc_cube
#' @keywords internal
#'
#' @param path_db    A \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#' @param period     A \code{character} with the The period of time in which it
#'  is desired to apply in the cube, must be provided based on ISO8601, where 1
#'  number and a unit are provided, for example "P16D".
#' @param method     A \code{character} with the method that will be applied in
#'  the aggregation, the following are available: "min", "max", "mean",
#'  "median" or "first".
#' @param resampling A \code{character} with the method that will be applied
#'  in the resampling in mosaic operation. The following are available: "near",
#'  "bilinear", "bicubic" or others supported by gdalwarp
#'  (see https://gdal.org/programs/gdalwarp.html).
#'
#' @return a \code{list} with a cube_view objects.
.sits_gc_cube <- function(cube, period, method, resampling) {

    assertthat::assert_that(!purrr::is_null(period),
                            msg = paste("sits_gdalcubes: the parameter",
                                        "'period' must be provided."))

    assertthat::assert_that(!purrr::is_null(method),
                            msg = paste("sits_gdalcubes: the parameter",
                                        "'method' must be provided."))

    # create a list of cube view
    cv_list <- slider::slide(cube, function(c_tile) {
        gdalcubes::cube_view(
            extent = list(left   = c_tile$xmin,
                          right  = c_tile$xmax,
                          bottom = c_tile$ymin,
                          top    = c_tile$ymax,
                          t0 = format(min(c_tile$file_info[[1]]$date),
                                      "%Y-%m-%d"),
                          t1 = format(max(c_tile$file_info[[1]]$date),
                                      "%Y-%m-%d")),
            srs = c_tile$crs[[1]],
            dt  = period,
            nx  = c_tile$ncols[[1]],
            ny  = c_tile$nrows[[1]],
            aggregation = method,
            resampling  = resampling)})

    return(cv_list)
}
