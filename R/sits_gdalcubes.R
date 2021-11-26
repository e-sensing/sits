#' @title Save the images based on an aggregation method.
#' @name .gc_new_cube
#' @keywords internal
#'
#' @param tile        A data cube tile
#' @param img_col     A \code{object} 'image_collection' containing information
#'  about the images metadata.
#' @param cv          A \code{list} 'cube_view' with values from cube.
#' @param cloud_mask  A \code{logical} corresponds to the use of the cloud band
#'  for aggregation.
#' @param path_db     Database to be created by gdalcubes
#' @param output_dir  Directory where the aggregated images will be written.
#' @param cloud_mask  A \code{logical} corresponds to the use of the cloud band
#'  for aggregation.
#' @param multicores  A \code{numeric} with the number of cores will be used in
#'  the regularize. By default is used 1 core.
#' @param ...         Additional parameters that can be included. See
#'  '?gdalcubes::write_tif'.
#'
#' @return  A data cube tile with information used in its creation.
.gc_new_cube <- function(tile,
                         cv,
                         img_col,
                         path_db,
                         output_dir,
                         cloud_mask,
                         multicores, ...) {

    # set caller to show in errors
    .check_set_caller(".gc_new_cube")

    bbox <- .cube_tile_bbox(cube = tile)
    cube_gc <- .cube_create(
        source     = tile$source,
        collection = tile$collection,
        satellite  = tile$satellite,
        sensor     = tile$sensor,
        tile       = tile$tile,
        xmin       = bbox$xmin,
        xmax       = bbox$xmax,
        ymin       = bbox$ymin,
        ymax       = bbox$ymax,
        crs        = tile$crs,
        file_info  = NA
    )

    # update cube metadata
    cube_gc <- .gc_update_metadata(cube = cube_gc, cube_view = cv)

    # create file info column
    cube_gc$file_info[[1]] <- tibble::tibble(band = character(),
                                             date = lubridate::as_date(""),
                                             res  = numeric(),
                                             path = character())

    # create a list of creation options and metadata
    .get_gdalcubes_pack <- function(cube, band) {

        # returns the type that the file will write
        format_type <- .source_collection_gdalcubes_type(
            .cube_source(cube = tile),
            collection = .cube_collection(cube = tile)
        )

        return(
            list(type   = format_type,
                 nodata = .cube_band_missing_value(cube = cube, band = band),
                 scale  = 1,
                 offset = 0
            )
        )
    }

    .get_cube_chunks <- function(cv) {

        bbox <- c(lon_min = cv[["space"]][["left"]],
                  lon_max = cv[["space"]][["right"]],
                  lat_min = cv[["space"]][["bottom"]],
                  lat_max = cv[["space"]][["top"]])

        size_x_max <- max(bbox[c("lon_min", "lon_max")])
        size_x_min <- min(bbox[c("lon_min", "lon_max")])
        size_x <- size_x_max - size_x_min

        size_y_max <- max(bbox[c("lat_min", "lat_max")])
        size_y_min <- min(bbox[c("lat_min", "lat_max")])
        size_y <- size_y_max - size_y_min

        # a vector with time, x and y
        chunk_size <- .config_gdalcubes_chunk_size()

        chunks_x <- round(size_x / cv[["space"]][["dx"]]) / chunk_size[[2]]
        chunks_y <- round(size_y / cv[["space"]][["dy"]]) / chunk_size[[3]]

        # guaranteeing that it will return fewer blocks than calculated
        num_chunks <- (floor(chunks_x) * floor(chunks_y))

        return(max(1, num_chunks))
    }

    # setting threads to process
    # multicores number must be smaller than chunks
    gdalcubes::gdalcubes_options(
        threads = min(multicores, .get_cube_chunks(cv))
    )

    for (band in .cube_bands(tile, add_cloud = FALSE)) {

        # create a raster_cube object to each band the select below change
        # the object value
        cube_brick <- .gc_raster_cube(tile, img_col, cv, cloud_mask)

        message(paste("Writing images of band", band, "of tile", tile$tile))

        # write the aggregated cubes
        path_write <- gdalcubes::write_tif(
            gdalcubes::select_bands(cube_brick, band),
            dir = output_dir,
            prefix = paste("cube", tile$tile, band, "", sep = "_"),
            creation_options = list("COMPRESS" = "LZW", "BIGTIFF" = "YES"),
            pack = .get_gdalcubes_pack(tile, band),
            write_json_descr = TRUE, ...
        )

        # retrieving image date
        images_date <- .gc_get_date(path_write)

        # set file info values
        cube_gc$file_info[[1]] <- tibble::add_row(
            cube_gc$file_info[[1]],
            band = rep(band, length(path_write)),
            date = images_date,
            res  = rep(cv$space$dx, length(path_write)),
            path = path_write
        )
    }

    return(cube_gc)
}

#' @title Extracted date from aggregated cubes
#' @name .gc_get_date
#' @keywords internal
#'
#' @param dir_images A \code{character}  corresponds to the path on which the
#'  images will be saved.
#'
#' @return a \code{character} vector with the dates extracted.
.gc_get_date <- function(dir_images) {

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
#' @name .gc_raster_cube
#' @keywords internal
#'
#' @param cube       Data cube from where data is to be retrieved.
#' @param img_col    A \code{object} 'image_collection' containing information
#'  about the images metadata.
#' @param cv         A \code{object} 'cube_view' with values from cube.
#' @param cloud_mask A \code{logical} corresponds to the use of the cloud band
#'  for aggregation.
#'
#' @return a \code{object} 'raster_cube' from gdalcubes containing information
#'  about the cube brick metadata.
.gc_raster_cube <- function(cube, img_col, cv, cloud_mask) {

    mask_band <- NULL
    if (cloud_mask)
        mask_band <- .gc_cloud_mask(cube)

    # create a brick of raster_cube object
    cube_brick <- gdalcubes::raster_cube(
        image_collection = img_col,
        view = cv,
        mask = mask_band,
        chunking = .config_gdalcubes_chunk_size())

    return(cube_brick)
}

#' @title Update metadata from sits cube using gdalcubes metadata
#' @name .gc_update_metadata
#' @keywords internal
#'
#' @param cube       Data cube from where data is to be retrieved.
#' @param cv         A \code{object} 'cube_view' with values from cube.
#'  for aggregation.
#'
#' @return a \code{sits_cube} object with updated metadata.
.gc_update_metadata <- function(cube, cube_view) {

    # update bbox
    bbox_names <- c("xmin", "xmax", "ymin", "ymax")
    cube[, bbox_names] <- cube_view$space[c("left", "right", "bottom", "top")]

    return(cube)
}

#' @title Create an object image_mask with information about mask band
#' @name .gc_cloud_mask
#' @keywords internal
#'
#' @param tile Data cube tile from where data is to be retrieved.
#'
#' @return A \code{object} 'image_mask' from gdalcubes containing information
#'  about the mask band.
.gc_cloud_mask <- function(tile) {

    bands <- .cube_bands(tile)
    cloud_band <- .source_cloud()

    # checks if the cube has a cloud band
    .check_chr_within(
        x = cloud_band,
        within = unique(bands),
        discriminator = "any_of",
        msg = paste("It was not possible to use the cloud",
                    "mask, please include the cloud band in your cube")
    )

    # create a image mask object
    mask_values <- gdalcubes::image_mask(
        cloud_band,
        values = .source_cloud_interp_values(
            source = .cube_source(cube = tile),
            collection = .cube_collection(cube = tile)
        )
    )

    # is this a bit mask cloud?
    if (.source_cloud_bit_mask(
        source = .cube_source(cube = tile),
        collection = .cube_collection(cube = tile)))

        mask_values <- list(
            band = cloud_band,
            min = 1,
            max = 2^16,
            bits = mask_values$values,
            values = NULL,
            invert = FALSE
        )

    class(mask_values) <- "image_mask"

    return(mask_values)
}

#' @title Create an image_collection object
#' @name .gc_create_database
#' @keywords internal
#'
#' @param cube      Data cube from where data is to be retrieved.
#' @param path_db   A \code{character} with the path and name where the
#'  database will be create. E.g. "my/path/gdalcubes.db"
#'
#' @return a \code{object} 'image_collection' containing information about the
#'  images metadata.
.gc_create_database <- function(cube, path_db) {

    # TODO: put as parameter
    if (file.exists(path_db))
        unlink(path_db)

    create_gc_database <- function(cube) {

        file_info <- dplyr::select(cube, file_info, crs, collection, tile) %>%
            dplyr::mutate(`proj:epsg` = gsub("^EPSG:", "", crs)) %>%
            tidyr::unnest(cols = c(file_info)) %>%
            dplyr::transmute(xmin = left,
                             ymin = bottom,
                             xmax = right,
                             ymax = top,
                             href = path,
                             datetime = as.character(date),
                             href = href,
                             band = band,
                             `proj:epsg` = `proj:epsg`,
                             id = paste(collection, tile, as.character(date),
                                        sep = "_"))

        features <- dplyr::mutate(file_info, fid = id) %>%
            tidyr::nest(features = -fid)

        purrr::map(features$features, function(feature) {

            feature <- feature %>%
                tidyr::nest(assets = c(href, band)) %>%
                tidyr::nest(properties = c(datetime, `proj:epsg`)) %>%
                tidyr::nest(bbox = c(xmin, ymin, xmax, ymax))

            feature$assets <- purrr::map(feature$assets, function(asset) {
                tidyr::pivot_wider(asset, names_from = band, values_from = href) %>%
                    purrr::map(function(x) list(href = x, `eo:bands` = list(NULL)))
            })

            feature <- unlist(feature, recursive = FALSE)
            feature$properties <- c(feature$properties)
            feature$bbox <- unlist(feature$bbox)
            feature
        })
    }

    gdalcubes::stac_image_collection(s = create_gc_database(cube),
                                     out_file = path_db,
                                     url_fun = identity)
}

#' @title Internal function to handle with different file collection formats
#'  for each provider.
#' @name .gc_format_col
#' @keywords internal
#'
#' @description
#' Generic function with the goal that each source implements its own way of
#' localizing the collection format file.
#'
#' @param source     A \code{character} value referring to a valid data source.
#' @param collection A \code{character} value referring to a valid collection.
#' @param ...        Additional parameters.
#'
#' @return A \code{character} path with format collection.
.gc_format_col <- function(source, collection, ...) {

    # set caller to show in errors
    .check_set_caller("sits_cube")
    # try to find the gdalcubes configuration format for this collection
    gdal_config <- .config_get(key = c("sources", source, "collections",
                                       collection, "gdalcubes_format_col"),
                               default = NA)
    # if the format does not exist, report to the user
    .check_that(!(is.na(gdal_config)),
                msg = paste0("collection ", collection, " in source ", source,
                             "not supported yet\n",
                             "Please raise an issue in github"))
    # return the gdal format file path
    system.file(paste0("extdata/gdalcubes/", gdal_config), package = "sits")
}

#' @title Create a cube_view object
#' @name .gc_create_cube_view
#' @keywords internal
#'
#' @param tile       A data cube tile
#' @param period     A \code{character} with the The period of time in which it
#'  is desired to apply in the cube, must be provided based on ISO8601, where 1
#'  number and a unit are provided, for example "P16D".
#' @param res        A \code{numeric} with spatial resolution of the image that
#'  will be aggregated.
#' @param roi        A region of interest.
#' @param toi        A timeline of intersection
#' @param agg_method A \code{character} with the method that will be applied in
#'  the aggregation, the following are available: "min", "max", "mean",
#'  "median" or "first".
#' @param resampling A \code{character} with method to be used by
#'  \code{gdalcubes} for resampling in mosaic operation.
#'  Options: \code{near}, \code{bilinear}, \code{bicubic} or others supported by
#'  gdalwarp (see https://gdal.org/programs/gdalwarp.html).
#'  By default is bilinear.
#'
#' @return a \code{cube_view} object from gdalcubes.
.gc_create_cube_view <- function(tile,
                                 period,
                                 res,
                                 roi,
                                 toi,
                                 agg_method,
                                 resampling) {

    # set caller to show in errors
    .check_set_caller(".gc_create_cube_view")

    .check_that(
        x = nrow(tile) == 1,
        msg = "tile must have only one row."
    )

    .check_null(
        x = period,
        msg = "the parameter 'period' must be provided."
    )

    .check_null(
        x = agg_method,
        msg = "the parameter 'method' must be provided."
    )

    .check_num(
        x = res,
        msg = "the parameter 'res' is invalid.",
        allow_null = TRUE,
        len_max = 1
    )

    bbox_roi <- sits_bbox(tile)

    if (!is.null(roi))
        bbox_roi <- .sits_roi_bbox(roi, tile)

    # create a list of cube view
    cv <- gdalcubes::cube_view(
        extent = list(left   = bbox_roi[["xmin"]],
                      right  = bbox_roi[["xmax"]],
                      bottom = bbox_roi[["ymin"]],
                      top    = bbox_roi[["ymax"]],
                      t0 = format(toi[["max_min_date"]], "%Y-%m-%d"),
                      t1 = format(toi[["min_max_date"]], "%Y-%m-%d")),
        srs = tile[["crs"]][[1]],
        dt = period,
        dx = res,
        dy = res,
        aggregation = agg_method,
        resampling = resampling
    )

    return(cv)
}

.gc_stac_metric <- function(agg_method, cube) {

    UseMethod(".gc_stac_metric", agg_method)
}

.gc_stac_metric.stac <- function(agg_method, cube) {

}
