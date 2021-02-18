#' @title Informs the names of the bands
#' @name sits_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds the names of the bands of time series in a sits tibble
#'               or in a metadata cube
#'               For details see:
#' \itemize{
#'  \item{"time series": }{see \code{\link{sits_bands.sits}}}
#'  \item{"data cube": }{see \code{\link{sits_bands.cube}}}
#' }
#'
#' @param data      Valid sits tibble (time series or a cube)
#' @return A string vector with the names of the bands.
#'
#' @export
sits_bands <- function(data) {
    # get the meta-type (sits or cube)
    data <- .sits_config_data_meta_type(data)

    UseMethod("sits_bands", data)
}

#' @title Informs the names of the bands of a set of timeseries
#' @name sits_bands.sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds the names of the bands of time series in a sits tibble
#'
#' @param data      Valid sits tibble (time series)
#' @return A string vector with the names of the bands.
#'
#' @examples
#' # Retrieve the set of samples for Mato Grosso (provided by EMBRAPA)
#' # show the bands
#' sits_bands(samples_mt_6bands)
#' @export
sits_bands.sits <- function(data) {
    # backward compatibility
    data <- .sits_tibble_rename(data)

    bands <- sits_time_series(data) %>%
        colnames() %>%
        .[2:length(.)]

    return(bands)
}
#' @title Informs the names of the bands of a cube
#' @name sits_bands.cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds the names of the bands of a cube
#'
#' @param data      Valid sits tibble (time series)
#' @return A string vector with the names of the bands.
#'
#' @export
sits_bands.cube <- function(data) {
    return(data[1, ]$bands[[1]])
}

#' @title Informs the names of the bands of a set of timeseries
#' @name sits_bands.patterns
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds the names of the bands of time series in a sits tibble
#'
#' @param data      Valid sits tibble (time series)
#' @return A string vector with the names of the bands.
#'
#' @export
sits_bands.patterns <- function(data) {
    bands <- sits_bands.sits(data)

    return(bands)
}
#' @title Replaces the names of the bands
#' @name `sits_bands<-`
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Replace the names of the bands of time series in a sits tibble
#'               or in a metadata cube
#'               For details see:
#' \itemize{
#'  \item{"time series": }{see \code{\link{sits_bands<-.sits}}}
#'  \item{"data cube": }{see \code{\link{sits_bands<-.cube}}}
#' }
#'
#' @param x         Valid sits tibble (time series or a cube)
#' @param value     Vector of bands
#' @return An updated data set with the new bands.
#'
#' @export
`sits_bands<-` = function(x, value) {
    # get the meta-type (sits or cube)
    x <- .sits_config_data_meta_type(x)

    UseMethod("sits_bands<-", x)
}
#' @title Replaces the names of the bands of a set of timeseries
#' @name `sits_bands<-.sits`
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Replaces the names of the bands of time series in a sits tibble
#'
#' @param x         Valid sits tibble (time series or a cube)
#' @param value     Vector of bands
#' @return An updated data set with the new bands.
#'
#' @examples
#' # Replace the name of bands for the samples for Mato Grosso
#' sits_bands(samples_mt_4bands) <- c("ndvi", "evi", "nir", "mir")
#' @export
`sits_bands<-.sits` =  function(x, value) {
    # backward compatibility
    x <- .sits_tibble_rename(x)

    ts <- sits_time_series(x)
    assertthat::assert_that(ncol(ts) == length(value) + 1,
                          msg = "Invalid number of bands to be replaced")

    rows <- slider::slide(x, function(row){
        ts <- sits_time_series(row)
        names(ts) <- c("Index", value)
        row$time_series[[1]] <- ts
        return(row)
    })
    x <- dplyr::bind_rows(rows)

  return(x)
}

#' @title Replaces the names of the bands of a cube
#' @name `sits_bands<-.cube`
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Finds the names of the bands of a cube
#'
#' @param x         Valid sits tibble (time series or a cube)
#' @param value     Vector of bands
#' @return An updated data cube with the new bands.
#'
#' @export
`sits_bands<-.cube` = function(x, value) {
    rows <- slider::slide(x, function(row){
        old_bands <- row$bands[[1]]
        assertthat::assert_that(length(old_bands) == length(value),
                                msg = "replacement bands have wrong length")
        row$bands[[1]] <- value
        return(row)
    })
    x <- dplyr::bind_rows(rows)
    return(x)
}

#' @title Replaces the names of the bands of a set of patterns
#' @name `sits_bands<-.patterns`
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Replaces the names of the bands of time series in a sits tibble
#'
#' @param x         Valid sits tibble (time series or a cube)
#' @param value     Vector of bands
#' @return An updated data set with the new bands.
#'
#' @examples
#' # Replace the name of bands for the samples for Mato Grosso
#' sits_bands(samples_mt_4bands) <- c("ndvi", "evi", "nir", "mir")
#' @export
`sits_bands<-.patterns` =  function(x, value) {
    return(`sits_bands<-.sits`(x, value))
}
#' @title Replaces the names of the bands of a set of predictions
#' @name `sits_bands<-.predicted`
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Replaces the names of the bands of time series in a sits tibble
#'
#' @param x         Valid sits tibble (time series or a cube)
#' @param value     Vector of bands
#' @return An updated data set with the new bands.
#'
#' @examples
#' # Replace the name of bands for the samples for Mato Grosso
#' sits_bands(samples_mt_4bands) <- c("ndvi", "evi", "nir", "mir")
#' @export
`sits_bands<-.predicted` =  function(x, value) {
    return(`sits_bands<-.sits`(x, value))
}
#' @title Get the bounding box of the data
#' @name sits_bbox
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description  Obtain a vector of limits (either on lat/long for time series
#'               or in projection coordinates in the case of cubes)
#' \itemize{
#'  \item{"time series": }{see \code{\link{sits_bbox.sits}}}
#'  \item{"data cube": }{see \code{\link{sits_bbox.cube}}}
#' }
#'
#' @param data      Valid sits tibble (time series or a cube).
#' @return named vector with bounding box ("lon_min", "lon_max",
#'      "lat_min", "lat_max").
#'
#' @export
sits_bbox <- function(data) {
    # get the meta-type (sits or cube)
    data <- .sits_config_data_meta_type(data)

    UseMethod("sits_bbox", data)
}
#' @title Get the bounding box of a set of time series
#' @name sits_bbox.sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  Obtain a vector of limits in lat/long for time series

#' @param data      Valid sits tibble with a set of time series
#' @return named vector with bounding box ("lon_min", "lon_max",
#'      "lat_min", "lat_max")
#'
#' @export
sits_bbox.sits <- function(data) {
    # is the data a valid set of time series
    .sits_test_tibble(data)

    # get the max and min longitudes and latitudes
    lon_max <- max(data$longitude)
    lon_min <- min(data$longitude)
    lat_max <- max(data$latitude)
    lat_min <- min(data$latitude)
    # create and return the bounding box
    bbox <- c(lon_min, lon_max, lat_min, lat_max)
    names(bbox) <- c("lon_min", "lon_max", "lat_min", "lat_max")
    return(bbox)
}

#' @title Get the bounding box of a data cube
#' @name sits_bbox.cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  Obtain a vector of limits for the cube

#' @param data      Valid data cube
#' @return named vector with bounding box ("xmin", "xmax", "ymin", "ymax")
#'
#' @export
sits_bbox.cube <- function(data) {

    # create and return the bounding box
    if (nrow(data) == 1) {
          bbox <- c(data$xmin, data$xmax, data$ymin, data$ymax)
      } else {
          bbox <- c(min(data$xmin), max(data$xmax),
                    min(data$ymin), max(data$ymax)
                    )
      }

    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")
    return(bbox)
}


#' @title Merge two data sets (time series or cubes)
#' @name sits_merge
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param data1      sits tibble or cube to be merged.
#' @param data2      sits tibble or cube to be merged.
#' @description For details see:
#' \itemize{
#'  \item{"time series": }{see \code{\link{sits_merge.sits}}}
#'  \item{"data cube": }{see \code{\link{sits_merge.cube}}}
#' }
#' @return merged data sets
#' @export
sits_merge <- function(data1, data2) {
    # get the meta-type (sits or cube)
    data1 <- .sits_config_data_meta_type(data1)

    UseMethod("sits_merge", data1)
}
#' @title Merge two satellite image time series
#' @name sits_merge.sits
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function merges the time series of two sits tibbles.
#' To merge two series, we consider that they contain different
#' attributes but refer to the same data cube, and spatio-temporal location.
#' This function is useful to merge different bands of the same locations.
#' For example, one may want to put the raw and smoothed bands
#' for the same set of locations in the same tibble.
#'
#' @param data1      The first sits tibble to be merged.
#' @param data2      The second sits tibble to be merged.
#' @return A merged sits tibble with a nested set of time series.
#' @examples
#' #' # Retrieve a time series with values of NDVI
#' data(point_ndvi)
#' # Filter the point using the whittaker smoother
#' point_ws.tb <- sits_whittaker(point_ndvi, lambda = 3.0)
#' # Plot the two points to see the smoothing effect
#' plot(sits_merge(point_ndvi, point_ws.tb))
#' @export
sits_merge.sits <- function(data1, data2) {

    # precondition
    .sits_test_tibble(data1)
    .sits_test_tibble(data2)

    # backward compatibility
    data1 <- .sits_tibble_rename(data1)
    data2 <- .sits_tibble_rename(data2)

    # if some parameter is empty returns the another one
    assertthat::assert_that(nrow(data1) > 0 & nrow(data2) > 0,
        msg = "sits_merge: invalid input data"
    )

    # verify if data1.tb and data2.tb has the same number of rows
    assertthat::assert_that(NROW(data1) == NROW(data2),
        msg = "sits_merge: cannot merge tibbles of different sizes"
    )

    # are the names of the bands different?
    # if they are not
    bands1 <- sits_bands(data1)
    bands2 <- sits_bands(data2)
    if (any(bands1 %in% bands2) || any(bands2 %in% bands1)) {
        if (!(any(".new" %in% bands1)) & !(any(".new" %in% bands2))) {
              bands2 <- paste0(bands2, ".new")
          } else {
              bands2 <- paste0(bands2, ".nw")
          }
        sits_bands(data2) <- bands2
    }
    # prepare result
    result <- data1

    # merge time series
    result$time_series <- purrr::map2(
        data1$time_series,
        data2$time_series,
        function(ts1, ts2) {
            ts3 <- dplyr::bind_cols(ts1, dplyr::select(ts2, -Index))
            return(ts3)
        }
    )
    return(result)
}

#' @title Merge two data cubes
#' @name sits_merge.cube
#' @description Merge two simple data cubes (one tibble row) from the same satellite,
#' with the same sensor, resolution, bounding box, and timeline, and different bands.
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param data1      The first cube to be merged.
#' @param data2      The second cube to be merged.
#' @return A data cube.
#' @export
#'
sits_merge.cube <- function(data1, data2) {
    # preconditions
    assertthat::assert_that(nrow(data1) == 1 & nrow(data2) == 1,
        msg = "merge only works from simple cubes (one tibble row)"
    )
    assertthat::assert_that(data1$satellite == data2$satellite,
        msg = "cubes from different satellites"
    )
    assertthat::assert_that(data1$sensor == data2$sensor,
        msg = "cubes from different sensors"
    )
    assertthat::assert_that(all(sits_bands(data1) != sits_bands(data2)),
        msg = "merge cubes requires different bands in each cube"
    )
    assertthat::assert_that(all(sits_bbox(data1) == sits_bbox(data2)),
        msg = "merge cubes requires same bounding boxes"
    )
    assertthat::assert_that(data1$xres == data2$xres &
        data1$yres == data2$yres,
    msg = "merge cubes requires same resolution"
    )
    assertthat::assert_that(all(sits_timeline(data1) == sits_timeline(data2)),
        msg = "merge cubes requires same timeline"
    )

    # get the file information
    file_info_1 <- data1$file_info[[1]]
    file_info_2 <- data2$file_info[[1]]

    file_info_1 <- file_info_1 %>%
        dplyr::bind_rows(file_info_2) %>%
        dplyr::arrange(date)
    # merge the file info and the bands
    data1$file_info[[1]] <- file_info_1
    data1$bands[[1]] <- c(sits_bands(data1), sits_bands(data2))

    return(data1)
}

#' @title Filter bands on a data set (tibble or cube)
#' @name sits_select
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @param data         A sits tibble or data cube
#' @param bands        Character vector with the names of the bands
#'
#' @description For details see:
#' \itemize{
#'  \item{"time series": }{see \code{\link{sits_select.sits}}}
#'  \item{"data cube": }{see \code{\link{sits_select.cube}}}
#' }
#' @return data sets with only the bands selected
#' @export
sits_select <- function(data, bands) {
    # get the meta-type (sits or cube)
    data <- .sits_config_data_meta_type(data)

    UseMethod("sits_select", data)
}

#' @title Filter bands on a data set (tibble or cube)
#' @name sits_select.sits
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Returns a sits tibble with the selected bands.
#'
#' @param data         A sits tibble metadata and data on time series.
#' @param bands        Character vector with the names of the bands
#' @return A tibble in sits format with the selected bands.
#' @examples
#' # Retrieve a set of time series with 2 classes
#' data(cerrado_2classes)
#' # Print the original bands
#' sits_bands(cerrado_2classes)
#' # Select only the NDVI band
#' data <- sits_select(cerrado_2classes, bands = c("NDVI"))
#' # Print the labels of the resulting tibble
#' sits_bands(data)
#' @export
sits_select.sits <- function(data, bands) {
    # backward compatibility
    data <- .sits_tibble_rename(data)
    # bands names in SITS are uppercase
    bands <- toupper(bands)
    sits_bands(data) <- toupper(sits_bands(data))

    assertthat::assert_that(all(bands %in% sits_bands(data)),
        msg = paste0(
            "sits_select: missing bands: ",
            paste(bands[!bands %in% sits_bands(data)], collapse = ", ")
        )
    )

    # prepare result sits tibble
    result <- data

    # select the chosen bands for the time series
    result$time_series <- data$time_series %>%
        purrr::map(function(ts) ts[, c("Index", bands)])

    # return the result
    return(result)
}
#' @title Filter bands on a data cube
#' @name sits_select.cube
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Filter only the selected bands from a data cube.
#'
#' @param data         data cube
#' @param bands        vector with the names of the bands
#' @return A data cube with the selected bands.
#'
#' @export
#'
sits_select.cube <- function(data, bands) {
    assertthat::assert_that(bands %in% sits_bands(data),
        msg = "requested bands are not available in the data cube"
    )
    # assign the bands
    data$bands[[1]] <- bands
    # filter the file info
    db_info <- data$file_info[[1]]
    db_info <- dplyr::filter(db_info, band %in% bands)
    data$file_info[[1]] <- db_info

    return(data)
}

#' @title Filter bands on a data set (tibble or cube)
#' @name sits_select.patterns
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Returns a sits tibble with the selected bands.
#'
#' @param data         A sits tibble metadata and data on time series.
#' @param bands        Character vector with the names of the bands
#' @return A tibble in sits format with the selected bands.
#' @export
sits_select.patterns <- function(data, bands) {
    result <- sits_select.sits(data, bands)

    return(result)
}
