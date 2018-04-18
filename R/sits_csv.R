#' @title Export a SITS tibble metadata to the CSV format
#' @name sits_metadata_toCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts metadata from a SITS tibble to a CSV file. The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date", "end_date", "coverage", "label").
#'
#' @param  data.tb    a SITS time series
#' @param  file       the name of the exported CSV file
#' @return status     the status of the operation
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_metadata_toCSV (cerrado_2classes, file = "./cerrado_2classes.csv")
#' }
#' @export
sits_metadata_toCSV <- function(data.tb, file){


    csv_columns <- c("longitude", "latitude", "start_date", "end_date", "label")

    #select the parts of the tibble to be saved
    csv.tb <- dplyr::select(data.tb, csv_columns)

    # create a column with the id
    id.tb <- tibble::tibble(id = 1:NROW(csv.tb))

    # join the two tibbles
    csv.tb <- dplyr::bind_cols(id.tb, csv.tb)

    tryCatch({utils::write.csv(csv.tb, file, row.names = FALSE, quote = FALSE)},
             error = function(e){
                 msg <- paste0("CSV - unable to save data in file ", file)
                 .sits_log_error(msg)
                 message("WTSS - unable to retrieve point - see log file for details" )
                 return(invisible(FALSE))})

    # write the CSV file
    return(invisible(TRUE))
}

#' @title Export a SITS tibble data to the CSV format
#' @name sits_data_toCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts time series data from a SITS tibble to a CSV file. The CSV file will not contain the metadata,
#' but will have the actual time series, with a reference value. This function is useful to
#' export the data for external applications
#'
#' @param  data.tb    a tibble with time series data and metadata
#' @param  file       name of the exported CSV file
#' @return status     status of the operation
#' @examples
#' \donttest{
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' data(cerrado_2classes)
#' # export a time series to zoo
#' sits_data_toCSV (cerrado_2classes, file = "./cerrado_2classes.csv")
#' }
#' @export
sits_data_toCSV <- function(data.tb, file){

    .sits_test_tibble(data.tb)

    distances_DT <- sits_distances(data.tb, adj_val = 0.0)

    tryCatch({utils::write.csv(distances_DT, file, row.names = FALSE, quote = FALSE)},
             error = function(e){
                 msg <- paste0("CSV - unable to save data in file ", file)
                 .sits_log_error(msg)
                 message("WTSS - unable to retrieve point - see log file for details" )
                 return(invisible(FALSE))})

    # write the CSV file
    return(invisible(TRUE))
}
#' @title Export a shapefile with points to a CSV file for later processing
#' @name sits_shp_toCSV
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts points from a shapefile to a CSV file. The CSV file will not contain the actual time
#'              series. Its columns will be the same as those of a CSV file used to retrieve data from
#'              ground information ("latitude", "longitude", "start_date", "end_date", "coverage", "label").
#'
#' @param  shpfile    a POINT or POLYGON shapefile
#' @param  csvfile    the name of the exported CSV file
#' @param  label      it can be either the label associated to the samples (POINT) or the name of a field in the shapefile (POLYGON)
#' @param  timeline   the timeline of the data set
#' @param  start_date starting date for which the samples are valid
#' @param  end_date   end date for which the samples are valid
#' @param  interval   interval between two samples of the same place
#' @param  n_samples     A length-one numeric. The number of samples requested. If is_density = TRUE, nsamples is the number of samples per unit of area. The default is 500.
#' @param  min_area      A length-one numeric. The minimum area of a sampled polygon. The default is 100.
#' @param  min_dist      A length-one numeric. The minimum disatnces between samples. The default is 50.
#' @param  border_offset A length-one numeric. The minimum distance from the samples to the polygon's borders. The default is 50.
#' @param  is_density    A length-one logical. Is this a density sampling? The dafault is FALSE
#' @return status     the status of the operation
#'
#' @examples
#' \donttest{
#' # set the timeline
#' data("timeline_2000_2017")
#' # set the start and end dates
#' start_date <- lubridate::ymd("2002-08-29")
#' end_date   <- lubridate::ymd("2013-08-13")
#' # define the input shapefile
#' shpfile <- system.file ("extdata/shapefiles/cerrado_forested.shp", package = "sits")
#' # define the output csv file
#' csvfile <- paste0("./cerrado_forested.csv")
#' # define the label
#' label <- "Cerrado_Forested"
#' # read the points in the shapefile and produce a CSV file
#' sits_shp_toCSV(shpfile, csvfile, label, timeline_2000_2017, start_date, end_date)
#' }
#' @export
sits_shp_toCSV <- function(shpfile, csvfile, label, timeline, start_date,
                           end_date, interval = "12 month", n_samples = 500,
                           min_area = 100, min_dist = 50, border_offset = 50,
                           is_density = FALSE) {

    # test parameters
    ensurer::ensure_that(shpfile, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_shp_toCSV: please provide a valid SHP file")

    # read the shapefile
    sf_shape <- sf::read_sf(shpfile)

    # assume WGS84 if crs is NA
    if(is.na(sf::st_crs(sf_shape))){
        bbox <- sf::st_bbox(sf_shape)
        test_lon <- c(bbox$xmin, bbox$xmax) <= 180 && c(bbox$xmin, bbox$xmax) >= -180
        test_lat <- c(bbox$ymin, bbox$ymax) <= 90 && c(bbox$ymin, bbox$ymax) >= -90
        if(test_lon && test_lat){
            warning("Assuming WGS84 coordinate reference system.")
            sf::st_crs(sf_shape) <- 4326
        }
    }
    # find out what is the projection of the shape file
    crs1 <- sf::st_crs(sf_shape)
    # if the shapefile is not in EPSG:4326 and WGS84, transform shape into WGS84
    if (is.na(crs1$epsg) || crs1$epsg != 4326) {
        sf_shape <- sf::st_transform(sf_shape, crs = 4326)
    }

    # create a tibble to store the samples
    csv.tb <- tibble::tibble(
        id         = integer(),
        longitude  = double(),
        latitude   = double(),
        start_date = as.Date(character()),
        end_date   = as.Date(character()),
        label      = character())

    # limit the timeline btw start and end_date
    timeline <- timeline[timeline >= start_date & timeline <= end_date]

    # obtain pairs of (start, end) dates for each interval
    subset_dates.lst <- sits_match_timeline(timeline, start_date, end_date, interval)

    if(length(grep("POINT", attr(sf_shape$geometry, "class"), value =T)) > 0){
        # extract the lat/long coords from the shapefile
        coords <- do.call(rbind, sf::st_geometry(sf_shape)) %>%
            tibble::as_tibble() %>%
            stats::setNames(c("longitude","latitude"))
        id <- 0
        # generate the output tibble
        purrr::pmap(list(coords$longitude, coords$latitude), function(long, lat){
            rows.lst <- subset_dates.lst %>%
                purrr::map(function(date_pair) {
                    id <<- id + 1
                    tibble::tibble(
                        id         = id,
                        longitude  = long,
                        latitude   = lat,
                        start_date = date_pair[1],
                        end_date   = date_pair[2],
                        label      = label)
                })
            csv.tb <<- dplyr::bind_rows(csv.tb, rows.lst)
        })
    }else if(length(grep("POLYGON", attr(sf_shape$geometry, "class"), value =T)) > 0){
        sf_samples <- .sample_polygons(sf_shape = sf_shape, label = label,
                                       n_samples = n_samples, min_area = min_area,
                                       min_dist = min_dist, border_offset = border_offset,
                                       is_density = is_density)
        sf_samples <- sf::st_set_geometry(sf_samples, NULL)
        if(nrow(sf_samples) > 0){
            csv.tb <- tibble::tibble(
                id         = 1:nrow(sf_samples),
                longitude  = sf_samples$X,
                latitude   = sf_samples$Y,
                start_date = start_date,
                end_date   = end_date,
                label      = as.vector(unlist(sf_samples[label])))
        }
    }

    # write the CSV file
    tryCatch({utils::write.csv(csv.tb, csvfile, row.names = FALSE, quote = FALSE)},
             error = function(e){
                 message(paste0("sits_shp_toCSV - unable to save data in file ", csvfile))
                 return(invisible(FALSE))})
    return(invisible(TRUE))
}




#' @title Obtain random samples from polygons
#' @name .sample_polygons
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get random samples from a polygon sf object. By default, this
#' function returns approximately the same number of samples for each unique
#' label. If is_density is true
#'
#' @param sf_shape      A POLYGON sf object.
#' @param label         A length-one character. The name of a variable in sf_shape
#' @param n_samples     A length-one numeric. The number of samples requested. If is_density = TRUE, n_samples is the number of samples per unit of area.
#' @param min_area      A length-one numeric. The minimum area of a sampled polygon.
#' @param min_dist      A length-one numeric. The minimum disatnces between samples.
#' @param border_offset A length-one numeric. The minimum distance from the samples to the polygon's borders.
#' @param is_density    A length-one logical. Is this a density sampling? The dafault is FALSE
#' @return sf_samples   A point sf object with the structure of sf_shape, plues the X and Y coordinates. The number of rows is approximately n_samples.
.sample_polygons <- function(sf_shape, label, n_samples, min_area,
                             min_dist, border_offset,
                             is_density = FALSE){

    # filter out invalid geometries
    sf_shape <- sf_shape[sf::st_is_valid(sf_shape),]
    # buffer to ensure no samples near the polygons' borders
    if(border_offset != 0){
        suppressMessages(suppressWarnings(
            sf_shape <- sf::st_buffer(sf_shape, dist = base::sqrt(border_offset^2) * (-1))
        ))
    }
    # add temporal variables
    sf_shape["tmp_label"] <- sf::st_set_geometry(sf_shape[label], NULL)
    sf_shape["tmp_area"] <- sf::st_area(sf_shape)
    # filter out small areas and empty geometries
    units(min_area) <- units::as_units(units(sf_shape$tmp_area))
    sf_shape <- sf_shape[sf_shape$tmp_area > min_area,]
    sf_shape <- sf_shape[!sf::st_is_empty(sf_shape),]
    #
    if(nrow(sf_shape) == 0){return(sf_shape)}
    #
    if(is_density){
        # get samples
        n <- round(sum(units::drop_units(sf_shape$tmp_area)) * n_samples,
                   digits = 0)
        suppressMessages(
            samples_sfc <- sf::st_sample(sf_shape, size = n)
        )
        # cast to sf
        sf_samples <- data.frame(id = 1:length(samples_sfc))
        sf::st_geometry(sf_samples) <- samples_sfc
    }else{
        label_vec <- unique(sf_shape$tmp_label)
        samples_per_class <- round(n_samples / length(label_vec), digits = 0)
        # get samples
        samples_sf_ls <- lapply(label_vec, FUN = function(x, sf_obj, n){
            #sf_obj <- sf_obj %>% dplyr::filter(tmp_label == x)
            sf_obj <- sf_obj[sf_obj$tmp_label == x, ]
            suppressMessages(
                samples_sfc <- sf::st_sample(sf_obj, size = n)
            )
            sf_samples <- data.frame(id = seq(along.with = samples_sfc))
            sf::st_geometry(sf_samples) <- samples_sfc
            return(sf_samples)
        }, sf_obj = sf_shape, n = samples_per_class)
        # cast to sf
        sf_samples <- do.call(rbind, samples_sf_ls)
    }
    # check minimum distance between points
    if(min_dist > 0){
        dist_mt <- sf::st_distance(sf_samples)
        units(min_dist) <- units::as_units(units(dist_mt))
        dist_lg <- dist_mt > min_dist
        dist_lg[upper.tri(dist_lg, diag = TRUE)] <- NA
        selected_samples <- apply(dist_lg, MARGIN = 1, all, na.rm = TRUE)
        sf_samples <- sf_samples[selected_samples, ]
    }
    # add coords as attributes
    sf_samples <- cbind(sf_samples, sf::st_coordinates(sf_samples))
    # spatial-join to the original attributes
    suppressMessages(
        sf_samples <- sf::st_join(sf_samples, sf_shape)
    )
    # prepare
    sf_samples["tmp_label"] <- NULL
    sf_samples["tmp_area"] <- NULL
    return(sf_samples)
}



