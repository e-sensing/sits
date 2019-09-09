#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name .sits_from_shp
#'
#' @description reads a shapefile and retrieves a sits tibble
#' containing time series from a data cube that are inside the SHP file.
#' The script uses the WTSS service, taking information about spatial and
#' temporal resolution from the WTSS configuration.
#'
#' @param shp_file        Name of a SHP file which provides the boundaries of a region of interest.
#' @param cube            Data cube metadata.
#' @param start_date      The start date of the period.
#' @param end_date        The end date of the period.
#' @param bands           A string vector with the names of the bands to be retrieved.
#' @param prefilter       A string related to data correction ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction).
#' @param label           A string with the label to attach to the time series.
#' @param epsg             An optional code for the planar projection to be used if the shapefile is in WGS84 (for reading data inside shapefiles)
#' @return A sits tibble.
.sits_from_shp <- function(shp_file, cube, start_date, end_date, bands,
                           prefilter, label, epsg) {
    # test parameters
    ensurer::ensure_that(shp_file, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_from_shp: please provide a valid SHP file")
    # Ensure that the service is available
    .sits_config_check(cube$service)

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # find out what is the projection of the shape file
    epsg_shp <- sf::st_crs(sf_shape)$epsg
    # if the shapefile is not in planar coordinates, convert it
    if (epsg_shp == 4326) {
        sf_shape <- sf::st_transform(sf_shape, crs = epsg)
    }
    else # if shapefile not in lat/long, use the shapefile EPSG
        epsg <- epsg_shp
    # get the bounding box
    bbox <- sf::st_bbox(sf_shape)
    # create an empty sits tibble
    shape.tb <- .sits_tibble()

    # If the resolution of the cube is expressed in latlong, convert it to planar coordinates
    res <- .sits_convert_resolution(cube)

    # setup the sequence of latitudes and longitudes to be searched
    xs <- seq(from = bbox["xmin"], to = bbox["xmax"], by = res["xres"])
    ys  <- seq(from = bbox["ymin"], to = bbox["ymax"], by = res["yres"])

    xys <- tidyr::crossing(xs, ys)
    names(xys) <- c("x", "y")
    rows.lst <-
        xys %>%
        purrr::pmap(function(x, y) {
            xy <- sf::st_point(c(x,y))
            if (1 %in% as.logical(unlist(sf::st_contains(sf_shape, xy)))) {
                ll <- .sits_proj_to_latlong(x, y, epsg)
                row <- .sits_from_service(cube, ll[,"X"], ll[,"Y"], start_date, end_date,
                                          bands, prefilter, label)
                return(row)
            }
        })
    shape.tb <- dplyr::bind_rows(rows.lst)
    return(shape.tb)
}
