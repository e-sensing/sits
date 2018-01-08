#' @title Obtain timeSeries from WTSS server, based on a SHP file.
#' @name sits_fromSHP
#'
#' @description reads a shapefile and retrieves a SITS tibble
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#'
#' @param shp_file        string  - name of a SHP file which provides the boundaries of a region of interest
#' @param service         string - name of the time series service (options are "WTSS" or "SATVEG")
#' @param coverage        name of the coverage
#' @param bands           string vector - the names of the bands to be retrieved
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param satellite       (optional) - the same of the satellite (options - "terra", "aqua", "comb")
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series
#' @return table          a SITS tibble
#'
#' @examples
#' \donttest{
#' # define a shapefile and read from the points inside it from the WTSS service
#' shp_file <- system.file("extdata/shapefiles/anhanguera/anhanguera.shp", package = "sits")
#' munic.tb <- sits_fromSHP(shp_file)
#' }
#' @export
#'
sits_fromSHP <- function(shp_file,
                         service    = "WTSS",
                         start_date = NULL,
                         end_date   = NULL,
                         coverage   = "mod13q1_512",
                         bands      = NULL,
                         satellite  = "terra",
                         prefilter  = "1",
                         label      = "NoClass") {

    # load the configuration file
    if (purrr::is_null(sits.env$config))
        sits_config()

    # test parameters
    ensurer::ensure_that(file, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_fromSHP: please provide a valid SHP file")
    # Ensure that the service is available
    ensurer::ensure_that(service, (.) %in% sits.env$config$ts_services,
                         err_desc = "sits_formSHP: Invalid time series service")

    ensurer::ensure_that(satellite, (.) %in% sits.env$config$SATVEG_satellites,
                         err_desc = "sits_fromSHP: SATVEG service does not support this satellite")

    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # get the bounding box
    bbox <- sf::st_bbox(sf_shape)
    # create an empty sits tibble
    shape.tb <- sits_tibble()
    # recover the coverage information
    if (service == "WTSS") {
        coverage.tb <- sits_coverageWTSS(coverage, .show = FALSE)
        xres <- coverage.tb$xres
        yres <- coverage.tb$yres
    }
    if (service == "SATVEG") {
        if (satellite %in% c("terra","aqua","comb")) {
            xres <- sits.env$config$resolution$MODIS$xres
            yres <- sits.env$config$resolution$MODIS$yres
        }
    }

    # setup the sequence of latitudes and longitudes to be searched
    longitudes <- seq(from = bbox["xmin"], to = bbox["xmax"], by = xres)
    latitudes  <- seq(from = bbox["ymin"], to = bbox["ymax"], by = yres)

    longitudes %>%
        purrr::map(function(long){
            latitudes %>%
                purrr::map(function(lat){
                    ll <- sf::st_point(c(long, lat))
                    if (1 %in% as.logical(unlist(sf::st_within(ll, sf_shape)))) {
                            row <- sits_from_service(service, long, lat, start_date, end_date,
                                                     coverage, bands, satellite, prefilter, label)
                        shape.tb <<- dplyr::bind_rows(shape.tb, row)
                    }
                })
        })
    return(shape.tb)
}
