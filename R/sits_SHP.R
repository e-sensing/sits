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
#' @param product         string - the name of the product (e.g., "MOD13Q1")
#' @param coverage        string - name of the coverage
#' @param bands           string vector - the names of the bands to be retrieved
#' @param start_date      date - the start of the period
#' @param end_date        date - the end of the period
#' @param prefilter       string ("0" - none, "1" - no data correction, "2" - cloud correction, "3" - no data and cloud correction)
#' @param label           string - the label to attach to the time series
#' @return table          a SITS tibble
#'
#' @examples
#' \donttest{
#' # define a shapefile and read from the points inside it from the WTSS service
#' shp_file <- system.file("extdata/shapefiles/madre_de_deus/madre_de_deus.shp", package = "sits")
#' munic.tb <- sits_fromSHP(shp_file)
#' }
#' @export
#'
sits_fromSHP <- function(shp_file,
                         service    = "WTSS",
                         product    = "MOD13Q1",
                         coverage   = "mod13q1_512",
                         start_date = NULL,
                         end_date   = NULL,
                         bands      = NULL,
                         prefilter  = "1",
                         label      = "NoClass") {


    # test parameters
    ensurer::ensure_that(shp_file, !purrr::is_null(.) && tolower(tools::file_ext(.)) == "shp",
                         err_desc = "sits_fromSHP: please provide a valid SHP file")
    # Ensure that the service is available
    .sits_check_service(service)


    # recover the coverage resolution
    resolution <- .sits_get_resolution(product)
    # read the shapefile
    sf_shape <- sf::read_sf(shp_file)
    # get the bounding box
    bbox <- sf::st_bbox(sf_shape)
    # create an empty sits tibble
    shape.tb <- sits_tibble()

    # setup the sequence of latitudes and longitudes to be searched
    longitudes <- seq(from = bbox["xmin"], to = bbox["xmax"], by = resolution["xres"])
    latitudes  <- seq(from = bbox["ymin"], to = bbox["ymax"], by = resolution["yres"])

    longitudes %>%
        purrr::map(function(long){
            latitudes %>%
                purrr::map(function(lat){
                    ll <- sf::st_point(c(long, lat))
                    if (1 %in% as.logical(unlist(sf::st_within(ll, sf_shape)))) {
                            row <- .sits_from_service(service, product, coverage, long, lat, start_date, end_date,
                                                      bands, prefilter, label)
                        shape.tb <<- dplyr::bind_rows(shape.tb, row)
                    }
                })
        })
    return(shape.tb)
}
