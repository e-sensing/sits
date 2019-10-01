
#' @title Provides information about the time series services available
#' @name sits_services
#'
#' @description Uses the configuration file to print information about the services, products and coverages.
#' WTSS - The Web Time Series Service is a lightweight web service the allow remote access to satellite
#'  image time series and provides three operations:
#'
#'  1. listCoverages: this operation allows clients to retrieve the capabilities provided
#'     by any server that implements WTSS. It returns a list of data cubes
#'     available in a server instance.
#'
#'  2. describeCoverage: this operation returns the metadata for a given data cube
#'     identified by its name. It includes its range in the spatial and temporal dimensions.
#'
#'  3. timeSeries: this operation requests the time series of values of a data cube
#'    at a given location.
#'
#' EOCUBES - The EOCubes service is a lightweight link data package that allows remote access to satellite
#'  image data cubes. It has the following functions:
#'
#'  1. listCubes: this operation allows clients to retrieve the capabilities provided
#'     by any server that implements EOCUBES. It returns a list of data cubes
#'     available in a server instance.
#'
#'  2. describeCube: this operation returns the metadata for a given data cube
#'     identified by its name. It includes its range in the spatial and temporal dimensions.
#'
#'  3. GetDataCube: this operation creates a metadata description of a data cube.
#' @export
sits_services <- function() {
    services <- sits.env$config$services

    for (s in services) {

        if (s == "WTSS") {
            # find the URL of the WTSS service in the configuration file
            URL  <- .sits_config_server(s)
            # check if the service is running
            wtss_ok <- .sits_wtss_check(URL)
            # if service is running, describe it
            if (wtss_ok) {
                wtss.obj  <- wtss::WTSS(URL)
                cubes <- wtss::listCoverages(wtss.obj)

                cat(paste0("Service: \"", s,"\"\n"))
                for (c_name in cubes) {
                    cat(paste0("   Cube: \"", c_name, "\"\n"))

                    # describe the data cube
                    cb <- wtss::describeCoverage(wtss.obj, c_name)[[c_name]]

                    # retrieve information about the bands
                    band_info <- cb$attributes
                    attr <- as.data.frame(band_info)
                    bands <- as.vector(attr[,"name"])
                    cat(paste0("  Bands: \"", paste(bands, collapse = "\", \""), "\"\n"))
                }
            }
            else
                message(paste0("Default WTSS service not running - please check configuration file"))
        }
        else if (s == "SATVEG") {
            # check if the service is running
            satveg_ok <- .sits_satveg_check()
            # if service is running, describe it
            if (satveg_ok) {
                q <- "SATVEG-EMBRAPA_cubes"
                cubes <- sits.env$config[[q]]

                for (cb in cubes) {
                    cat(paste0("   Cube: \"", cb, "\"\n"))
                    q1 <- paste0(s, "_bands")
                    bands <- sits.env$config[[q1]][[cb]]
                    cat(paste0("      Bands: \"", paste(bands, collapse = "\", \""), "\"\n"))
                }
            }
            else
                message(paste0("Default SATVEG service not running - please check configuration file"))

        } else if (s == "EOCUBES") {
            # find the URL of the EOCUBES service in the configuration file
            URL  <- .sits_config_server(s)
            # check if the service is running
            eocubes_ok <- .sits_eocubes_check(URL)
            # if service is running, describe it
            if (eocubes_ok) {
                cat(paste0("Service: \"", s,"\"\n"))

                remote.obj   <- EOCubes::remote(name = "eocubes")
                cubes <- names(EOCubes::list_cubes(remote.obj))

                for (cb in cubes) {
                    cat(paste0("   Cube: \"", cb, "\"\n"))
                    cub.obj <- EOCubes::cube(cb, remote.obj)
                    cat(paste0("      Bands: \"", paste(EOCubes::cube_bands(cub.obj), collapse = "\", \""), "\"\n"))
                }
            }
            else
                message(paste0("Default EOCUBES service not running - please check configuration file"))
        }
    }
}






