
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
            tryCatch({
                URL  <- .sits_server(s)
                # obtains information about the available data cubes in the WTSS service
                wtss.obj  <- wtss::WTSS(URL)
            }, error = function(e){
                msg <- paste0("WTSS service not available at URL ", URL)
                .sits_log_error(msg)
                message(msg)
            })

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
                cat(paste0("      Bands: \"", paste(bands, collapse = "\", \""), "\"\n"))
            }
        } else if (s == "SATVEG") {
            cat(paste0("Service: \"", s,"\"\n"))
            q <- paste0(s,"_cubes")
            cubes <- sits.env$config[[q]]

            for (cb in cubes) {
                cat(paste0("   Cube: \"", cb, "\"\n"))
                q1 <- paste0(s, "_bands")
                bands <- sits.env$config[[q1]][[cb]]
                cat(paste0("      Bands: \"", paste(bands, collapse = "\", \""), "\"\n"))
            }
        } else if (s == "EOCUBES") {
            cat(paste0("Service: \"", s,"\"\n"))

            remote.obj   <- EOCubes::remote(name = "eocubes")
            cubes <- names(EOCubes::list_cubes(remote.obj))

            for (cb in cubes) {
                cat(paste0("   Cube: \"", cb, "\"\n"))
                cub.obj <- EOCubes::cube(cb, remote.obj)
                cat(paste0("      Bands: \"", paste(EOCubes::cube_bands(cub.obj), collapse = "\", \""), "\"\n"))
            }
        }
    }
}
