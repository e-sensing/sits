#' @title Post-process a classified data raster with bayesian filter
#' @name  sits_bayes_postprocess
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_coverage}}),
#'              and a noise value, to do a bayesian smoothing process.
#'
#' @param  raster_class      output raster coverage
#' @param  noise             bayesian smoothing parameter
#' @param  file              file to save the post processed raster
#' @return raster_layers.tb  tibble with metadata about the output RasterLayer objects
#'
#' @export
sits_bayes_postprocess <- function(raster_class, noise, file) {

    # allocate matrix of each class
    smooth_values <- matrix(NA,
                            nrow = length(raster_class$r_objs[[2]][[1]][[1]]),
                            ncol = length(raster_class$labels[[2]]))

    # rename bands
    bands.vec <- gsub("(class)(.+)", "smooth\\2", raster_class$bands[[1]])

    # file name
    files.vec <- paste0(file, "_", bands.vec, ".tif")

    # for each classification interval
    smoothed_raster.lst <-
        seq_along(raster_class$files[[2]]) %>%
        purrr::map(function(t) {
            # for each class, compute the smooth estimator
            for (band in seq_along(raster_class$labels[[2]])) {

                # read from file using GDAL
                values <- matrix(as.matrix(rgdal::readGDAL(fname = raster_class$files[[2]][t],
                                                           band = band, silent = TRUE)@data),
                                 nrow = raster_class$nrows[[2]],
                                 byrow = TRUE)

                # avoid extrem values
                values[values < 1] <- 1
                values[values > 9999] <- 9999

                # apply bayes smooth estimator
                smooth_values[, band] <- smooth_estimator_class(values, noise)
            }

            # generate an output raster layer based on the input first layer
            smooth_raster <-
                raster::raster(raster_class$r_objs[[2]][[t]][[1]])

            # select the best class by choosing the maximum estimator
            # copy classes to raster
            smooth_raster[] <- apply(smooth_values, 1, which.max)

            # save raster output to file
            smooth_raster <-
                raster::writeRaster(smooth_raster,
                                    filename = files.vec[t],
                                    overwrite = TRUE)

            return(smooth_raster)
        })


    # populate coverage_class to be returned
    raster_layers.tb <-
        .sits_create_raster_coverage(raster.lst         = smoothed_raster.lst,
                                     service            = "RASTER",
                                     name               = raster_class$name[[1]],
                                     timeline.lst       = raster_class$timeline[[1]],
                                     bands.vec          = bands.vec,
                                     labels.vec         = raster_class$labels[[1]],
                                     scale_factors.vec  = raster_class$scale_factors[[1]],
                                     missing_values.vec = raster_class$missing_values[[1]],
                                     minimum_values.vec = raster_class$minimum_values[[1]],
                                     files.vec          = files.vec)

    # return
    return(raster_layers.tb)
}
