#' @title Post-process a classified data raster with bayesian filter
#' @name  sits_bayes_postprocess
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a set of classified raster layers, whose metadata is
#'              described by tibble (created by \code{\link[sits]{sits_cube}}),
#'              and a noise value, to do a bayesian smoothing process.
#'
#' @param  raster_class      Classified image data cube.
#' @param  window            A matrix with the neighborhood window to compute bayesian smooth.
#'                           The central element index (i, j) is given by
#'                           i = floor(nrows(window)/2)+1 and j = floor(ncols(window)/2)+1.
#'                           Elements '0' are excluded from window.
#' @param  noise             Bayesian smoothing parameter.
#' @param  file              File to save the post processed raster.
#' @return A tibble with metadata about the output RasterLayer objects.
#' @export
sits_bayes_postprocess <- function(raster_class, window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE), noise = 10, file) {

    raster_smooth <-
        lapply(seq_along(raster_class$files), function(i) {

            # allocate matrix of each class
            smooth_values <- matrix(NA,
                                    nrow = length(raster_class$r_objs[[i]][[1]][[1]][[1]]),
                                    ncol = length(raster_class$labels[[i]][[1]]))

            # rename bands
            bands.vec <- raster_class$bands[[i]][[1]]

            # file name
            files.vec <- paste0(file, "_", gsub(".*/(.*)$" ,"\\1" , raster_class$files[[i]][[1]]))

            # create directory
            if (!dir.exists(dirname(files.vec[[1]])))
                dir.create(dirname(files.vec[[1]]), recursive = TRUE)

            # for each classification interval
            smoothed_raster.lst <-
                seq_along(files.vec) %>%
                purrr::map(function(t) {

                    # for each class, compute the smooth estimator
                    for (band in seq_along(raster_class$labels[[i]][[1]])) {

                        # read from file using GDAL
                        values <- matrix(as.matrix(rgdal::readGDAL(fname = raster_class$files[[i]][[2]][t],
                                                                   band = band, silent = TRUE)@data),
                                         nrow = raster_class$nrows[[1]][[2]],
                                         byrow = TRUE)

                        # avoid extrem values
                        values[values < 1] <- 1
                        values[values > 9999] <- 9999

                        # apply bayes smooth estimator
                        new_values <- smooth_estimator_class(data = values, window = window, noise = noise)
                        smooth_values[, band] <- new_values
                    }

                    # generate an output raster layer based on the input first layer
                    smooth_raster <-
                        raster::raster(raster_class$r_objs[[i]][[1]][[1]][[t]])

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

            # populate raster object to be returned
            raster_layers.tb <-
                .sits_create_raster_cube(raster.lst         = smoothed_raster.lst,
                                             service            = "RASTER",
                                             name               = raster_class$name[[i]][[1]],
                                             timeline.lst       = raster_class$timeline[[i]][[1]],
                                             bands.vec          = bands.vec,
                                             labels.vec         = raster_class$labels[[i]][[1]],
                                             scale_factors.vec  = raster_class$scale_factors[[i]][[1]],
                                             missing_values.vec = raster_class$missing_values[[i]][[1]],
                                             minimum_values.vec = raster_class$minimum_values[[i]][[1]],
                                             maximum_values.vec = raster_class$maximum_values[[i]][[1]],
                                             files.vec          = files.vec)

            raster_layers.tb <- dplyr::as_tibble(lapply(raster_layers.tb, list))

            return(raster_layers.tb)
        })

    raster_smooth <- dplyr::bind_rows(raster_smooth)

    return(raster_smooth)
}
