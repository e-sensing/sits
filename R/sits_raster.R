#' @title Relabel raster stack object
#' @name sits_raster
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description  This function creates a tibble containing the metadata for
#'               a set of spatio-temporal raster files. These files should be of the same size and
#'               projection. Each raster file should contain a data set, one layer
#'               per time step. Different bands are archived in different raster files.
#'
#'
#' @param files      Vector with the file paths of the raster files
#' @param timeline   Vector of dates with the timeline of the bands
#' @param bands       The bands contained in the file (in the same order as the files)
#'
#' @description This function creates a tibble to store the information
#' about a raster time series
#'
#' @export
sits_raster <- function (files, timeline, bands){

    ensurer::ensure_that (bands, length(.) == length(files), err_desc = "number of bands does not match number of files")


    raster.tb <- purrr::map2(files, bands,
                function (file, band){
                    # create a raster object associated to the file
                    raster.obj <- raster::raster (file)
                    # find out how many layers the object has
                    n_layers    <-  raster.obj@file@nbands
                    # check that there are as many layers as the length of the timeline
                    ensurer::ensure_that(n_layers, (.) == length(timeline),
                                         err_desc = "duration of timeline is not matched by number of layers in raster")

                    new_raster.tb <- tibble::tibble (

                        r_obj    = list(raster.obj),
                        ncols    = raster.obj@ncols,
                        nrows    = raster.obj@nrows,
                        band     = band,
                        timeline = list(timeline),
                        min      = raster.obj@data@min,
                        max      = raster.obj@data@max,
                        xmin     = raster.obj@extent@xmin,
                        xmax     = raster.obj@extent@xmax,
                        ymin     = raster.obj@extent@ymin,
                        ymax     = raster.obj@extent@ymax,
                        crs      = raster.obj@crs@projargs,
                        name     = raster.obj@file@name
                    )
                }) %>% dplyr::bind_rows()

    row <- raster.tb[1,]

    raster.tb[-1,] %>%
        purrrlyr::by_row(function (r){
            ensurer::ensure_that(r$ncols, (.) == row$ncols, err_desc = "raster layers not compatible with ncols values")
            ensurer::ensure_that(r$nrows, (.) == row$nrows, err_desc = "raster layers not compatible with nrows values")
            ensurer::ensure_that(r$min,   (.) == row$min,   err_desc = "raster layers not compatible with min values")
            ensurer::ensure_that(r$max,   (.) == row$max,   err_desc = "raster layers not compatible with max values")
            ensurer::ensure_that(r$xmax,  (.) == row$xmax,  err_desc = "raster layers not compatible with xmax values")
            ensurer::ensure_that(r$xmin,  (.) == row$xmin,  err_desc = "raster layers not compatible with xmin values")
            ensurer::ensure_that(r$ymax,  (.) == row$ymax,  err_desc = "raster layers not compatible with ymax values")
            ensurer::ensure_that(r$ymin,  (.) == row$ymin,  err_desc = "raster layers not compatible with ymin values")
            ensurer::ensure_that(r$crs,   (.) == row$crs,   err_desc = "raster layers not compatible with projection values")
        })

    return (raster.tb)
}

