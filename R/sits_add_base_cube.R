#' @title Add base maps to a time series data cube
#' @name sits_add_base_cube
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function add base maps to time series data cube.
#' Base maps have information that is stable in time (e.g, DEM) which
#' provide relevant information for modelling and classification.
#'
#' To add a base cube to an existing data cube,
#' they should share the same sensor, resolution,
#' bounding box, timeline, and have different bands.
#'
#' @param cube1      Data cube (tibble of class "raster_cube") .
#' @param cube2      Data cube (tibble of class "dem_cube") .
#'
#'
#' @return a merged data cube with the inclusion of a base_info tibble
#' @examples
#' if (sits_run_examples()) {
#'      s2_cube <- sits_cube(
#'            source = "MPC",
#'            collection = "SENTINEL-2-L2A",
#'            tiles = "18HYE",
#'            bands = c("B8A", "CLOUD"),
#'            start_date = "2022-01-01",
#'            end_date = "2022-03-31"
#'            )
#'      output_dir <- paste0(tempdir(), "/reg")
#'      if (!dir.exists(output_dir)) {
#'                dir.create(output_dir)
#'      }
#'      dem_cube <- sits_cube(
#'            source = "MPC",
#'            collection = "COP-DEM-GLO-30",
#'            tiles = "18HYE",
#'            bands = "ELEVATION"
#'            )
#'      s2_reg  <- sits_regularize(
#'            cube = s2_cube,
#'            period = "P1M",
#'            res = 240,
#'            output_dir = output_dir,
#'            multicores = 2,
#'            memsize = 4
#'      )
#'      dem_reg  <- sits_regularize(
#'            cube = dem_cube,
#'            res = 240,
#'            tiles = "18HYE",
#'            output_dir = output_dir,
#'            multicores = 2,
#'            memsize = 4
#'      )
#'      s2_reg <- sits_add_base_cube(s2_reg, dem_reg)
#' }
#' @export
#'
sits_add_base_cube <- function(cube1, cube2){
    .check_set_caller("sits_add_base_cube")
    .check_is_raster_cube(cube1)
    .check_that(.cube_is_regular(cube1))
    .check_that(inherits(cube2, "dem_cube"))
    # pre-condition for merge is having the same tiles
    .check_that(all(cube1[["tile"]] %in% cube2[["tile"]]))
    # add a new tibble with base cube information
    cube1[["base_info"]] <- list(cube2)
    class(cube1) <- c("base_raster_cube", class(cube1))
    return(cube1)
}
