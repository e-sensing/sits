test_that("plot_distances work", {

    # TEST cerrado
    samples_tb <- sitsdata::samples_cerrado_lc8

    roi_sf <- "~/Documents/data/geodata/brazil_biomes/brazil_biomes.shp" %>%
        sf::read_sf()

    sits_plot_distances(samples_tb, roi_sf)


    # TEST 1
    samples_tb <-
        sits::samples_modis_4bands %>%
        sits_select(bands = c("NDVI")) %>%
        dplyr::mutate(id = paste0(longitude, "-", latitude)) %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::select(-id)

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    raster <-
        cube %>%
        dplyr::pull(file_info) %>%
        magrittr::extract2(1) %>%
        dplyr::pull(path) %>%
        dplyr::first() %>%
        terra::rast()

    roi_sf <-
        raster %>%
        terra::ext() %>%
        terra::as.polygons(crs = terra::crs(raster)) %>%
        sf::st_as_sf()

    n = nrow(samples_tb)

    sits_plot_distances(samples_tb, roi_sf)

}
