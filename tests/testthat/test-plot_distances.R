test_that("plot_distances work", {

    samples_tb <- sits_select(
        sits::samples_modis_4bands,
        bands = c("NDVI")
    )

    roi_sf <- sf::st_as_sf(samples_tb,
                           coords = c("longitude", "latitude"),
                           crs = 4326,
                           remove = FALSE) %>%
                           sf::st_bbox() %>%
                           sf::st_as_sfc()

}
