test_that("plot geo_distances", {

    samples <-
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

    roi <-
        raster %>%
        terra::ext() %>%
        terra::as.polygons(crs = terra::crs(raster)) %>%
        sf::st_as_sf()

   distances <- sits_geo_dist(samples, roi)

   expect_true("geo_distances" %in% class(distances))
   expect_true(all(c("from", "to", "distance", "type") %in%
                   colnames(distances)))

})
