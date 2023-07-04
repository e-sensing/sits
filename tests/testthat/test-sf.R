test_that("sf", {
    # define a shapefile to be read from the cube
    shp_file <- system.file("extdata/shapefiles/bdc-test/samples.shp",
                            package = "sits"
    )
    sf_shape <- sf::read_sf(shp_file)
    sf_object <- sf_shape

    sf_object$start_date <- "2021-01-01"
    sf_object$end_date <- "2021-12-31"

    # empty a geometry
    sf_object[1,6] <- NA

    expect_warning(
        .sf_to_tibble(
        sf_object = sf_object,
        label_attr = "label",
        label = "Crop",
        n_sam_pol = 10,
        pol_id = NULL,
        start_date = "2020-01-01",
        end_date = "2020-12-31"
        )
    )
    # define a shapefile to be read from the cube
    point_file <- system.file("extdata/shapefiles/cerrado/cerrado_forested.shp",
                            package = "sits"
    )
    sf_point <- sf::read_sf(point_file)
    # case 1
    sf_point_1 <- sf_point
    sf_point_1$label <- "Cerradao"
    tb <- .sf_point_to_tibble(sf_point_1, label_attr = NULL, label = NULL)
    expect_equal(nrow(tb), 40)
    expect_true(all(tb$label == "Cerradao"))
    # case 2
    sf_point_2 <- sf_point
    sf_point_2$class <- "Cerradao"
    tb2 <- .sf_point_to_tibble(sf_point_2, label_attr = "class", label = NULL)
    expect_equal(nrow(tb2), 40)
    expect_true(all(tb2$label == "Cerradao"))
    # case 3
    sf_point_3 <- sf_point
    tb3 <- .sf_point_to_tibble(sf_point_2,
                               label_attr = NULL, label = "Cerradao")
    expect_equal(nrow(tb3), 40)
    expect_true(all(tb3$label == "Cerradao"))

    # polygon with labels
    pol_file <- system.file("extdata/shapefiles/mato_grosso/mt.shp",
                              package = "sits"
    )
    sf_pol <- sf::read_sf(pol_file)
    sf_pol$label <- "MatoGrosso"

    tbp <- .sf_polygon_to_tibble(sf_pol,
                                 label_attr = NULL,
                                 label = NULL,
                                 n_sam_pol = 10,
                                 pol_id = "CD_GEOCUF")
    expect_equal(nrow(tbp), 10)
    expect_true(all(tbp$label == "MatoGrosso"))
})
