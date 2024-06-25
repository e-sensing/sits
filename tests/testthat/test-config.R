# save variable value
user_file <- Sys.getenv("SITS_CONFIG_USER_FILE")
test_that("User functions", {
    expect_true(
        Sys.setenv("SITS_CONFIG_USER_FILE" = "")
    )

    # load default + user config
    expect_true(
        Sys.setenv(
            "SITS_CONFIG_USER_FILE" =
                system.file("extdata/config_user_example.yml",
                    package = "sits"
                )
        )
    )
    default <- sits_config()

    expect_equal(
        unname(.colors_get(labels = c(
            "Rangeland", "Forest_Land", "Water"
        ), legend = NULL, palette = "Spectral", rev = TRUE)),
        c(
            "#F1C40F", "#27AE60", "#2980B9"
        )
    )
    # add a new source, collection
    .conf_set_options(
        sources = list(TEST = .conf_new_source(
            s3_class = c("bdc_cube", "stac_cube", "raster_cube"),
            collections = list(TEST = .conf_new_collection(
                bands = list(
                    B2 = .conf_new_band(
                        missing_value = 0,
                        minimum_value = 1,
                        maximum_value = 65455,
                        scale_factor  = 0.0000275,
                        offset_value  = -0.2,
                        resampling    = "bilinear",
                        band_name     = "SR_B2",
                        resolution    = 30
                    ),
                    CLOUD = .conf_new_cloud_band(
                        bit_mask      = TRUE,
                        values        = list(
                            "0" = "No Data",
                            "127" = "Clear Pixel",
                            "255" = "Cloud"),
                        interp_values = 1,
                        resampling    = "near",
                        resolution    = 30,
                        band_name     = "QA_PIXEL"
                    )
                ),
                satellite = "SENTINEL-2",
                sensor = "MSI"
            ))
        ))
    )

    expect_error(
        object = .source_url(source = "TEST"),
        regexp = "key 'sources->TEST->url' not found in config"
    )

    expect_equal(
        .source_url(source = "BDC"),
        "https://brazildatacube.dpi.inpe.br/stac/"
    )

    expect_error(
        .source_service(source = "TEST")
    )

    expect_equal(
        .source_service(source = "BDC"),
        "STAC"
    )

    expect_equal(
        .source_s3class(source = "TEST"),
        c("bdc_cube", "stac_cube", "raster_cube")
    )
    expect_equal(
        .source_s3class(source = "BDC"),
        c("bdc_cube", "stac_cube", "eo_cube", "raster_cube")
    )

    expect_error(
        .source_check(source = "ZZZ"),
        "invalid source parameter"
    )

    expect_equal(
        .source_check(source = "TEST"),
        NULL
    )

    expect_equal(
        .source_check(source = "BDC"),
        NULL
    )

    expect_equal(
        .source_collections(source = "TEST"),
        "TEST"
    )

    expect_error(
        .source_collection_check(source = "ZZZ", collection = "ZZZ"),
        ".source_check: invalid source variable - invalid source parameter"
    )

    expect_error(
        .source_collection_check(source = "TEST", collection = "ZZZ"),
        ".source_collection_check: invalid collection variable - collection is not available in data provider or sits is not configured to access it"
    )

    expect_equal(
        .source_collection_check(source = "TEST", collection = "TEST"),
        NULL
    )
    expect_equal(
        .source_collection_tile_check(
            "MPC",
            "LANDSAT-8-C2-L2",
            "232067"
        ),
        NULL
    )
})

# restore variable value
Sys.setenv("SITS_CONFIG_USER_FILE" = user_file)
