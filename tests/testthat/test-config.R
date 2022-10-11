# save variable value
user_file <- Sys.getenv("SITS_CONFIG_USER_FILE")
user_aws_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
user_aws_secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
test_that("User functions", {

    # check config file
    expect_equal(
        .check_file(.conf_file()),
        .conf_file()
    )

    expect_true(
        Sys.setenv("SITS_CONFIG_USER_FILE" = "")
    )

    # load default config
    expect_message(
        {
            default <- sits_config()
        },
        "To provide additional configurations, create an YAML file"
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

    expect_equal(
        .check_file(.conf_user_file()),
        .conf_user_file()
    )

    expect_message(
        sits_config(),
        "Additional configurations found in"
    )

    expect_equal(
        .conf("processing_bloat"),
        5
    )

    expect_equal(
        .conf("rstac_pagination_limit"),
        999
    )

    expect_equal(
        .conf("gdal_creation_options"),
        c("COMPRESS=LZW", "BIGTIFF=YES")
    )

    expect_equal(
        unname(.colors_get(labels = c(
            "Cropland", "Deforestation",
            "Forest", "Grassland", "NonForest"
        ))),
        c(
            "khaki", "sienna", "darkgreen", "lightgreen",
            "lightsteelblue1"
        )
    )

    # load default + user + user provided values
    expect_message(
        sits_config(
            processing_bloat = 6,
            rstac_pagination_limit = 500,
            raster_api_package = "terra",
            gdal_creation_options = c("BIGTIFF=YES")
        ),
        "Additional configurations found in"
    )

    expect_equal(
        .conf("processing_bloat"),
        6
    )

    expect_equal(
        .conf("rstac_pagination_limit"),
        500
    )

    expect_equal(
        .conf("raster_api_package"),
        "terra"
    )

    expect_equal(
        .conf("gdal_creation_options"),
        c("BIGTIFF=YES")
    )


    expect_output(
        object = sits_config_show(source = "BDC"),
        regexp = "s3_class: bdc_cube, stac_cube, eo_cube, raster_cube"
    )

    expect_output(
        object = sits_config_show(source = "BDC", collection = "CB4_64-1"),
        regexp = "bands:"
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
                        values        = list(),
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

    expect_equal(
        .source_url(source = "TEST"),
        NA_character_
    )

    expect_equal(
        .source_url(source = "BDC"),
        "https://brazildatacube.dpi.inpe.br/stac/"
    )

    expect_equal(
        .source_service(source = "TEST"),
        NA_character_
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
        "invalid 'source' parameter"
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
        "invalid 'source' parameter"
    )

    expect_error(
        .source_collection_check(source = "TEST", collection = "ZZZ"),
        "invalid 'collection' parameter"
    )

    expect_equal(
        .source_collection_check(source = "TEST", collection = "TEST"),
        NULL
    )
    expect_equal(
        sits:::.source_collection_tile_check(
            "MPC",
            "LANDSAT-8-C2-L2",
            "232067"
        ),
        NULL
    )

    # reset config
    expect_true(
        Sys.setenv("SITS_CONFIG_USER_FILE" = "")
    )

    # load default config
    expect_message(
        {
            sits_config(reset = TRUE)
        },
        "To provide additional configurations, create an YAML file"
    )

    expect_equal(
        sits_env$config,
        default
    )
})

test_that("Config AWS", {
    expect_error(
        .conf("zzz"),
        "key 'zzz' not found"
    )

    expect_error(
        .conf_names("zzz"),
        "invalid names for 'zzz' key"
    )

    .source_collection_access_vars_set(
        source = "AWS",
        collection = "SENTINEL-S2-L2A"
    )

    expect_equal(
        Sys.getenv("AWS_DEFAULT_REGION"),
        "eu-central-1"
    )

    expect_equal(
        .conf(
            "sources", "AWS", "collections",
            "SENTINEL-S2-L2A", "access_vars"
        ),
        list(
            AWS_DEFAULT_REGION = "eu-central-1",
            AWS_S3_ENDPOINT = "s3.amazonaws.com",
            AWS_REQUEST_PAYER = "requester",
            AWS_NO_SIGN_REQUEST = FALSE
        )
    )

    expect_true(
        Sys.setenv("AWS_ACCESS_KEY_ID" = "ZZZ")
    )

    expect_true(
        Sys.setenv("AWS_SECRET_ACCESS_KEY" = "ZZZ")
    )

    expect_equal(
        .source_collection_token_check(
            source = "AWS",
            collection = "SENTINEL-S2-L2A"
        ),
        c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
    )

    expect_equal(
        .source_bands(
            source = "AWS",
            collection = "SENTINEL-S2-L2A"
        ),
        c(
            "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A",
            "B09", "B11", "B12", "CLOUD"
        )
    )

    expect_equal(
        .source_bands(
            source = "AWS",
            collection = "SENTINEL-S2-L2A",
            add_cloud = FALSE
        ),
        c(
            "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A",
            "B09", "B11", "B12"
        )
    )

    expect_equal(
        .source_bands(
            source = "AWS",
            collection = "SENTINEL-S2-L2A",
            fn_filter = function(x) 10 %in% x$resolution,
            add_cloud = TRUE
        ),
        c("B02", "B03", "B04", "B08")
    )

    expect_equal(
        .source_bands_resolution(
            source = "AWS",
            collection = "SENTINEL-S2-L2A",
            bands = c("B01", "B03")
        ),
        list(B01 = 60, B03 = 10)
    )

    expect_equal(
        .source_cloud(),
        "CLOUD"
    )
})

test_that("Metatype", {
    expect_error(
        .conf_data_meta_type("abc"),
        "Data not recognized as a sits object"
    )
})

test_that("List collections", {
    expect_output(
        object = sits_list_collections(),
        regexp = "(BDC)|(AWS)|(- MOD13Q1-6 (TERRA/MODIS))"
    )
})

# restore variable value
Sys.setenv("SITS_CONFIG_USER_FILE" = user_file)
Sys.setenv("AWS_ACCESS_KEY_ID" = user_aws_id)
Sys.setenv("AWS_SECRET_ACCESS_KEY" = user_aws_secret)
