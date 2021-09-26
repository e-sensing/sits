# save variable value
user_file <- Sys.getenv("SITS_CONFIG_USER_FILE")
user_aws_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
user_aws_secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
test_that("User functions", {

    # check config file
    expect_equal(
        .check_file(.config_file()),
        .config_file()
    )

    expect_true(
        Sys.setenv("SITS_CONFIG_USER_FILE" = "")
    )

    # load default config
    expect_message({
        default <- sits_config()
    }, "To provide additional configurations, create an YAML file")

    expect_equal(
        .config_processing_bloat(),
        5
    )

    expect_equal(
        .config_rstac_limit(),
        1000
    )

    expect_equal(
        .config_raster_pkg(),
        "terra"
    )

    expect_equal(
        .config_gtiff_default_options(),
        c("COMPRESS=LZW", "BIGTIFF=YES")
    )

    expect_equal(
        .sources(),
        c("CLASSIFIED", "PROBS", "BDC", "WTSS", "SATVEG", "AWS", "OPENDATA",
          "DEAFRICA", "LOCAL")
    )

    expect_equal(
        .config_palettes(),
        "default"
    )

    # load default + user config
    expect_true(
        Sys.setenv("SITS_CONFIG_USER_FILE" =
                       system.file("extdata/config_user_example.yml",
                                   package = "sits"))
    )

    expect_equal(
        .check_file(.config_user_file()),
        .config_user_file()
    )

    expect_message(
        sits_config(),
        "Additional configurations found in"
    )

    expect_equal(
        .config_processing_bloat(),
        5
    )

    expect_equal(
        .config_rstac_limit(),
        999
    )

    expect_equal(
        .config_gtiff_default_options(),
        c("COMPRESS=LZW", "BIGTIFF=YES")
    )

    expect_equal(
        .sources(),
        c("CLASSIFIED", "PROBS", "BDC", "WTSS", "SATVEG", "AWS", "OPENDATA",
          "DEAFRICA", "LOCAL")
    )

    expect_equal(
        .config_palettes(),
        c("default", "my_project")
    )

    expect_error(
        .config_palette_check(palette = "zzz"),
        "invalid 'palette' parameter"
    )

    expect_equal(
        .config_palette_check(palette = "my_project"),
        c("my_project")
    )

    expect_error(
        .config_palette_colors(labels = c("Cropland", "Deforestation",
                                          "Forest", "Grassland", "NonForest",
                                          "Cropland", "Deforestation",
                                          "Forest", "Grassland", "NonForest"),
                               palette = "my_project")
    )
    expect_equal(
        unname(.config_palette_colors(labels = c("Cropland", "Deforestation",
                                          "Forest", "Grassland", "NonForest"),
                               palette = "my_project")),
        c("khaki", "sienna", "darkgreen", "lightgreen",
          "lightsteelblue1")
    )

    # load default + user + user provided values
    expect_message(
        sits_config(processing_bloat = 6,
                    rstac_pagination_limit = 500,
                    raster_api_package = "terra",
                    gdal_creation_options = c("BIGTIFF=YES")),
        "Additional configurations found in"
    )

    expect_equal(
        .config_processing_bloat(),
        6
    )

    expect_equal(
        .config_rstac_limit(),
        500
    )

    expect_equal(
        .config_raster_pkg(),
        "terra"
    )

    expect_equal(
        .config_gtiff_default_options(),
        c("BIGTIFF=YES")
    )

    expect_equal(
        .sources(),
        c("CLASSIFIED", "PROBS", "BDC", "WTSS", "SATVEG", "AWS", "OPENDATA",
          "DEAFRICA", "LOCAL")
    )

    expect_equal(
        .config_palettes(),
        c("default", "my_project")
    )

    config_txt <- capture.output({
        sits_config_show()
    })

    expect_true(
        any(grepl(
            "- CLASSIFIED, PROBS, BDC, WTSS, SATVEG, AWS, OPENDATA, DEAFRICA, LOCAL",
            config_txt
        ))
    )

    config_txt <- capture.output({
        sits_config_show(source = "BDC")
    })

    expect_true(
        any(grepl(
            "s3_class: bdc_cube, stac_cube, raster_cube",
            config_txt
        ))
    )

    config_txt <- capture.output({
        sits_config_show(source = "BDC",
                         collection = "CB4_64-1")
    })

    expect_true(
        any(grepl(
            "bands:",
            config_txt
        ))
    )

    config_txt <- capture.output({
        sits_config_show(palette = "default")
    })

    expect_true(
        any(grepl(
            "Water: royalblue3",
            config_txt
        ))
    )

    # add a new source, collection
    .config_set_options(
        sources = list(TEST = .config_new_source(
            s3_class      = c("bdc_cube", "stac_cube", "raster_cube"),
            collections   = list(TEST = .config_new_collection(
                bands     = list(
                    B2    = .config_new_band(
                        missing_value = 0,
                        minimum_value = 1,
                        maximum_value = 65455,
                        scale_factor  = 0.0000275,
                        offset_value  = -0.2,
                        resampling    = "bilinear",
                        band_name     = "SR_B2",
                        resolutions   = 30),
                    CLOUD = .config_new_cloud_band(
                        bit_mask      = TRUE,
                        values        = list(),
                        interp_values = 1,
                        resampling    = "near",
                        resolutions   = 30,
                        band_name     = "QA_PIXEL")
                ),
                satellite = "SENTINEL-2",
                sensor = "MSI")
            )
        )))

    expect_equal(
        .sources(),
        c("CLASSIFIED", "PROBS", "BDC", "WTSS", "SATVEG", "AWS", "OPENDATA",
          "DEAFRICA", "LOCAL", "TEST")
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
        c("bdc_cube", "stac_cube", "raster_cube")
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

    # reset config
    expect_true(
        Sys.setenv("SITS_CONFIG_USER_FILE" = "")
    )

    # load default config
    expect_message({
        sits_config(reset = TRUE)
    }, "To provide additional configurations, create an YAML file")

    expect_equal(
        sits_env$config,
        default
    )

})

test_that("Configs", {

    expect_error(
        .config_get(key = c("zzz")),
        "key 'zzz' not found"
    )

    expect_equal(
        .config_get(key = c("zzz"), default = "aaa"),
        "aaa"
    )

    expect_error(
        .config_names(key = c("zzz")),
        "invalid names for 'zzz' key"
    )

    expect_equal(
        .config_names(key = c("palettes")),
        c("default")
    )

    expect_equal(
        .source_collection_aws(source = "AWS",
                               collection = "SENTINEL-S2-L2A"),
        list(AWS_DEFAULT_REGION = "eu-central-1",
             AWS_S3_ENDPOINT = "s3.amazonaws.com",
             AWS_REQUEST_PAYER = "requester")
    )

    expect_equal(
        .source_collection_aws(source = "BDC",
                               collection = "CB4_64-1"),
        list()
    )

    expect_true(
        Sys.setenv("AWS_ACCESS_KEY_ID" = "ZZZ")
    )

    expect_true(
        Sys.setenv("AWS_SECRET_ACCESS_KEY" = "ZZZ")
    )

    expect_equal(
        .source_collection_aws_check(source = "AWS",
                                     collection = "SENTINEL-S2-L2A"),
        NULL
    )

    expect_error(
        .source_collection_aws_check(source = "BDC",
                                     collection = "CB4_64-1"),
        "missing AWS_DEFAULT_REGION, AWS_S3_ENDPOINT and, AWS_REQUEST_PAYER"
    )

    expect_equal(
        .source_bands(source = "AWS",
                      collection = "SENTINEL-S2-L2A"),
        c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A",
          "B09", "B11", "B12", "CLOUD")
    )

    expect_equal(
        .source_bands(source = "AWS",
                      collection = "SENTINEL-S2-L2A",
                      add_cloud = FALSE),
        c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A",
          "B09", "B11", "B12")
    )

    expect_equal(
        .source_bands(source = "AWS",
                      collection = "SENTINEL-S2-L2A",
                      fn_filter = function(x) 10 %in% x$resolution,
                      add_cloud = TRUE),
        c("B02", "B03", "B04", "B08")
    )

    expect_equal(
        .source_bands_reap(source = "BDC",
                           collection = "CB4_64-1",
                           key = "resampling",
                           bands = c("NDVI", "EVI"),
                           default = NA_character_),
        list(NDVI = "bilinear", EVI = "bilinear")
    )

    expect_error(
        .source_bands_reap(source = "BDC",
                           collection = "CB4_64-1",
                           key = "zzz"),
        paste0("key 'sources\\$BDC\\$collections\\$CB4_64-1\\$bands",
               "\\$NDVI\\$zzz' not found")
    )

    expect_equal(
        .source_bands_band_name(source = "BDC",
                                collection = "MOD13Q1-6",
                                bands = c("BLUE", "RED")),
        c("blue_reflectance", "red_reflectance")
    )

    expect_equal(
        .source_bands_resolutions(source = "AWS",
                                  collection = "SENTINEL-S2-L2A",
                                  bands = c("B01", "B03")),
        list(B01 = 60, B03 = c(10, 20, 60))
    )

    expect_equal(
        .source_cloud(),
        "CLOUD"
    )

    expect_false(
        .source_cloud_bit_mask(source = "BDC",
                               collection = "CB4_64_16D_STK-1")
    )

    expect_equal(
        names(.source_cloud_values(source = "WTSS",
                                   collection = "CB4_64-1")),
        c("0", "127", "255")
    )

    expect_equal(
        .source_cloud_interp_values(source = "WTSS",
                                    collection = "S2_10-1"),
        c(2, 3, 4, 255)
    )
})

# restore variable value
Sys.setenv("SITS_CONFIG_USER_FILE" = user_file)
Sys.setenv("AWS_ACCESS_KEY_ID" = user_aws_id)
Sys.setenv("AWS_SECRET_ACCESS_KEY" = user_aws_secret)
