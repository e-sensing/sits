test_that("Segmentation", {
    # Example of classification of a data cube
    # Create a data cube from local files
    set.seed(29031956)
    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
    sinop <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = data_dir,
        progress = FALSE
    )
    # create output dir
    output_dir <- paste0(tempdir(), "/seg")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    # Segment the cube
    segments <- sits_segment(
        cube = sinop,
        output_dir = output_dir,
        multicores = 1,
        memsize = 24,
        progress = FALSE,
        version = "vt"
    )

    expect_s3_class(object = segments, class = "vector_cube")
    expect_true("vector_info" %in% colnames(segments))
    # Read segments as sf object
    vector_segs <- sits:::.segments_read_vec(segments)
    expect_equal(
        as.character(unique(sf::st_geometry_type(vector_segs))),
        expected = "POLYGON"
    )
    vector_obj <- sits:::.vector_open_vec(segments$vector_info[[1]]$path)

    expect_true("sf" %in% class(vector_obj))

    crs_wkt <- sits:::.vector_crs(vector_obj, wkt = TRUE)
    expect_equal(class(crs_wkt), "character")
    expect_true(grepl("PROJCRS", crs_wkt))

    crs_nowkt <- sits:::.vector_crs(vector_obj, wkt = FALSE)
    expect_equal(class(crs_nowkt), "crs")
    expect_true(grepl("PROJCRS", crs_nowkt$wkt))

    p_segments_ndvi <- plot(segments, band = "NDVI")
    rast_segs <- p_segments_ndvi[[1]]$shp
    expect_equal(nrow(rast_segs), 147)

    # testing resume feature
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_message({
        object <- sits_segment(
            cube = sinop,
            output_dir = output_dir,
            multicores = 1,
            memsize = 24,
            progress = FALSE,
            version = "vt"
        )
    })
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)
    # test read vector cube
    segment_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        raster_cube = sinop,
        vector_dir = output_dir,
        vector_band = "segments",
        version = "vt",
        progress = FALSE
    )
    expect_s3_class(object = segment_cube, class = "vector_cube")
    expect_true("vector_info" %in% colnames(segment_cube))

    # Train a rf model
    samples_filt <- sits_apply(samples_modis_ndvi,
        NDVI = sits_sgolay(NDVI)
    )

    rfor_model <- sits_train(samples_filt, sits_rfor())
    # Create a probability vector cube
    start_date <- sits_timeline(sinop)[1]
    end_date <- sits_timeline(sinop)[length(sits_timeline(sinop))]
    probs_segs <- sits_classify(
        data = segments,
        ml_model = rfor_model,
        filter_fn = sits_sgolay(),
        output_dir = output_dir,
        n_sam_pol = 20,
        multicores = 6,
        memsize = 24,
        start_date = start_date,
        end_date = end_date,
        version = "vt2",
        progress = FALSE
    )
    # test plot
    p_probs_segs <- plot(probs_segs)
    sf_probs <- p_probs_segs[[1]]$shp
    expect_true(all(sf::st_geometry_type(sf_probs) == "POLYGON"))

    expect_s3_class(probs_segs, class = "probs_vector_cube")
    expect_true(
        "vector_info" %in% colnames(probs_segs)
    )
    # Read segments of a probability cube
    vector_probs <- sits:::.segments_read_vec(probs_segs)
    expect_true(
        all(sits_labels(probs_segs) %in% colnames(vector_probs))
    )
    # test resume feature
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_message({
        obj <- sits_classify(
            data = segments,
            ml_model = rfor_model,
            output_dir = output_dir,
            n_sam_pol = 20,
            multicores = 6,
            memsize = 24,
            version = "vt2",
            progress = FALSE
        )
    })
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)
    # Create a classified vector cube
    class_segs <- sits_label_classification(
        cube = probs_segs,
        output_dir = output_dir,
        multicores = 2,
        memsize = 4,
        progress = FALSE
    )
    expect_s3_class(object = class_segs, class = "class_vector_cube")
    expect_true(
        "vector_info" %in% colnames(class_segs)
    )
    # Read segments of a classified cube
    vector_class <- sits:::.segments_read_vec(class_segs)
    expect_equal(nrow(vector_probs), nrow(vector_class))
    expect_true(all(sits_labels(rfor_model) %in% colnames(vector_probs)))
    expect_true(all(sits_labels(rfor_model) %in% colnames(vector_class)))
    expect_true(
        "class" %in% colnames(vector_class)
    )
    p_class_segs <- plot(class_segs)
    sf_segs <- p_class_segs[[1]]$shp
    bbox <- sf::st_bbox(sf_segs)
    expect_true(bbox[["xmin"]] < bbox[["xmax"]])
    expect_true(bbox[["ymin"]] < bbox[["ymax"]])

    # testing resume feature
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_message({
        obj <- sits_label_classification(
            cube = probs_segs,
            output_dir = output_dir,
            multicores = 2,
            memsize = 4,
            progress = FALSE
        )
    })
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)
    uncert_vect <- sits_uncertainty(probs_segs,
        output_dir = output_dir,
        progress = FALSE
    )

    p_uncert_vect <- plot(uncert_vect)
    shp_uncert <- p_uncert_vect[[1]]$shp
    bbox <- sf::st_bbox(shp_uncert)
    expect_true(bbox[["xmin"]] < bbox[["xmax"]])
    expect_true(bbox[["ymin"]] < bbox[["ymax"]])

    sf_uncert <- .segments_read_vec(uncert_vect)
    expect_true("entropy" %in% colnames(sf_uncert))
    expect_equal(nrow(sf_uncert), nrow(vector_class))
    expect_true(all(sits_labels(rfor_model) %in% colnames(sf_uncert)))
})

test_that("Segmentation of large files", {
    set.seed(29031956)
    modis_cube <- .try(
        {
            sits_cube(
                source = "BDC",
                collection = "MOD13Q1-6.1",
                bands = c("NDVI", "EVI", "CLOUD"),
                tiles = "012010",
                start_date = "2018-09-14",
                end_date = "2019-08-29",
                progress = FALSE
            )
        },
        .default = NULL
    )
    testthat::skip_if(purrr::is_null(modis_cube),
        message = "BDC is not accessible"
    )
    output_dir <- paste0(tempdir(), "/segs")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    doc_mode <- Sys.getenv("SITS_DOCUMENTATION_MODE")
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")
    expect_warning(
        modis_cube_local <- sits_regularize(
            cube = modis_cube,
            period = "P1M",
            res = 1000,
            multicores = 6,
            output_dir = output_dir,
            progress = FALSE
        )
    )
    Sys.setenv("SITS_DOCUMENTATION_MODE" = doc_mode)

    .check_cube_is_regular(modis_cube_local)
    segments <- sits_segment(
        cube = modis_cube_local,
        seg_fn = sits_slic(
            step = 50,
            iter = 10,
            minarea = 50
        ),
        output_dir = output_dir,
        multicores = 4,
        memsize = 16,
        progress = FALSE,
        version = "v2bands"
    )
    expect_s3_class(object = segments, class = "vector_cube")
    expect_true("vector_info" %in% colnames(segments))

    # Train a rf model
    rfor_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
    probs_segs <- sits_classify(
        data = segments,
        ml_model = rfor_model,
        output_dir = output_dir,
        n_sam_pol = 10,
        multicores = 6,
        memsize = 24,
        version = "v2bands",
        progress = FALSE
    )
    expect_s3_class(probs_segs, class = "probs_vector_cube")
    expect_true(
        "vector_info" %in% colnames(probs_segs)
    )
    # Read segments of a probability cube
    vector_probs <- .segments_read_vec(probs_segs)
    expect_true(
        all(sits_labels(probs_segs) %in% colnames(vector_probs))
    )
})

test_that("Segmentation with Sentinel-2 and DEM combined", {
    set.seed(29031956)
    s2_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "SENTINEL-2-L2A",
                tiles = "20LKP",
                bands = c("B02", "CLOUD"),
                start_date = "2020-06-01",
                end_date = "2020-07-28",
                progress = FALSE
            )
        },
        .default = NULL
    )

    dir_images <- paste0(tempdir(), "/images_merge_s2_dem_seg/")
    if (!dir.exists(dir_images)) {
        suppressWarnings(dir.create(dir_images))
    }

    testthat::skip_if(
        purrr::is_null(s2_cube),
        "MPC collection is not accessible"
    )

    s2_reg <- suppressWarnings(
        sits_regularize(
            cube = s2_cube,
            period = "P1M",
            res = 240,
            multicores = 2,
            output_dir = dir_images,
            progress = FALSE
        )
    )

    dem_cube <- .try(
        {
            sits_cube(
                source = "MPC",
                collection = "COP-DEM-GLO-30",
                roi = sits_bbox(s2_reg)
            )
        },
        .default = NULL
    )
    testthat::skip_if(
        purrr::is_null(dem_cube),
        "MPC collection is not accessible"
    )

    dem_reg <- suppressWarnings(
        sits_regularize(
            cube = dem_cube,
            res = 240,
            grid_system = "MGRS",
            roi = sits_bbox(s2_reg),
            multicores = 2,
            output_dir = dir_images,
            progress = FALSE
        )
    )

    dem_reg <- sits_select(dem_reg, tiles = "20LKP")
    # Merging images without writing
    cube_merged <- sits_add_base_cube(
        s2_reg,
        dem_reg
    )
    testthat::expect_true(
        all(
            sits_bands(cube_merged) %in% c(
                sits_bands(s2_reg),
                sits_bands(dem_reg)
            )
        )
    )
    testthat::expect_equal(nrow(cube_merged), 1)
    testthat::expect_s3_class(cube_merged, "base_raster_cube")

    # Extracting some points from base cube
    pts <- tibble::tibble(
        longitude = c(-65.48027, -65.4872),
        latitude = c(-10.43557, -10.54364),
        start_date = .as_date(c("2020-06-01", "2020-06-01")),
        end_date = .as_date(c("2020-09-28", "2020-09-28")),
        label = c("Water", "Forest")
    )
    ts <- sits_get_data(cube_merged, pts)

    # Segment the merged cube
    object <- sits_segment(
        cube = cube_merged,
        seg_fn = sits_slic(iter = 10),
        multicores = 2,
        memsize = 24,
        progress = FALSE,
        output_dir = dir_images,
        version = "vt"
    )
    testthat::expect_s3_class(object, "segs_cube")
    #
    rfor <- sits_train(ts, sits_rfor())
    # Classify segments
    segs_probs <- sits_classify(
        data = object,
        ml_model = rfor,
        multicores = 2,
        memsize = 24,
        progress = FALSE,
        output_dir = dir_images
    )

    unlink(list.files(dir_images, pattern = ".tif", full.names = TRUE))
    unlink(list.files(dir_images, pattern = ".gpkg", full.names = TRUE))
})
