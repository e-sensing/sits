context("Database")
test_that("Access to RSQLite", {
    # create RSQLite connection
    db_file <- paste0(tempdir(), "/sits.sql")
    conn <- sits_db_connect(db_file)
    # write a set of time series
    conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
    #' # read a set of time series
    ts <- sits_db_read(conn, "cerrado_2classes")

    vals1 <- dplyr::pull(as.vector(ts$time_series[[1]][2, "NDVI"]))
    vals2 <- dplyr::pull(cerrado_2classes$time_series[[1]][2, "NDVI"])
    expect_equal(vals1, vals2)

    date1 <- ts[3, ]$start_date
    date2 <- cerrado_2classes[3, ]$start_date
    expect_equal(date1, date2)

    lat1 <- ts[3, ]$latitude
    lat2 <- cerrado_2classes[3, ]$latitude
    expect_equal(lat1, lat2, tolerance = 0.01)

    # files to build a raster cube
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
        package = "sits"
    ))

    # create a raster cube file based on the information about the files
    raster_cube <- sits_cube(
        type = "BRICK",
        name = "Sinop-crop",
        satellite = "TERRA",
        sensor = "MODIS",
        timeline = timeline_modis_392,
        bands = "NDVI",
        files = files
    )

    # write a raster cube
    conn <- suppressWarnings(sits_db_write(conn, "sinop", raster_cube))
    # read a raster cube
    cube_raster <- sits_db_read(conn, "sinop")

    # test data
    expect_true(all(sits_bands(raster_cube) %in% sits_bands(cube_raster)))
    expect_true(raster_cube$crs == cube_raster$crs)
    expect_true(raster_cube$name == cube_raster$name)
    expect_true(all(sits_timeline(raster_cube) %in% sits_timeline(cube_raster)))

    samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = "NDVI")
    rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 100))
    # classify using one core
    sinop_probs <- suppressMessages(
        sits_classify(raster_cube,
                      rfor_model,
                      memsize = 2,
                      output_dir = tempdir()
        )
    )

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))

    # write a raster probs cube
    conn <- sits_db_write(conn, "sinop_probs", sinop_probs)
    # read a raster cube
    cube_probs <- sits_db_read(conn, "sinop_probs")

    expect_true(nrow(sinop_probs) == nrow(cube_probs))
    expect_true(all(sinop_probs$file_info[[1]]$path[[1]] ==
                        cube_probs$file_info[[1]]$path[[1]]
                    )
    )

    # label classification
    sinop_label <- sits::sits_label_classification(sinop_probs,
        output_dir = tempdir()
    )

    # save a classified image to the DB
    conn <- sits_db_write(conn, "sinop_label", sinop_label)
    # read a classified image
    cube_label <- sits_db_read(conn, "sinop_label")

    expect_true(nrow(sinop_label) == nrow(cube_label))
    expect_true(all(sinop_label$file_info[[1]]$path ==
                        cube_label$file_info[[1]]$path)
    )
    db <- sits_db_info(conn)

    expect_true(nrow(db) == 4)
    cube_classes <- sits:::sits_env$config$cube_classes
    db_classes <- c("sits", cube_classes)

    expect_true(all(db$class %in% db_classes))
    expect_true("cerrado_2classes" %in% db$name)
    expect_true("sinop" %in% db$name)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_label$file_info[[1]]$path))))

    unlink(db_file)
})
