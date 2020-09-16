context("Database")
test_that("Access to RSQLite",{
    # create RSQLite connection
    home <- Sys.getenv('HOME')
    db_file <- paste0(home,"/sits.sql")
    conn <- sits_db_connect(db_file)
    # write a set of time series
    conn <- sits_db_write(conn, "cerrado_2classes", cerrado_2classes)
    #' # read a set of time series
    ts <-  sits_db_read(conn, "cerrado_2classes")

    vals1 <- dplyr::pull(as.vector(ts$time_series[[1]][2,"NDVI"]))
    vals2 <- dplyr::pull(as.vector(cerrado_2classes$time_series[[1]][2,"NDVI"]))
    expect_equal(vals1, vals2)

    date1 <- ts[3,]$start_date
    date2 <- cerrado_2classes[3,]$start_date
    expect_equal(date1, date2)

    lat1 <- ts[3,]$latitude
    lat2 <- cerrado_2classes[3,]$latitude
    expect_equal(lat1, lat2, tolerance = 0.01)

    # files to build a raster cube
    files <- c(system.file("extdata/raster/mod13q1/sinop-crop-ndvi.tif",
                           package = "sits"))

    # create a raster cube file based on the information about the files
    raster.tb <- sits_cube(type = "BRICK", name  = "Sinop-crop",
                           satellite = "TERRA", sensor = "MODIS",
                           timeline = timeline_modis_392, bands = "NDVI",
                           files = files)

    # write a raster cube
    conn <- sits_db_write(conn, "sinop", raster.tb)
    # read a raster cube
    cube_raster <- sits_db_read(conn, "sinop")

    # test data
    expect_equal(raster.tb$bands[[1]], cube_raster$bands[[1]])
    expect_equal(raster.tb$crs, cube_raster$crs)
    expect_equal(raster.tb$name, cube_raster$name)
    expect_equal(raster.tb$timeline[[1]][[1]], cube_raster$timeline[[1]][[1]])

    samples_mt_ndvi <- sits_select_bands(samples_mt_4bands, ndvi)
    rfor_model <- sits_train(samples_mt_ndvi, sits_rfor(num_trees = 100))
    # classify using one core
    sinop_probs <- sits_classify(raster.tb, rfor_model, memsize = 2)

    expect_true(all(file.exists(unlist(sinop_probs$file_info[[1]]$path))))

    # write a raster probs cube
    conn <- sits_db_write(conn, "sinop_probs", sinop_probs)
    # read a raster cube
    cube_probs <- sits_db_read(conn, "sinop_probs")

    expect_true(nrow(sinop_probs) == nrow(cube_probs))
    expect_true(all(sinop_probs$file_info[[1]]$path[[1]] == cube_probs$file_info[[1]]$path[[1]]))

    # label classification
    sinop_bayes <- sits::sits_label_classification(sinop_probs,
                                                   smoothing = "bayesian")
    expect_true(all(file.exists(unlist(sinop_bayes$file_info[[1]]$path))))

    # save a classified image to the DB
    conn <- sits_db_write(conn, "sinop_bayes", sinop_bayes)
    # read a classified image
    cube_bayes <- sits_db_read(conn, "sinop_bayes")

    expect_true(nrow(sinop_bayes) == nrow(cube_bayes))
    expect_true(all(sinop_bayes$file_info[[1]]$path == cube_bayes$file_info[[1]]$path))

    db.tb <- sits_db_info(conn)

    expect_true(NROW(db.tb) == 4)
    cube_classes <-  sits:::sits.env$config$cube_classes_generic
    db_classes <- c("sits", cube_classes)

    expect_true(all(db.tb$class %in% db_classes))
    expect_true("cerrado_2classes" %in% db.tb$name)
    expect_true("sinop" %in% db.tb$name)

    expect_true(all(file.remove(unlist(sinop_probs$file_info[[1]]$path))))
    expect_true(all(file.remove(unlist(sinop_bayes$file_info[[1]]$path))))

    unlink(db_file)
})
