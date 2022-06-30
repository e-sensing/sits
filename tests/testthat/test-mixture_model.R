test_that("Mixture model with RMSE band", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_"
    )

    endmembers_spectra <- tibble::tibble(
        type = c("vegetation", "not-vegetation"),
        NDVI = c(8500, 3000)
    )

    mixture_cube <- sits_mixture_model(
        cube = modis_cube,
        endmembers_spectra = endmembers_spectra,
        memsize = 4,
        multicores = 2,
        output_dir = tempdir()
    )

    frac_bands <- sits_bands(mixture_cube)

    expect_true(all(frac_bands %in% c("NOT-VEGETATION", "RMSE", "VEGETATION")))
    expect_true("raster_cube" %in% class(mixture_cube))
    expect_true(all(sits_timeline(modis_cube) %in% sits_timeline(mixture_cube)))
    expect_true(all(file.exists(unlist(mixture_cube$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(mixture_cube$file_info[[1]]$path[[2]])

    expect_true(.raster_nrows(r_obj) == .cube_size(modis_cube)[["nrows"]])
    unlink(list.files(tempdir(), full.names = TRUE))
})

test_that("Mixture model without RMSE band", {

    data_dir <- system.file("extdata/raster/mod13q1", package = "sits")

    modis_cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6",
        data_dir = data_dir,
        delim = "_"
    )

    endmembers_spectra <- tibble::tibble(
        type = c("vegetation", "not-vegetation"),
        NDVI = c(8500, 3000)
    )

    mixture_cube <- sits_mixture_model(
        cube = modis_cube,
        endmembers_spectra = endmembers_spectra,
        memsize = 4,
        multicores = 2,
        output_dir = tempdir(),
        rmse_band = FALSE
    )

    frac_bands <- sits_bands(mixture_cube)

    expect_true(all(frac_bands %in% c("NOT-VEGETATION", "VEGETATION")))
    expect_true("raster_cube" %in% class(mixture_cube))
    expect_true(all(sits_timeline(modis_cube) %in% sits_timeline(mixture_cube)))
    expect_true(all(file.exists(unlist(mixture_cube$file_info[[1]]$path))))

    r_obj <- .raster_open_rast(mixture_cube$file_info[[1]]$path[[2]])

    expect_true(.raster_nrows(r_obj) == .cube_size(modis_cube)[["nrows"]])
    unlink(list.files(tempdir(), full.names = TRUE))
})

