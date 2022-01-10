test_that("NDVI generation", {
    testthat::skip_on_cran()

    s2_cube <- tryCatch({
        sits_cube(source = "MSPC",
                  collection = "sentinel-2-l2a",
                  tiles = "20LKP",
                  bands = c("B05", "B8A", "CLOUD"),
                  start_date = as.Date("2019-07-18"),
                  end_date = as.Date("2019-08-30")
        )},
        error = function(e){
            return(NULL)
        })

    testthat::skip_if(purrr::is_null(s2_cube),
                      "MSPC is not accessible")

    dir_images <-  paste0(tempdir(), "/images/")
    if (!dir.exists(dir_images))
        suppressWarnings(dir.create(dir_images))

    gc_cube <- sits_regularize(
        cube        = s2_cube,
        output_dir  = dir_images,
        res         = 160,
        period      = "P1M",
        multicores = 2)

    gc_cube_new <- sits_apply(gc_cube,
                              EVI2 = 2.5*(B8A - B05)/(B8A + 2.4*B05 + 1))

    gc_cube_new <- sits_apply(gc_cube,
                              EVI2 = (B8A - B05)/(B8A + B05))

})
