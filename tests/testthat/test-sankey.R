make_sankey_class_cube <- function() {
    data_dir <- system.file("extdata/raster/classif", package = "sits")
    sits_cube(
        source = "MPC",
        collection = "SENTINEL-2-L2A",
        data_dir = data_dir,
        parse_info = c(
            "X1", "X2", "tile", "start_date", "end_date",
            "band", "version"
        ),
        bands = "class",
        labels = c(
            "1" = "ClearCut_Fire", "2" = "ClearCut_Soil",
            "3" = "ClearCut_Veg", "4" = "Forest"
        ),
        progress = FALSE
    )
}

test_that("sits_sankey plots the class trajectories", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggalluvial")

    p <- sits_sankey(
        make_sankey_class_cube(),
        make_sankey_class_cube(),
        labels = c("2020", "2021"),
        multicores = 1,
        progress = FALSE
    )

    expect_s3_class(p, "ggplot")
})

test_that("sits_sankey accepts both variadic and list inputs", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggalluvial")

    cubes <- list(
        make_sankey_class_cube(),
        make_sankey_class_cube()
    )

    # plot using dots
    p_dots <- sits_sankey(
        cubes[[1]],
        cubes[[2]],
        labels = c("2020", "2021"),
        multicores = 1,
        progress = FALSE
    )

    # plot using list
    p_list <- sits_sankey(
        cubes = cubes,
        labels = c("2020", "2021"), multicores = 1, progress = FALSE
    )

    # test
    expect_s3_class(p_dots, "ggplot")
    expect_equal(p_dots[["data"]], p_list[["data"]])
})

test_that("sits_sankey validates its inputs", {
    class_cube <- make_sankey_class_cube()

    # one cube is provided produces an error
    expect_error(sits_sankey(class_cube, progress = FALSE))

    # both dots and list inputs at once produces an error
    expect_error(
        sits_sankey(
            class_cube,
            cubes = list(class_cube, class_cube),
            progress = FALSE
        )
    )

    # labels length not matching number of cubes produces an error
    expect_error(
        sits_sankey(
            class_cube,
            class_cube,
            labels = "2020",
            progress = FALSE
        )
    )

    # explicitly duplicated labels are rejected
    expect_error(
        sits_sankey(
            class_cube,
            class_cube,
            labels = c("2020", "2020"),
            progress = FALSE
        )
    )

    # cubes covering different tiles are rejected
    prodes_cube <- sits_cube(
        source = "USGS",
        collection = "LANDSAT-C2L2-SR",
        data_dir = system.file("extdata/raster/prodes", package = "sits"),
        parse_info = c(
            "X1", "X2", "tile", "start_date", "end_date", "band", "version"
        ),
        bands = "class",
        version = "v20220606",
        labels = c(
            "1" = "Forest",
            "11" = "d2012",
            "16" = "d2017",
            "17" = "d2018",
            "27" = "d2019",
            "29" = "d2020",
            "32" = "Clouds2021",
            "33" = "d2021"
        ),
        progress = FALSE
    )

    # test
    expect_error(
        sits_sankey(
            class_cube,
            prodes_cube,
            progress = FALSE
        )
    )
})

test_that("sits_sankey deduplicates repeated step years with a warning", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggalluvial")

    # both cubes share the same start year, so the derived steps collide
    expect_warning(
        p <- sits_sankey(
            make_sankey_class_cube(),
            make_sankey_class_cube(),
            multicores = 1,
            progress = FALSE
        )
    )

    expect_s3_class(p, "ggplot")
})

test_that(".sankey_trajectories counts pixel trajectories", {
    cubes <- list(
        make_sankey_class_cube(),
        make_sankey_class_cube()
    )

    traj <- .sankey_trajectories(
        cubes = cubes,
        timeline_labels = c("2020", "2021"),
        roi = NULL,
        memsize = 4,
        multicores = 1,
        progress = FALSE
    )

    # one column per step plus the frequency column
    expect_equal(colnames(traj), c("2020", "2021", "freq"))

    # frequencies are strictly positive pixel counts
    expect_true(all(traj[["freq"]] > 0))

    # identical cubes yield only "stable" trajectories (class -> same class)
    expect_true(all(traj[["2020"]] == traj[["2021"]]))
})
