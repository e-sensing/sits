#' @title Plot class trajectories from multi-temporal classified cubes
#' @name sits_sankey
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description
#' Builds a Sankey (alluvial) diagram showing how each pixel changes class
#' across a sequence of classified cubes (e.g., yearly land-use/land-cover maps
#' of the same area). It reveals the "from-to" class dynamics over time, which
#' is useful to inspect transitions and multi-year classification consistency.
#'
#' The cubes specified must share the same tiles.
#'
#' @param ...        Two or more classified cubes (\code{class_cube}), one per
#'                   time step, given as separate arguments. Ignored when
#'                   \code{cubes} is supplied.
#' @param cubes      Alternatively, a \code{list} of classified cubes. Provide
#'                   either this argument or \code{...}, not both.
#' @param labels     Optional character vector naming each step (one per cube),
#'                   shown on the diagram x-axis. Defaults to the start year of
#'                   each cube.
#' @param roi        Optional region of interest restricting the computation.
#' @param legend     Named vector that associates labels to colors (overrides
#'                   the default sits colors).
#' @param palette    A "cols4all" palette used for labels without an assigned
#'                   color (default = "Set3").
#' @param title      Plot title (default = "Class trajectories").
#' @param memsize    Maximum memory available (in GB, default = 4).
#' @param multicores Number of cores for parallel processing (default = 2).
#' @param progress   Show a progress bar? (default = TRUE).
#'
#' @return A \code{ggplot} object with the Sankey diagram.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # train a random forest model
#'     rfor_model <- sits_train(samples_modis_ndvi, sits_rfor())
#'     # create a data cube from local files
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir = data_dir
#'     )
#'     # classify and label the cube for two time steps
#'     probs_cube <- sits_classify(cube, rfor_model, output_dir = tempdir())
#'     class_2013 <- sits_label_classification(
#'         probs_cube, output_dir = tempdir(), version = "v2013"
#'     )
#'     # a second classified cube for another time step
#'     class_2014 <- sits_label_classification(
#'         probs_cube, output_dir = tempdir(), version = "v2014"
#'     )
#'     # plot the Sankey diagram of class trajectories between the two steps
#'     sits_sankey(class_2013, class_2014, labels = c("2013", "2014"))
#' }
#' @export
sits_sankey <- function(...,
                        cubes = NULL,
                        labels = NULL,
                        roi = NULL,
                        legend = NULL,
                        palette = "Set3",
                        title = "Class trajectories",
                        memsize = 4L,
                        multicores = 2L,
                        progress = TRUE) {
    .check_set_caller("sits_sankey")
    # pre-conditions: required packages
    .check_require_packages(c("ggplot2", "ggalluvial"))
    # pre-conditions: resources, roi and progress bar
    .check_roi(roi)
    .check_lgl_parameter(progress)
    .check_num_parameter(memsize, min = 1L, max = 16384L)
    .check_num_parameter(multicores, min = 1L, max = 2048L)
    # accept both dots and cubes arg
    dots <- list(...)
    # pre-condition: users can only use dots or cubes to define cubes
    .check_that(
        xor(length(dots) > 0L, .has(cubes)),
        msg = .conf("messages", "sits_sankey_input")
    )
    # select cubes from dots or cubes arg
    cubes <- if (.has(cubes)) cubes else dots
    # pre-condition: tiles must be aligned
    .sankey_check_cubes(cubes)
    # derive the step names
    timeline_labels <- .sankey_labels(cubes, labels)
    # compute the trajectory frequency table
    traj <- .sankey_trajectories(
        cubes = cubes,
        timeline_labels = timeline_labels,
        roi = roi,
        memsize = memsize,
        multicores = multicores,
        progress = progress
    )
    # ordered union of class labels across steps
    class_labels <- unique(unlist(
        purrr::map(cubes, .tile_labels),
        use.names = FALSE
    ))
    # build sankey diagram
    .sankey_plot(
        data = traj,
        timeline_labels = timeline_labels,
        class_labels = class_labels,
        legend = legend,
        palette = palette,
        title = title
    )
}
