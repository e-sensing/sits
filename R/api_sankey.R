#' @title Resolve the step names of a Sankey diagram
#' @name .sankey_labels
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#'
#' @param cubes  A list of \code{class_cube} objects.
#' @param labels Optional character vector with one name per cube.
#' @return A character vector with the name of each timeline step.
.sankey_labels <- function(cubes, labels) {
    # if the user provided labels, use them as the name of each
    # step in the sankey timeline
    if (.has(labels)) {
        # validate if the labels provided match the number of cubes
        labels_valid <- length(labels) == length(cubes)
        # and validate duplications. If there are duplications, the
        # plot will fail.
        labels_valid <- labels_valid && !anyDuplicated(labels)
        # check if labels are valid
        .check_that(
            labels_valid, msg = .conf("messages", "sits_sankey_labels")
        )
        # if so, define timeline labels as the provided labels
        timeline_labels <- as.character(labels)
    } else {
        # if no label is provided, we can derivate them. for this,
        # we can use the dates of the cube
        timeline_labels <- purrr::map_chr(cubes, function(cube) {
            format(.tile_start_date(.tile(cube)), "%Y")
        })
        # cubes sharing a start year yield duplicated step names, which breaks
        # the plot function. In such case, use generic ids
        if (anyDuplicated(timeline_labels) > 0L) {
            # inform user what we are doing
            if (.message_warnings()) {
                warning(
                    .conf("messages", "sits_sankey_steps_duplicated"),
                    call. = FALSE
                )
            }
            # generate unique ids
            timeline_labels <- make.unique(timeline_labels, sep = "_")
        }
    }
    # return!
    timeline_labels
}

#' @title Validate the cubes used to build a Sankey trajectory table
#' @name .sankey_check_cubes
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @description
#' A Sankey diagram tracks how each pixel changes class across a sequence of
#' classification results. This requires at least two classified cubes that
#' cover exactly the same tiles, so that pixels at the same position can be
#' compared step by step.
#'
#' @param cubes A list of \code{class_cube} objects.
#' @return Called for side effects (throws an error on invalid input).
.sankey_check_cubes <- function(cubes) {
    .check_set_caller(".sankey_check_cubes")
    # all cubes must be `class_cube`
    has_two_cubes <- all(purrr::map_lgl(cubes, inherits, "class_cube"))
    # we need at least two cubes
    has_two_cubes <- has_two_cubes && length(cubes) >= 2
    # check if there are two valid cubes
    .check_that(
        has_two_cubes, msg = .conf("messages", "sits_sankey_cubes")
    )
    # get tiles from first cube
    base_tiles <- sort(.cube_tiles(cubes[[1]]))
    # compare cube tiles
    has_same_tiles <- all(purrr::map_lgl(cubes[-1], function(cube) {
        identical(sort(.cube_tiles(cube)), base_tiles)
    }))
    # all cubes must cover exactly the same tiles
    .check_that(has_same_tiles, msg = .conf("messages", "sits_sankey_tiles"))
}

#' @title Build the pixel trajectory frequency table for a Sankey diagram
#' @name .sankey_trajectories
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @description
#' Reads the classified cubes block by block, and counts how many
#' pixels follow each unique sequence of class labels (its trajectory). Only
#' the trajectory and its frequency are kept. The pixel positions are not
#' needed for the diagram, which keeps memory usage low even for large cubes.
#'
#' @param cubes      A list of \code{class_cube} objects sharing the same tiles.
#' @param timeline_labels Character vector naming each step (one per cube).
#' @param roi        Optional region of interest to restrict the computation.
#' @param memsize    Maximum memory available (in GB).
#' @param multicores Number of cores for parallel processing.
#' @param progress   Show a progress bar?
#' @return A tibble with pixel trajectories
.sankey_trajectories <- function(cubes, timeline_labels, roi, memsize,
                                 multicores, progress) {
    # as all cubes share the same tiles, here we use the first cube as
    # the reference for the raster template and chunk size
    base_tile <- .tile(cubes[[1L]])
    # get raster block
    block <- .raster_file_blocksize(.raster_open_rast(.tile_path(base_tile)))
    # define optimal memsize
    job_block_memsize <- .jobs_block_memsize(
        block_size = .block_size(block = block, overlap = 0L),
        npaths = length(cubes),
        nbytes = 8L,
        proc_bloat = .conf("processing_bloat_cpu")
    )
    # define optimal multicores
    multicores <- .jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # define optimal block size
    block <- .jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = .tile_size(base_tile),
        memsize = memsize,
        multicores = multicores
    )
    # prepare parallel processing once for all tiles
    if (.parallel_start(workers = multicores)) {
        on.exit(.parallel_stop(), add = TRUE)
    }
    # if roi is defined, transform it to sf
    if (.has(roi)) {
        roi <- .roi_as_sf(roi)
    }
    # extract cube labels
    cube_labels <- purrr::map(cubes, .tile_labels)
    # extract cube tiles
    cube_tiles <- .cube_tiles(cubes[[1L]])
    # process each tile
    freq_tbl <- purrr::map_dfr(cube_tiles, function(tile_name) {
        # filter the selected tile in all cubes
        current_tiles <- purrr::map(cubes, function(cube) {
            .tile(.cube_filter_tiles(cube, tile_name))
        })
        # split tile into chunks
        chunks <- .tile_chunks_create(
            tile = current_tiles[[1L]], overlap = 0L, block = block
        )
        # if roi is defined, filter chunks to match it
        if (.has(roi)) {
            chunks <- .chunks_filter_spatial(chunks = chunks, roi = roi)
        }
        # count trajectories within each chunk
        .jobs_map_parallel_dfr(chunks, function(chunk) {
            # count!
            .sankey_chunk_count(
                block = .block(chunk),
                tiles = current_tiles,
                timeline_labels = timeline_labels,
                cube_labels = cube_labels
            )
        }, progress = progress)
    })
    # the same trajectory may appear across tiles and chunks, to reduce
    # memory usage, sum them.
    freq_tbl |>
        dplyr::group_by(dplyr::across(dplyr::all_of(timeline_labels))) |>
        dplyr::summarise(freq = sum(.data[["freq"]]), .groups = "drop")
}

#' @title Count pixel trajectories within a single block
#' @name .sankey_chunk_count
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @param block            The block to read.
#' @param tiles            A list of tiles
#' @param timeline_labels  Character vector naming each step.
#' @param cube_labels      List of cube labels
#' @return A tibble of unique trajectories found in the block and their counts.
.sankey_chunk_count <- function(block, tiles, timeline_labels, cube_labels) {
    # read the class data
    values <- purrr::map(tiles, function(tile) {
        as.integer(.tile_read_block(
            tile = tile,
            band = .tile_bands(tile),
            block = block
        ))
    })
    # transform values in data frame
    values <- as.data.frame(do.call(cbind, values))
    # columns represents each year, label them
    colnames(values) <- timeline_labels
    # drop pixels that have no class in at least one of the steps
    values <- values[stats::complete.cases(values), , drop = FALSE]
    # if there are no valid values, skip block
    if (nrow(values) == 0L) {
        return(tibble::tibble())
    }
    # translate class values to class labels
    for (i in seq_along(timeline_labels)) {
        # get labels
        labels <- cube_labels[[i]]
        # get year
        col <- timeline_labels[[i]]
        # transform value in label
        values[[col]] <- unname(labels[as.character(values[[col]])])
    }
    # count identical trajectories found in this block
    dplyr::count(
        values,
        dplyr::across(dplyr::everything()),
        name = "freq"
    )
}

#' @title Reshape a trajectory table into the long format used for plotting
#' @name .sankey_to_long
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @description
#' \code{ggalluvial} expects one row per (trajectory, step) pair. This pivots
#' the wide trajectory table, one column per step, into that long format,
#' tagging each trajectory with a stable id so its flow can be drawn across
#' the steps.
#'
#' @param data            A trajectory table.
#' @param timeline_labels Character vector naming each step..
#' @return A tibble with trajectory in long format.
.sankey_to_long <- function(data, timeline_labels) {
    # define an id for each trajectory considering
    # each row of the frequency table as a distinct trajectory
    data[["trajectory"]] <- seq_len(nrow(data))
    # transform data to long
    data |>
        tidyr::pivot_longer(
            cols = dplyr::all_of(timeline_labels),
            names_to = "step",
            values_to = "class"
        ) |>
        dplyr::mutate(
            step = factor(.data[["step"]], levels = timeline_labels)
        )
}

#' @title Render a Sankey diagram from a trajectory frequency table
#' @name .sankey_plot
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @keywords internal
#' @noRd
#' @description
#' Draws the class trajectories as a Sankey (alluvial) diagram.
#'
#' @param data             A trajectory table.
#' @param timeline_labels  Character vector naming each step.
#' @param class_labels     Character vector with the class labels to color.
#' @param legend           Named vector associating labels to colors, or NULL.
#' @param palette          A "cols4all" palette for labels without a color.
#' @param title            Plot title.
#' @return A \code{ggplot} object with the Sankey diagram.
.sankey_plot <- function(data, timeline_labels, class_labels, legend, palette,
                         title) {
    # get legend
    legend <- .colors_legend_set(legend)
    # resolve colors
    colors <- .colors_get(
        labels = class_labels,
        legend = legend,
        palette = palette
    )
    # reshape to the long format expected by `ggalluvial`
    data <- .sankey_to_long(data, timeline_labels)
    # build the alluvial diagram
    ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = .data[["step"]],
            y = .data[["freq"]],
            stratum = .data[["class"]],
            alluvium = .data[["trajectory"]],
            fill = .data[["class"]]
        )
    ) +
        ggalluvial::geom_alluvium(alpha = 0.7, decreasing = FALSE) +
        ggalluvial::geom_stratum(decreasing = FALSE) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = title,
            x     = "Step",
            y     = "Frequency",
            fill  = "Class"
        ) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3L))
}
