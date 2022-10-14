#---- chunks API: ----

#' Chunks API
#'
#' A chunk is a tibble of rectangular regions defining a matrix and
#' its corresponding geographical area. So, each  region contains a
#' \code{block} and a \code{bbox} information. chunks can be used to access
#' specific raster image regions and optimize memory usage.
#'
#' Generally, chunks are created from an actual image that is divided
#' into small blocks. The chunks also provide overlapping support, that is,
#' chunks that intersects its neighbors by some amount of pixels.
#'
#' @param block A \code{block} to represent the common chunk size.
#' @param overlap An integer informing overlapping size in pixels.
#' @param image_size A \code{block} informing original image's matrix size.
#' @param image_bbox A \code{bbox} informing original image bbox.
#' @param chunks A \code{chunk}.
#'
#' @examples
#' if (sits_run_examples()) {
#' chunks <- .chunks_create(
#'   block = c(ncols = 512, nrows = 512),
#'   overlap = 2,
#'   image_size = c(ncols = 4000, nrows = 4000),
#'   image_bbox = c(xmin = 1, xmax = 2, ymin = 3, ymax = 4, crs = 4326)
#' )
#' # remove overlaps from chunks
#' cropped <- .chunks_no_overlap(chunks)
#' # removing overlaps from a non overlapped chunks produces identical bbox
#' identical(.bbox(cropped), .bbox(.chunks_no_overlap(cropped)))
#' # blocks from 'cropped' can be used to remove any overlap from rasters
#' # produced from 'chunks'.
#' .chunks_filter_spatial(
#'   chunks = chunks,
#'   roi = c(lon_min = 1.3, lon_max = 1.7, lat_min = 3.3, lat_max = 3.7)
#' )
#' }
#'
#' @seealso \link{chunk_accessors}
#' @family region objects API
#' @keywords internal
#' @name chunks_api
#' @noRd
NULL

#' @describeIn chunks_api Creates a tibble of chunks with the same size as
#'   \code{block} and additional \code{overlap}.
#' @returns \code{.chunks_create()}: \code{chunks} tibble.
#' @noRd
.chunks_create <- function(block, overlap, image_size, image_bbox) {
    # Generate all starting block points (col, row)
    chunks <- purrr::cross_df(list(
        col = seq(1, .ncols(image_size), .ncols(block)),
        row = seq(1, .nrows(image_size), .nrows(block))
    ))
    # Adjust col and row to do overlap
    chunks[["col"]] <- .as_int(pmax(1, .col(chunks) - overlap))
    chunks[["row"]] <- .as_int(pmax(1, .row(chunks) - overlap))
    # Adjust ncols and nrows to do overlap
    chunks[["ncols"]] <-
        .as_int(pmin(.ncols(image_size), .col(chunks) + .ncols(block) +
                         overlap - 1) - .col(chunks) + 1)
    chunks[["nrows"]] <-
        .as_int(pmin(.nrows(image_size), .row(chunks) + .nrows(block) +
                         overlap - 1) - .row(chunks) + 1)
    # Chunk of entire image
    entire_image <- c(image_size, image_bbox)
    # Prepare a raster as template to crop bbox
    t_obj <- .chunks_as_raster(chunk = entire_image, nlayers = 1)
    # Generate chunks' bbox
    chunks <- slider::slide_dfr(chunks, function(chunk) {
        # Crop block from template
        r_obj <- .raster_crop_metadata(r_obj = t_obj, block = .block(chunk))
        # Add bbox information
        .xmin(chunk) <- .raster_xmin(r_obj = r_obj)
        .xmax(chunk) <- .raster_xmax(r_obj = r_obj)
        .ymin(chunk) <- .raster_ymin(r_obj = r_obj)
        .ymax(chunk) <- .raster_ymax(r_obj = r_obj)
        .crs(chunk) <- .raster_crs(r_obj = r_obj)
        chunk
    })
    # Overlapping support
    chunks[["overlap"]] <- .as_int(overlap)
    # Chunk size without overlap
    chunks[["crop_ncols"]] <- .as_int(pmin(
        .ncols(image_size) - .col(chunks) + 1, .ncols(block)
    ))
    chunks[["crop_nrows"]] <- .as_int(pmin(
        .nrows(image_size) - .row(chunks) + 1, .nrows(block)
    ))
    # Return chunks
    chunks
}

#' @describeIn chunks_api Creates an empty \code{raster} object based on the
#'   first chunk passed in \code{chunk} parameter.
#' @returns \code{raster} object.
#' @noRd
.chunks_as_raster <- function(chunk, nlayers) {
    .raster_new_rast(
        nrows = .nrows(chunk)[[1]],
        ncols = .ncols(chunk)[[1]],
        xmin = .xmin(chunk)[[1]],
        xmax = .xmax(chunk)[[1]],
        ymin = .ymin(chunk)[[1]],
        ymax = .ymax(chunk)[[1]],
        nlayers = nlayers,
        crs = .crs(chunk)[[1]]
    )
}

#' @describeIn chunks_api Creates a \code{chunk} that can be used to
#'   remove overlaps.
#' @returns \code{.chunks_no_overlap()}: \code{chunks} tibble.
#' @noRd
.chunks_no_overlap <- function(chunks) {
    # Generate blocks
    cropped <- tibble::tibble(
        col = .as_int(pmin(chunks[["overlap"]] + 1, .col(chunks))),
        row = .as_int(pmin(chunks[["overlap"]] + 1, .row(chunks)))
    )
    # Adjust blocks size
    .ncols(cropped) <- pmin(
        .ncols(chunks) - .col(cropped) + 1, .as_int(chunks[["crop_ncols"]])
    )
    .nrows(cropped) <- pmin(
        .nrows(chunks) - .row(cropped) + 1, .as_int(chunks[["crop_nrows"]])
    )
    # Generate bbox for each chunk
    cropped <- slider::slide2_dfr(chunks, cropped, function(chunk, crop) {
        # Prepare a raster as template to crop bbox
        t_obj <- .chunks_as_raster(chunk = chunk, nlayers = 1)
        # Crop block from template
        r_obj <- .raster_crop_metadata(r_obj = t_obj, block = .block(crop))
        # Add bbox information
        .xmin(crop) <- .raster_xmin(r_obj = r_obj)
        .xmax(crop) <- .raster_xmax(r_obj = r_obj)
        .ymin(crop) <- .raster_ymin(r_obj = r_obj)
        .ymax(crop) <- .raster_ymax(r_obj = r_obj)
        .crs(crop) <- .raster_crs(r_obj = r_obj)
        crop
    })
    # Finish cropped chunks
    cropped[["overlap"]] <- 0
    cropped[["crop_ncols"]] <- chunks[["crop_ncols"]]
    cropped[["crop_nrows"]] <- chunks[["crop_nrows"]]
    # Return cropped chunks
    cropped
}

#' @describeIn chunks_api Filter \code{chunks} that intersects a given
#'   \code{roi}.
#' @returns \code{.chunks_filter_spatial()}: \code{chunks} tibble.
#' @noRd
.chunks_filter_spatial <- function(chunks, roi) {
    chunks[.intersects(.bbox_as_sf(.bbox(chunks)), .roi_as_sf(roi)), ]
}
