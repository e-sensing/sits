
# Chunks API
#
#  A chunk is a tibble of rectangular regions defining a matrix and
#  its corresponding geographical area. So, each  region contains a
#  \code{block} and a \code{bbox} information. chunks can be used to access
#  specific raster image regions and optimize memory usage.
#
#  Generally, chunks are created from an actual image that is divided
#  into small blocks. The chunks also provide overlapping support, that is,
#  chunks that intersects its neighbors by some amount of pixels.
#
#' @name .chunks_create
#' @noRd
#' @param block        A block  to represent the common chunk size.
#' @param overlap      Ooverlap size in pixels.
#' @param image_size   Original image's matrix size.
#' @param image_bbox   Original image bbox.
#' @return             A data frame with chunks
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

#' @title Create a raster based on a chunk
#' @name .chunks_as_raster
#' @noRd
#' @param chunk    A data frame with chunks
#' @param nlayers  number of layers in the raster
#' @return raster object.
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

#' @title Creates a chunk to remove overlaps
#' @name .chunks_no_overlap
#' @noRd
#' @param chunks  A data frame with chunks
#' @return        A data frame with chunks without overlap
#'
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

#' @title Filter chunks that intersects a given roi
#' @name .chunks_filter_spatial
#' @noRd
#' @param chunks A data frame with chunks
#' @param roi    Region of interest
#' @return       A data frame with filtered chunks
#'
.chunks_filter_spatial <- function(chunks, roi) {
    chunks[.intersects(.bbox_as_sf(chunks), .roi_as_sf(roi)), ]
}

# Chunk accessors
#
# These functions are read-only accessors of chunk fields
#
#  .xres() and .yres()} computes, respectively, \code{"xres"} and
#  \code{"yres"} values from chunk fields. The values are computed as
#  \itemize{
#  \item \eqn{xres=(xmax - xmin)/ncols}
#  \item \eqn{yres=(ymax - ymin)/nrows}
#
#
.xres <- function(x) {
    (.xmax(x) - .xmin(x)) / .ncols(x)
}

#' @noRd
.yres <- function(x) {
    (.ymax(x) - .ymin(x)) / .nrows(x)
}

