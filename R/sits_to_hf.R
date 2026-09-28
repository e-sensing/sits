#' @title Describe a data cube as a HuggingFace dataset
#' @name sits_to_hf
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Data cubes shared on HuggingFace are described by a
#' \code{"sits.yml"} file, placed in the root of the dataset repository, which
#' \code{sits} reads to open the dataset.
#'
#' This function writes that description from a data cube: the bands, their
#' resolution and, for results produced by \code{sits} (e.g., probabilities,
#' classified maps), the labels of the classification.
#'
#' Each type of cube is described the way \code{sits} reads it:
#' \itemize{
#'   \item{image raster cubes are described band by band;}
#'
#'   \item{embeddings have only base satellite and sensor described. Bands are
#'   not included as \code{sits} manages how embeddings are managed;}
#'
#'   \item{results produced by \code{sits} inform only their resolution and
#'   the labels of the classification.}
#' }
#'
#' A dataset can share more than one result of the same classification (e.g.,
#' probabilities and the classified map), which are described together when
#' they are informed as a list of cubes.
#'
#' Loading images available in a HuggingFace dataset as a data cube, sometimes
#' requires \code{sits} to read many files. To avoid users to do so every time,
#' you can share a cache cube in a file named (\code{"cache.rds"}). When it is
#' available in a HuggingFace dataset \code{sits} loads it instead of read
#' the image files.
#'
#' @param cube Data cube or list of cubes to be shared as a dataset.
#' @param file Full path of the description file to be written. It must have
#'             a valid name with extension \code{".yml"}. We recommend name the
#'             file as \code{"sits.yml"} as this is the name it must have in
#'             HuggingFace.
#' @param cache Full path of the cache file to write the data cube shared in the
#'              dataset. It must have a valid name with extension \code{".rds"}.
#'              We recommend name the file as \code{"cache.rds"} as this is the
#'              name it must have in HuggingFace.
#' @param repo HuggingFace repository where the cube files are stored
#'             (\code{"<user>/<dataset>"}).
#'
#' @return Collection definition of the dataset.
#'
#' @note Images must be named in the dataset as \code{sits} names them
#' (\code{sits_cube_copy} and \code{sits_regularize} do it), and only the
#' bands shared in the dataset must be described.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a cube from the images distributed with sits
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source     = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir   = data_dir
#'     )
#'     # describe the cube as a HuggingFace dataset
#'     sits_to_hf(cube, file = paste0(tempdir(), "/sits.yml"))
#'     # share the cube of the dataset, so sits reads it instead of
#'     # describing the images of the dataset one by one
#'     sits_to_hf(
#'         cube,
#'         file  = paste0(tempdir(), "/sits.yml"),
#'         cache = paste0(tempdir(), "/cache.rds"),
#'         repo  = "user/dataset"
#'     )
#' }
#'
#' @family data conversion
#' @export
sits_to_hf <- function(cube, file = NULL, cache = NULL, repo = NULL) {
    # set caller
    .check_set_caller("sits_to_hf")
    # describe cube!
    .hf_conf_dataset(cube, file, cache, repo)
}

#' @title Describe a collection of sits as a HuggingFace dataset
#' @name sits_config_to_hf
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Data cubes shared on HuggingFace are described by a
#' \code{"sits.yml"} file, placed in the root of the dataset repository, which
#' \code{sits} reads to open the dataset.
#'
#' This function writes that description from a collection registered in
#' \code{sits} (see \code{\link{sits_list_collections}}), which is useful when
#' a dataset shares images of a known collection (e.g., images of
#' \code{"SENTINEL-2-L2A"} downloaded from \code{"MPC"}).
#'
#' The description of a collection is written as the dataset needs it: the
#' bands are named as \code{sits} names them in the images, and the properties
#' used by \code{sits} to reach the provider are not written, as \code{sits}
#' defines them when it registers a dataset.
#'
#' @param source Data source.
#' @param collection Image collection.
#' @param bands Bands shared in the dataset (default is all of them).
#' @param file Full path of the description file to be written. It must have
#'             a valid name with extension \code{".yml"}. We recommend name the
#'             file as \code{"sits.yml"} as this is the name it must have in
#'             HuggingFace.
#'
#' @return Collection definition of the dataset.
#'
#' @note A dataset must describe only the bands it holds, and the resolution
#' of each band is the one the collection declares. If you need to describe
#' a regularized cube, or cube with custom bands, use \code{\link{sits_to_hf}}.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # describe a collection as a HuggingFace dataset
#'     sits_config_to_hf(
#'         source     = "MPC",
#'         collection = "SENTINEL-2-L2A",
#'         bands      = c("B02", "B03", "B04", "B08"),
#'         file       = paste0(tempdir(), "/sits.yml")
#'     )
#' }
#'
#' @family data conversion
#' @export
sits_config_to_hf <- function(source, collection, bands = NULL, file = NULL) {
    # set caller
    .check_set_caller("sits_config_to_hf")
    # sources and collections are registered in upper case
    source <- toupper(source)
    collection <- toupper(collection)
    # the collection must be registered in sits
    .check_chr_within(x = source, within = .sources())
    .check_chr_within(
        x = collection, within = .source_collections(source = source)
    )
    # write collection configuration
    .hf_conf_write(
        collection_conf = .hf_conf_source(source, collection, bands),
        file = file,
        origin = paste(source, collection)
    )
}
