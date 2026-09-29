#' @title Describe a data cube as a HuggingFace dataset
#' @name sits_to_hf
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Data cubes shared on HuggingFace are described by files
#' \code{sits} reads to understand the dataset.  They are kept in a \code{"sits"} 
#' directory of the repository, so they are not mixed with the other files of 
#' the dataset:
#'
#' \itemize{
#'   \item{\code{"sits/sits.yml"}: the collection definition of the dataset;}
#'
#'   \item{\code{"sits/cache.rds"}: the data cube of the dataset. When it is
#'   available, \code{sits} loads it instead of reading each image of the
#'   dataset.}
#' }
#'
#' This function writes these files in \code{output_dir}, as they must be
#' uploaded to the repository (i.e., the \code{"sits"} directory and the
#' images, named as \code{sits} names them, in the root of the repository).
#'
#' The collection definition is written from a data cube: the bands, their
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
#' they are informed as a list of cubes. A cache describes a single cube, so it
#' is not written for a list of cubes.
#'
#' @param cube Data cube or list of cubes to be shared as a dataset.
#' @param output_dir Directory where the files of the dataset are written
#'                   (optional). When not informed, the collection definition
#'                   is only returned.
#' @param repo HuggingFace repository where the images are uploaded
#'             (\code{"<user>/<dataset>"}). Required to write the cache of a
#'             data cube, unless the cube was read from HuggingFace.
#'
#' @return Collection definition of the dataset.
#'
#' @note Images must be named in the dataset as \code{sits} names them
#' (\code{sits_cube_copy} and \code{sits_regularize} do it), and only the
#' bands shared in the dataset must be described.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # create a cube
#'     data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#'     cube <- sits_cube(
#'         source     = "BDC",
#'         collection = "MOD13Q1-6.1",
#'         data_dir   = data_dir
#'     )
#'
#'     # get the collection definition of the cube
#'     collection_conf <- sits_to_hf(cube)
#'     
#'     # write the files describing the dataset ("sits/sits.yml" and "sits/cache.rds"), 
#'     # to be uploaded with the images of the cube
#'     sits_to_hf(
#'         cube,
#'         output_dir = tempdir(),
#'         repo       = "user/dataset"
#'     )
#' }
#'
#' @seealso \code{\link[sits]{sits_from_hf}}
#' @family data conversion
#' @export
sits_to_hf <- function(cube, output_dir = NULL, repo = NULL) {
    # set caller
    .check_set_caller("sits_to_hf")
    # pre-condition - output directory must be a valid path
    if (.has(output_dir)) {
        output_dir <- .file_path_expand(output_dir)
        .check_output_dir(output_dir)
    }
    # pre-condition - repository must be a valid character string
    .check_chr_parameter(repo, len_max = 1L, allow_null = TRUE)
    # describe cube!
    .hf_conf_dataset(cube, output_dir, repo)
}

#' @title Describe a collection of sits as a HuggingFace dataset
#' @name sits_config_to_hf
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Data cubes shared on HuggingFace are described by a
#' \code{"sits/sits.yml"} file in the dataset repository, which \code{sits}
#' reads to open the dataset.
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
#' @param output_dir Directory where the file \code{"sits/sits.yml"} is
#'                   written (optional). When not informed, the collection
#'                   definition is only returned.
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
#'         output_dir = tempdir()
#'     )
#' }
#'
#' @family data conversion
#' @export
sits_config_to_hf <- function(source, collection, bands = NULL,
                              output_dir = NULL) {
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
    # check output directory (optional)
    if (.has(output_dir)) {
        output_dir <- .file_path_expand(output_dir)
        .check_output_dir(output_dir)
    }
    # write collection configuration
    .hf_conf_write(
        collection_conf = .hf_conf_source(source, collection, bands),
        output_dir = output_dir,
        origin = paste(source, collection)
    )
}
