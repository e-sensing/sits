#' @title Read data cubes, samples and models shared on HuggingFace
#' @name sits_from_hf
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Reads data shared in a HuggingFace repository. The content
#' of the repository defines what is read:
#'
#' \itemize{
#'   \item{\bold{data cubes}: datasets describing a collection in a
#'   \code{"sits/sits.yml"} file (see \code{\link[sits]{sits_to_hf}}). The
#'   images selected are downloaded to \code{output_dir} and a local data cube
#'   is returned;}
#'
#'   \item{\bold{samples}: time series saved as \code{".rds"} (e.g., by
#'   \code{saveRDS()}) or \code{".parquet"} files (see
#'   \code{\link[sits]{sits_to_parquet}});}
#'
#'   \item{\bold{models}: models trained by \code{\link[sits]{sits_train}}
#'   and saved as \code{".rds"} files.}
#' }
#'
#' When \code{file} is not informed, a repository with a
#' \code{"sits/sits.yml"} file is read as a data cube. Otherwise, the
#' repository must have a single \code{".rds"} or \code{".parquet"} file. 
#' When there are many, choose one using \code{file}.
#'
#' @param repo       HuggingFace repository (\code{"<user>/<repository>"}).
#' @param file       File to be read in the repository (optional).
#' @param ...        Other parameters to read data cubes: \code{labels} and
#'                   \code{version} select results produced by \code{sits}
#'                   (e.g., probabilities).
#' @param type       Type of the repository: \code{"dataset"} (default) or
#'                   \code{"model"}. Data cubes are always shared in datasets.
#' @param bands      Bands to be selected in the data cube (optional).
#' @param tiles      Tiles to be selected in the data cube (optional).
#' @param roi        Region of interest to be selected in the data cube (optional).
#' @param crs        The Coordinate Reference System (CRS) of the roi.
#' @param start_date start date of the data cube
#' @param end_date   end date of the data cube
#' @param output_dir Directory where the images of the data cube are saved
#'                   (required for data cubes).
#' @param n_tries    Number of attempts to download the same image.
#' @param multicores Number of cores for parallel downloading.
#' @param progress   Logical: show progress bar?
#'
#' @return A data cube with local images, a set of samples (tibble of class
#' "sits"), or a model (class "sits_model").
#'
#' @note
#' HuggingFace limits the number of files a user requests in windows of
#' five minutes. Reading the images of a remote data cube requests the same
#' images many times, which quickly consumes these limits. For this reason,
#' \code{sits_from_hf} downloads the images of the data cube, requesting each
#' one of them only once, in batches that fit the requests available. When no
#' request is available, the download waits the next window. Images already
#' downloaded in \code{output_dir} are not requested again, so an interrupted
#' download is resumed by calling \code{sits_from_hf} again.
#'
#' To access private repositories, the HuggingFace access token must be set in the
#' \code{HF_TOKEN} environment variable.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # download a data cube shared in a HuggingFace dataset
#'     cube <- sits_from_hf(
#'         repo = "felipemcarlos/sits_mod13q1_sinop",
#'         output_dir = tempdir()
#'     )
#' }
#'
#' @family data conversion
#' @export
sits_from_hf <- function(repo,
                         file = NULL, ...,
                         type = "dataset",
                         bands = NULL,
                         tiles = NULL,
                         roi = NULL,
                         crs = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         output_dir = NULL,
                         n_tries = 3L,
                         multicores = 2L,
                         progress = TRUE) {
    # set caller
    .check_set_caller("sits_from_hf")
    # pre-condition - parameters must be valid
    .check_chr_parameter(repo, len_max = 1L)
    .check_chr_parameter(file, len_max = 1L, allow_null = TRUE)
    .check_chr_within(
        type,
        within = names(.conf("hf", "repo_types")),
        discriminator = "one_of"
    )
    # select the file read from the repository
    file <- .hf_repo_file(repo = repo, file = file, type = type)
    # identify the content of the repository by the file read
    repo <- .hf_repo_new(repo = repo, file = file)
    # read content!
    .hf_repo_read(
        repo = repo,
        file = file,
        type = type, ...,
        bands = bands,
        tiles = tiles,
        roi = roi,
        crs = crs,
        start_date = start_date,
        end_date = end_date,
        output_dir = output_dir,
        n_tries = n_tries,
        multicores = multicores,
        progress = progress
    )
}
