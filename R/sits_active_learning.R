#' Use the given samples to automatically collect new samples.
#'
#' @param samples_tb      A sits tibble.
#' @param sits_method     A sits model specification.
#' @param data_cube       A sits data cube.
#' @param n_samples       The number of random points to take.
#' @param min_probability The minimum probability for automatically including new samples.
#' @param max_entropy     The maximum entropy for consider a sample for the oracle.
#' @return                A list with two sits tibbles: One for new samples and another for a human expert to review.
#'
#'@examples
#'\dontrun{
#' samples_tb <- sits_select(samples_modis_3bands, bands = c("NDVI", "EVI"))
#' sits_method <- sits_xgboost(verbose = FALSE)
#' data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
#' data_cube <- sits_cube(source = "LOCAL",
#'                        name = "sinop-2014",
#'                        satellite = "TERRA",
#'                        sensor = "MODIS",
#'                        data_dir = data_dir,
#'                        delim = "_",
#'                        parse_info = c("X1", "X2", "tile", "band", "date") )
#' sits_active_learning(samples_tb, sits_method, data_cube)
#'}
#' @export
sits_active_learning <- function(samples_tb, sits_method,
                                 data_cube,
                                 n_samples = 100,
                                 min_probability = 0.95,
                                 max_entropy = 0.1){

    # Get the extent of the data cube.
    xmin <- data_cube[["xmin"]]
    xmax <- data_cube[["xmax"]]
    ymin <- data_cube[["ymin"]]
    ymax <- data_cube[["ymax"]]
    pol <- sf::st_sfc(sf::st_polygon(list(rbind(c(xmin, ymin), c(xmax, ymin),
                                                c(xmax, ymax), c(xmin, ymax),
                                                c(xmin, ymin)))))
    sf::st_crs(pol) <- data_cube[["crs"]]

    # Get n_samples random points in the data_cube's extent.
    points_tb <- sf::st_sf(sf::st_sample(pol,
                                         size = n_samples,
                                         type = "random"))
    points_tb <- sf::st_transform(points_tb,
                                  crs = 4326)
    points_tb <- cbind(points_tb, sf::st_coordinates(points_tb))
    colnames(points_tb) <- c("longitude", "latitude", "geometry")
    sf::st_geometry(points_tb) <- "geometry"
    points_tb <- sf::st_set_geometry(points_tb, NULL)
    points_tb["start_date"]  <- unique(samples_tb[["start_date"]])[[1]]
    points_tb["end_date"]    <- unique(samples_tb[["end_date"]])[[1]]
    points_tb["label"]       <- NA
    points_tb["cube"]        <- NA
    points_tb["time_series"] <- NA
    class(points_tb) <- c(class(points_tb), "sits")

    # Get the points' time series.
    tmp_file <- tempfile(pattern = "points_",
                         fileext = ".csv")
    utils::write.csv(points_tb, file = tmp_file)
    points_tb <- sits_get_data(cube = data_cube,
                               file = tmp_file)

    # Classify the new samples
    my_model <- sits_train(samples_tb, ml_method = sits_method)
    points_tb <- lapply(seq_len(nrow(points_tb)), function(x){
        sits_classify(points_tb[x, ],
                      ml_model = my_model)
    })
    points_tb <- do.call(rbind, points_tb)

    # Get label, probability, and entropy
    prob_entropy <- lapply(seq_len(nrow(points_tb)), function(x){
        pred_df <- points_tb[x, ][["predicted"]][[1]]
        pred_label <- pred_df[["class"]]
        probs <- unlist(pred_df[["probs"]][[1]])
        list(pred_prob = unlist(pred_df[["probs"]][[1]])[pred_label],
             entropy = -1 * sum(probs * log(probs)))
    })
    points_tb["label"] <- unlist(lapply(prob_entropy, function(x){
        names(x[["pred_prob"]])
    }))
    points_tb["label_prob"] <- unlist(lapply(prob_entropy, function(x){
        x[["pred_prob"]]
    }))
    points_tb["entropy"] <- unlist(lapply(prob_entropy, function(x){
        x[["entropy"]]
    }))

    new_samples    <- points_tb[points_tb["label_prob"] > min_probability, ]
    oracle_samples <- points_tb[points_tb["entropy"] < max_entropy, ]
    return(list(new_samples = new_samples,
                oracle_samples = oracle_samples))
}
