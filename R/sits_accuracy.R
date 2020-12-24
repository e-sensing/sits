#' @title Area-weighted classification accuracy assessment
#' @name sits_accuracy
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description To use this function the input table should be
#' a set of results containing
#' both the label assigned by the user and the classification result.
#' Accuracy assessment set us a confusion matrix to determine the accuracy
#' of your classified result.
#' This function uses an area-weighted technique proposed by Olofsson et al. to
#' produce more reliable accuracy estimates at 95% confidence level.
#'
#' This function performs an accuracy assessment of the classified, including
#' Overall Accuracy, User's Accuracy, Producer's Accuracy
#' and error matrix (confusion matrix) according to [1-3].
#'
#' @references
#' [1] Olofsson, P., Foody, G.M., Stehman, S.V., Woodcock, C.E. (2013).
#' Making better use of accuracy data in land change studies: Estimating
#' accuracy and area and quantifying uncertainty using stratified estimation.
#' Remote Sensing of Environment, 129, pp.122-131.
#'
#' @references
#' [2] Olofsson, P., Foody G.M., Herold M., Stehman, S.V.,
#' Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change.
#' Remote Sensing of Environment, 148, pp. 42-57.
#'
#' @references
#' [3] FAO, Map Accuracy Assessment and Area Estimation: A Practical Guide.
#' National forest monitoring assessment working paper No.46/E, 2016.
#'
#'
#' @param label_cube       A tibble with metadata about the classified maps.
#' @param validation_csv   A CSV file path with validation data
#'
#' @return
#' A list of lists: The error_matrix, the class_areas, the unbiased
#' estimated areas, the standard error areas, confidence interval 95% areas,
#' and the accuracy (user, producer, and overall), or NULL if the data is empty.
#'
#' @examples
#' \dontrun{
#' # get the samples for Mato Grosso for bands NDVI and EVI
#' samples_mt_ndvi <- sits_select(samples_mt_4bands, bands = c("NDVI"))
#' # filter the samples for three classes (to simplify the example)
#' samples_mt_ndvi <- dplyr::filter(samples_mt_ndvi, label %in%
#'   c("Forest", "Pasture", "Soy_Corn"))
#' # build an XGB model
#' xgb_model <- sits_train(
#'   samples_mt_ndvi,
#'   sits_xgboost(nrounds = 10, verbose = FALSE)
#' )
#'
#' # files that make up the data cube
#' ndvi_file <- c(system.file("extdata/raster/mod13q1/sinop-ndvi-2014.tif",
#'   package = "sits"
#' ))
#' # create the data cube
#' sinop_2014 <- sits_cube(
#'     type = "BRICK",
#'     name = "sinop-2014",
#'     timeline = timeline_2013_2014,
#'     satellite = "TERRA",
#'     sensor = "MODIS",
#'     bands = c("NDVI"),
#'     files = c(ndvi_file)
#' )
#'
#' # classify the data cube with xgb model
#' sinop_2014_probs <- sits_classify(sinop_2014,
#'   xgb_model,
#'   output_dir = tempdir(),
#'   memsize = 4,
#'   multicores = 1
#' )
#' # label the classification
#' sinop_2014_label <- sits_label_classification(sinop_2014_probs,
#'   output_dir = tempdir()
#' )
#' # get ground truth points
#' ground_truth <- system.file("extdata/samples/samples_sinop_crop.csv",
#'   package = "sits"
#' )
#' # calculate accuracy according to Olofsson's method
#' accuracy <- suppressWarnings(sits_accuracy(sinop_2014_label, ground_truth))
#' }
#' @export
sits_accuracy <- function(label_cube, validation_csv) {
    assertthat::assert_that("classified_image" %in% class(label_cube),
        msg = "sits_accuracy requires a labelled cube"
    )
    assertthat::assert_that(file.exists(validation_csv),
        msg = "sits_accuracy: validation file missing."
    )

    # read sample information from CSV file and put it in a tibble
    csv_tb <- tibble::as_tibble(utils::read.csv(validation_csv))

    # Precondition - check if CSV file is correct
    .sits_csv_check(csv_tb)

    # find the labels of the cube
    labels_cube <- sits_labels(label_cube)

    # get xy in cube projection
    xy_tb <- .sits_latlong_to_proj(
        longitude = csv_tb$longitude,
        latitude = csv_tb$latitude,
        crs = label_cube$crs
    )

    # join lat-long with XY values in a single tibble
    points <- dplyr::bind_cols(csv_tb, xy_tb)

    # are there points to be retrieved from the cube?
    assertthat::assert_that(nrow(points) != 0,
        msg = "No validation point intersects the map's spatiotemporal extent."
    )

    # the label cube may contain several classified images
    pred_ref_lst <- slider::slide(label_cube, function(row) {

        # find the labelled band
        labelled_band <- sits_bands(row)
        # the labelled band must be unique
        assertthat::assert_that(length(labelled_band) == 1,
            msg = "sits_accuracy: invalid labelled cube"
        )

        # filter the points inside the data cube
        points_row <- dplyr::filter(
            points,
            X > row$xmin & X < row$xmax &
                Y > row$ymin & Y < row$ymax,
            start_date == row$file_info[[1]]$date
        )

        # if there are no points in the cube, return an empty list
        if (nrow(points_row) < 1) {
            return(NULL)
        }

        # convert the tibble to a matrix
        xy <- matrix(c(points_row$X, points_row$Y),
                     nrow = nrow(points_row), ncol = 2)
        colnames(xy) <- c("X", "Y")

        # extract values from cube
        values <- .sits_raster_api_extract(
            cube = row,
            band_cube = labelled_band,
            xy = xy
        )
        # convert to a vector
        val <- dplyr::pull(values[, 1])
        # get the predicted values
        predicted <- labels_cube[val]
        # Get reference classes
        reference <- points_row$label
        # do the number of predicted and reference values match
        assertthat::assert_that(length(reference) == length(predicted),
            msg = "sits_accuracy: predicted and reference vector do not match"
        )
        # create a tibble to store the results
        tb <- tibble::tibble(predicted = predicted, reference = reference)
        # return the list
        return(tb)
    })
    # retrieve the predicted and reference vectors for all rows of the cube
    pred_ref <- do.call(rbind, pred_ref_lst)

    # Create the error matrix
    error_matrix <- table(
        factor(pred_ref$predicted,
            levels = labels_cube,
            labels = labels_cube
        ),
        factor(pred_ref$reference,
            levels = labels_cube,
            labels = labels_cube
        )
    )

    # Get area for each class for each row of the cube
    freq_lst <- slider::slide(label_cube, function(row) {

        # get the frequency count and value for each labelled image
        freq <- .sits_raster_api_area_freq(row)
        # include class names
        freq <- dplyr::mutate(freq, class = labels_cube[freq$value])
        return(freq)
    })
    # get a tibble by binding the row (duplicated labels with different counts)
    freq <- do.call(rbind, freq_lst)
    # summarize the counts for each label
    freq <- freq %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(count = sum(count))

    # area is taken as the sum of pixels
    area <- freq$count
    # names of area are the classes
    names(area) <- freq$class
    # NAs are set to 0
    area[is.na(area)] <- 0

    # Compute accuracy metrics
    assess <- .sits_assess_accuracy_area(error_matrix, area)

    # Print assessment values
    tb <- t(dplyr::bind_rows(assess$accuracy$user, assess$accuracy$producer))
    colnames(tb) <- c("User", "Producer")

    print(knitr::kable(tb,
        digits = 2,
        caption = "Users and Producers Accuracy per Class"
    ))

    # print overall accuracy
    print(paste0("\nOverall accuracy is ", assess$accuracy$overall))

    return(assess)
}


#' @title Support for Area-weighted post-classification accuracy
#' @name .sits_assess_accuracy_area
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @param error_matrix A matrix given in sample counts.
#'                     Columns represent the reference data and
#'                     rows the results of the classification
#' @param area         A named vector of the total area of each class on
#'                     the map
#'
#' @references
#' Olofsson, P., Foody G.M., Herold M., Stehman, S.V.,
#' Woodcock, C.E., Wulder, M.A. (2014)
#' Good practices for estimating area and assessing accuracy of land change.
#' Remote Sensing of Environment, 148, pp. 42-57.
#'
#' @return
#' A list of lists: The error_matrix, the class_areas, the unbiased
#' estimated areas, the standard error areas, confidence interval 95% areas,
#' and the accuracy (user, producer, and overall).

.sits_assess_accuracy_area <- function(error_matrix, area) {
  if (any(dim(error_matrix) == 0)) {
    stop("Invalid dimensions in error matrix.", call. = FALSE)
  }
  if (length(unique(dim(error_matrix))) != 1) {
    stop("The error matrix is not square.", call. = FALSE)
  }
  if (!all(colnames(error_matrix) == rownames(error_matrix))) {
    stop("Labels mismatch in error matrix.", call. = FALSE)
  }
  if (unique(dim(error_matrix)) != length(area)) {
    stop("Mismatch between error matrix and area vector.",
      call. = FALSE
    )
  }
  if (!all(names(area) %in% colnames(error_matrix))) {
    stop("Label mismatch between error matrix and area vector.",
      call. = FALSE
    )
  }

  # Reorder the area based on the error matrix
  area <- area[colnames(error_matrix)]
  #
  weight <- area / sum(area)
  class_areas <- rowSums(error_matrix)

  # proportion of area derived from the reference classification
  # weighted by the area of the classes
  # cf equation (1) of Olofsson et al (2014)
  prop <- weight * error_matrix / class_areas
  prop[is.na(prop)] <- 0

  # An unbiased estimator of the total area
  # based on the reference classification
  # cf equation (2) of Olofsson et al (2014)
  error_adjusted_area <- colSums(prop) * sum(area)

  # Estimated standard error of the estimated area proportion
  # cf equation (3) of Olofsson et al (2014)
  stderr_prop <- sqrt(colSums((weight * prop - prop**2) / (class_areas - 1)))

  # Standard error of the error-adjusted estimated area
  # cf equation (4) of Olofsson et al (2014)
  stderr_area <- sum(area) * stderr_prop

  # area-weighted user's accuracy
  # cf equation (6) of Olofsson et al (2014)
  user_acc <- diag(prop) / rowSums(prop)

  # area-weigthed producer's accuracy
  # cf equation (7) of Olofsson et al (2014)
  prod_acc <- diag(prop) / colSums(prop)

  # overall area-weighted accuracy
  over_acc <- sum(diag(prop))


  return(
    list(
      error_matrix = error_matrix,
      area_pixels = area,
      error_ajusted_area = error_adjusted_area,
      stderr_prop = stderr_prop,
      stderr_area = stderr_area,
      conf_interval = 1.96 * stderr_area,
      accuracy = list(user = user_acc, producer = prod_acc, overall = over_acc)
    )
  )
}
