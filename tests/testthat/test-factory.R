test_that("test factory", {
    sits_naive_bayes <- function(samples = NULL) {
        train_fun <- function(samples) {
            # Data normalization
            ml_stats <- sits_stats(samples)
            train_samples <- sits_predictors(samples)
            train_samples <- sits_pred_normalize(
                pred = train_samples,
                stats = ml_stats
            )
            # call method and return the trained model
            nb_model <- e1071::naiveBayes(
                x = sits_pred_features(train_samples),
                y = as.factor(sits_pred_references(train_samples))
            )
            # construct model predict closure function and returns
            predict_fun <- function(values) {
                # retrieve the prediction (values and probs)
                prediction <- tibble::as_tibble(
                    stats::predict(nb_model,
                                   newdata = values,
                                   type = "raw"
                    )
                )
                return(prediction)
            }
            class(predict_fun) <- c("sits_model", "naiveBayes", class(predict_fun))
            return(predict_fun)
        }
        result <- sits_factory_function(samples, train_fun)
        return(result)
    }
    # create an lda model
    nb_model <- sits_train(samples_modis_ndvi, sits_naive_bayes)
    # classify a point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(
        point_ndvi,
        nb_model,
        multicores = 1,
        progress = FALSE
    )

    expect_true(inherits(nb_model, "function"))
    expect_true(all(unique(point_class$predicted[[1]]$class)
    %in% sits_labels(samples_modis_ndvi)))
    expect_equal(nrow(point_class$predicted[[1]]), 17)

    ml_function <- sits_factory_function(data = NULL, sits_naive_bayes)
    expect_true(inherits(ml_function, "function"))
})
