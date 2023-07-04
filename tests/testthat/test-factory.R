test_that("test factory", {
    sits_mlr <- function(samples = NULL, formula = sits_formula_linear(),
                         n_weights = 20000, maxit = 2000) {
        train_fun <- function(samples) {
            # Data normalization
            ml_stats <- sits_stats(samples)
            train_samples <- sits_predictors(samples)
            train_samples <- sits_pred_normalize(
                pred = train_samples,
                stats = ml_stats
            )
            formula <- formula(train_samples[, -1])
            # call method and return the trained model
            result_mlr <- nnet::multinom(
                formula = formula,
                data = train_samples,
                maxit = maxit,
                MaxNWts = n_weights,
                trace = FALSE,
                na.action = stats::na.fail
            )

            # construct model predict closure function and returns
            predict_fun <- function(values) {
                # retrieve the prediction (values and probs)
                prediction <- tibble::as_tibble(
                    stats::predict(result_mlr, newdata = values, type = "probs")
                )
                return(prediction)
            }
            class(predict_fun) <- c("sits_model", class(predict_fun))
            return(predict_fun)
        }
        result <- sits_factory_function(samples, train_fun)
        return(result)
    }
    # create an lda model
    mlr_model <- sits_train(samples_modis_ndvi, sits_mlr)
    # classify a point
    point_ndvi <- sits_select(point_mt_6bands, bands = "NDVI")
    point_class <- sits_classify(point_ndvi,
        mlr_model,
        multicores = 1,
        progress = FALSE
    )

    expect_true(inherits(mlr_model, "function"))
    expect_true(all(unique(point_class$predicted[[1]]$class)
    %in% sits_labels(samples_modis_ndvi)))
    expect_equal(nrow(point_class$predicted[[1]]), 17)

    ml_function <- sits_factory_function(data = NULL, sits_mlr)
    expect_true(inherits(ml_function, "function"))
})
