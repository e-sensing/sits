#' @title Produce a list of best features for spatiotemporal classification
#'        for a Random Forest model
#' @name sits_rfor_feature_selection
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use Random Forest algorithm to classify samples.
#' This function is a front-end to the \code{randomForest} package.
#' Please refer to the documentation in that package for more details.
#'
#' @param samples    Time series with the training samples
#'                   (tibble of class "sits").
#' @param ml_method  Machine learning method (default = RandomForest)
#' @param band       Bands to evaluate (NULL = all bands)
#' @param num_trees  Number of trees
#' @param mtry       Number of variables randomly sampled as candidates at
#'                   each split
#' @param metric     Importance metric (default = "mda")
#' @param n_iter     Number of iterations to create the output.
#' @param acc_loss   Maximum accuracy loss

#' @return           List of selected features (band/date combination)
#'
#'
#' @examples
#' if (sits_run_examples()) {
#'
#' }
#' @export
#'

# sits_rfor_feature_selection <- function(
#                           samples,
#                           num_trees = 100,
#                           mtry = NULL,
#                           bands = NULL,
#                           importance_metric = "mda",
#                           n_iter = 20,
#                           accuracy_loss = 0.02) {
#
#     # select bands if required
#     if (.has(bands))
#         samples <- .select_sits(samples = samples, bands = bands)
#
#     # Checks 'num_trees'
#     .check_int_parameter(num_trees, min = 20L)
#     # Get labels (used later to ensure column order in result matrix)
#     labels <- .samples_labels(samples)
#     # Get predictors features
#     train_samples <- .predictors(samples)
#     # Post condition: is predictor data valid?
#     .check_predictors(pred = train_samples, samples = samples)
#     # determine number of random forest
#     n_features <- ncol(train_samples) - 2L
#     # Apply the 'mtry' default value of 'randomForest' package
#     if (.has(mtry)) {
#         # Checks 'mtry'
#         .check_int_parameter(mtry, min = 1L, max = n_features)
#     } else {
#         # set the default values of `mtry`
#         mtry <- floor(sqrt(n_features))
#     }
#
#     # Train a random forest model
#     model <- randomForest::randomForest(
#         x = .pred_features(train_samples),
#         y = as.factor(.pred_references(train_samples)),
#         samples = NULL, ntree = num_trees, mtry = mtry,
#         nodesize = 1L, localImp = TRUE, norm.votes = FALSE, ...,
#         na.action = stats::na.fail
#     )
#     importance_df <- randomForest::importance(model)
#     importance_df
#
# }
# # Random-Forest feature selection with:
# # - accuracy loss tolerance = 0.02
# # - max iterations = 20
# # - selectable importance measure (e.g., "gini", "permutation", "accuracy")
# #
# # Works for classification. Uses CV accuracy as the objective.
# #
# # Packages
# suppressPackageStartupMessages({
#     library(randomForest)
#     library(caret)
# })
#
# #-----------------------------
# # User parameters
# #-----------------------------
# set.seed(123)
#
# accuracy_loss_tol <- 0.02
# max_iter          <- 20
# ntree             <- 500
# cv_folds          <- 5
#
# # Choose importance measure:
# #  - "gini"        : MeanDecreaseGini (classification only)
# #  - "accuracy"    : MeanDecreaseAccuracy (permutation importance; classification)
# #  - "permutation" : caret::varImp on RF, usually based on OOB / permutation-style importance
# importance_measure <- "gini"  # <- change this
#
# #-----------------------------
# # Example data (replace with yours)
# #-----------------------------
# data(iris)
# x <- iris[, -5]
# y <- iris$Species
#
# #-----------------------------
# # Helper: cross-validated accuracy for a given feature set
# #-----------------------------
# cv_accuracy <- function(x_df, y_vec, ntree, cv_folds) {
#     ctrl <- trainControl(method = "cv", number = cv_folds)
#     m <- train(
#         x = x_df,
#         y = y_vec,
#         method = "rf",
#         trControl = ctrl,
#         ntree = ntree
#     )
#     max(m$results$Accuracy)
# }
#
# #-----------------------------
# # Helper: importance extraction (returns named numeric vector)
# #-----------------------------
# get_importance <- function(rf_fit, x_df, y_vec, measure) {
#     measure <- match.arg(measure, c("gini", "accuracy", "permutation"))
#
#     if (measure %in% c("gini", "accuracy")) {
#         # randomForest::importance expects importance=TRUE in the model
#         imp_mat <- randomForest::importance(rf_fit)
#
#         col_needed <- if (measure == "gini") "MeanDecreaseGini" else "MeanDecreaseAccuracy"
#         if (!col_needed %in% colnames(imp_mat)) {
#             stop(sprintf(
#                 "Importance column '%s' not found. Available: %s",
#                 col_needed, paste(colnames(imp_mat), collapse = ", ")
#             ))
#         }
#         out <- imp_mat[, col_needed]
#         return(setNames(as.numeric(out), rownames(imp_mat)))
#     }
#
#     # "permutation": use caret::varImp on a caret RF model trained on current features
#     # (kept separate to allow different importance implementations)
#     ctrl <- trainControl(method = "none")
#     caret_rf <- train(
#         x = x_df,
#         y = y_vec,
#         method = "rf",
#         trControl = ctrl,
#         ntree = rf_fit$ntree
#     )
#     vi <- varImp(caret_rf, scale = FALSE)$importance
#
#     # caret's varImp returns a data.frame; for multiclass it may have multiple columns.
#     # We'll collapse by mean across classes if needed.
#     if (ncol(vi) > 1) {
#         score <- rowMeans(vi)
#     } else {
#         score <- vi[[1]]
#     }
#     return(setNames(as.numeric(score), rownames(vi)))
# }
#
# #-----------------------------
# # Feature selection loop (backward elimination)
# #-----------------------------
# features <- colnames(x)
#
# # Baseline accuracy with all features
# best_acc <- cv_accuracy(x[, features, drop = FALSE], y, ntree, cv_folds)
# best_features <- features
#
# cat(sprintf("Baseline (all features): accuracy = %.4f | p = %d\n", best_acc, length(features)))
#
# for (iter in seq_len(max_iter)) {
#     if (length(features) <= 1) {
#         cat("Stopping: only one feature left.\n")
#         break
#     }
#
#     # Fit RF on current features (for importance ranking)
#     rf_fit <- randomForest(
#         x = x[, features, drop = FALSE],
#         y = y,
#         ntree = ntree,
#         importance = TRUE
#     )
#
#     # Compute importance scores and pick least important feature to drop
#     imp <- get_importance(rf_fit, x[, features, drop = FALSE], y, importance_measure)
#
#     # Handle any NAs just in case (push NAs to the bottom)
#     imp[is.na(imp)] <- -Inf
#
#     drop_feat <- names(which.min(imp))
#     new_features <- setdiff(features, drop_feat)
#
#     # Evaluate CV accuracy after dropping
#     new_acc <- cv_accuracy(x[, new_features, drop = FALSE], y, ntree, cv_folds)
#
#     cat(sprintf(
#         "Iter %02d | measure=%s | drop=%s | acc=%.4f | loss=%.4f | p=%d\n",
#         iter, importance_measure, drop_feat, new_acc, (best_acc - new_acc), length(new_features)
#     ))
#
#     # Accept drop if within tolerance (accuracy does not fall more than 0.02 from best)
#     if ((best_acc - new_acc) <= accuracy_loss_tol) {
#         features <- new_features
#         best_acc <- new_acc
#         best_features <- features
#     } else {
#         cat("Stopping: accuracy loss exceeded tolerance.\n")
#         break
#     }
# }
#
# cat("\nSelected features:\n")
# print(best_features)
#
# cat(sprintf("\nFinal CV accuracy: %.4f\n", best_acc))

# library(randomForest)
# library(caret)
#
# set.seed(123)
#
# # Example dataset
# data(iris)
#
# x <- iris[, -5]
# y <- iris$Species
#
# # Parameters
# accuracy_loss <- 0.02
# max_iter <- 20
# ntree <- 500
#
# # Cross-validation setup
# ctrl <- trainControl(
#     method = "cv",
#     number = 5
# )
#
# # Initial feature set
# features <- colnames(x)
#
# # Train initial model
# rf_full <- train(
#     x = x,
#     y = y,
#     method = "rf",
#     ntree = ntree,
#     importance = TRUE,
#     trControl = ctrl
# )
#
# best_accuracy <- max(rf_full$results$Accuracy)
# best_features <- features
#
# cat("Initial accuracy:", best_accuracy, "\n")
#
# # Iterative feature elimination
# for (i in seq_len(max_iter)) {
#
#     # Train RF on current feature set
#     rf_model <- randomForest(
#         x = x[, features, drop = FALSE],
#         y = y,
#         ntree = ntree,
#         importance = TRUE
#     )
#
#     # Variable importance
#     imp <- importance(rf_model, type = 2)
#     imp <- data.frame(
#         feature = rownames(imp),
#         importance = imp[, 1]
#     )
#
#     # Remove least important feature
#     remove_feature <- imp$feature[which.min(imp$importance)]
#     new_features <- setdiff(features, remove_feature)
#
#     # Stop if no features left
#     if (length(new_features) == 0) break
#
#     # Refit model with CV
#     rf_cv <- train(
#         x = x[, new_features, drop = FALSE],
#         y = y,
#         method = "rf",
#         ntree = ntree,
#         trControl = ctrl
#     )
#
#     new_accuracy <- max(rf_cv$results$Accuracy)
#
#     cat(
#         sprintf(
#             "Iter %d | Removed: %s | Accuracy: %.4f\n",
#             i, remove_feature, new_accuracy
#         )
#     )
#
#     # Check accuracy loss constraint
#     if ((best_accuracy - new_accuracy) <= accuracy_loss) {
#         features <- new_features
#         best_accuracy <- new_accuracy
#         best_features <- features
#     } else {
#         cat("Stopping: accuracy loss exceeded threshold\n")
#         break
#     }
# }
#
# # Final result
# cat("\nSelected features:\n")
# print(best_features)
#
# cat("\nFinal accuracy:\n")
# print(best_accuracy)

