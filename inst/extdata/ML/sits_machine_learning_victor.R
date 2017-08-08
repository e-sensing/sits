#' @param data.tb a SITS tibble time series 
#' @param ... other parameters to pass to \code{\link[sits]{sits_patterns}} and 
#' \code{\link[sits]{sits_TWDTW_matches}}
#' 
#' @export 
sits_train <- function(data.tb, ...){

     # Create temporal patterns 
     fun_sits_patterns <- .set_fun_args(fun = sits_patterns, ...)
     patterns.tb <- fun_sits_patterns(samples.tb = data.tb)
     
     # Apply TWDTW analysis 
     fun_sits_TWDTW_matches <- .set_fun_args(fun = sits_TWDTW_matches, ...)
     alignments.tb <- fun_sits_TWDTW_matches(series.tb = data.tb, patterns.tb = patterns.tb)
     
     # Spread TWDTW matches  
     matches.tb <- .sits_spread_matches(alignments.tb)
     
     # Get categories 
     categories <- patterns.tb$label
     
     # Get distances 
     distances.tb <- matches.tb %>% 
          dplyr::select_(.dots = c("reference", categories))

     ynn <- nnet::class.ind(as.factor(distances.tb$reference)) %>% 
          tibble::as_tibble() 
     
     colunas_classes <- paste0('l', seq_along(categories))
     
     names(ynn) <- colunas_classes
     
     distances.tb <- distances.tb %>% 
          dplyr::bind_cols(ynn)

     distances.tb %>% 
          dplyr::mutate(categories_num = match(reference, categories) )
     
     trainIndex <- caret::createDataPartition(distances.tb$reference, p = .8, 
                                              list = FALSE, 
                                              times = 1)
     
     distances.tbTrain <- distances.tb %>% 
          dplyr::slice(trainIndex[,1])
     
     distances.tbTest <- distances.tb %>% 
          dplyr::slice(- trainIndex[,1])
     
     # Defining models
     nomes <- names(distances.tb)
     
     lognomes <- paste0('log(', categories, ')')

     formulann <- as.formula(paste0(paste(colunas_classes, collapse = " + "), " ~ ",
                                    paste(categories, collapse = " + ")))
     
     formula1 <- as.formula(paste("factor(reference) ~ ", 
                                  paste(lognomes, collapse = " + ")))
     
     formula2 <- as.formula(paste("categories_num ~ ", 
                                  paste(lognomes, collapse = " + ")))
     
     # yTrain <- data.matrix(distances.tbTrain[, 1])
     yTrain <- distances.tbTrain %>% 
          dplyr::select(reference) %>% 
          as.matrix()
     
     # yTest <-  data.matrix(distances.tbTest[, 1])
     yTest <-  distances.tbTest %>% 
          dplyr::select(reference) %>% 
          as.matrix()
     
     # xTrain <- log(data.matrix(distances.tbTrain[, c(2:(length(categories) + 1))]))
     xTrain <- distances.tbTrain %>% 
          dplyr::select(categories) %>% 
          as.matrix()
     
     # xTest <-   log(data.matrix(distances.tbTest[, c(2:(length(categories) + 1))]))
     xTest <- distances.tbTest %>% 
          dplyr::select(categories) %>% 
          as.matrix()

     # Training the models
     out.tb <- tibble::tribble(
                       ~model, ~fit, 
                        "svm", try(e1071::svm(formula1, data = distances.tbTrain, kernel = "linear", type = "C-classification", epsilon = 0.1, cost = 100)),
                        "lda", try(MASS::lda(formula1, data = distances.tbTrain)),
                    "lasso 0", try(glmnet::cv.glmnet(y = factor(yTrain), x = xTrain, family = "multinomial", alpha = 0)),
                  "lasso 0.5", try(glmnet::cv.glmnet(y = factor(yTrain), x = xTrain, family = "multinomial", alpha = .5)),
                  "lasso 1.0", try(glmnet::cv.glmnet(y = factor(yTrain), x = xTrain, family = "multinomial", alpha = 1)),
          "multinomial logit", try(nnet::multinom(formula1, data = distances.tbTrain)),
                   "boosting", try(gbm::gbm(formula1, data = distances.tbTrain, distribution = "multinomial", n.trees = 500, interaction.depth = 4)),
              "random forest", try(randomForest::randomForest(y = factor(yTrain), x = xTrain, data = NULL, ntree = 200, norm.votes = FALSE))
     ) %>% dplyr::mutate(patterns = list(patterns.tb)) %>% 
          dplyr::mutate(twdtw_call = list(fun_sits_TWDTW_matches))
     
     out.tb <- out.tb %>% 
          dplyr::rowwise() %>% 
          dplyr::do(reference = yTest, predicted = try(as.character(predict(.$fit, newdata = xTest)))) %>% 
          dplyr::bind_cols(out.tb, .)

     out.tb <- out.tb %>% 
          dplyr::rowwise() %>% 
          dplyr::do(accuracy = try(rfUtilities::accuracy(.$pred, yTest))) %>% 
          dplyr::bind_cols(out.tb, .)
     
     return(out.tb)
     
}


#' @title Predict class based on the trained models 
#' @name sits_predict
#' 
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a SITS tibble time series and a model trained by \code{\link[sits]{sits_train}}, 
#' returns a SITS tibble with the classification. 
#' 
#' @param data.tb a SITS tibble time series
#' @param model.fit a model trained by \code{\link[sits]{sits_train}}
#' @param ... other parameters to pass to \code{\link[sits]{sits_TWDTW_matches}}
#' 
#' @export
sits_predict <- function(data.tb, model.fit, ...){

     # Get best model based on the f.score 
     i <- which.max(sapply(model.fit$accuracy, function(xx) try(xx$f.score)))
     best_model.tb <- model.fit %>% 
          dplyr::slice(i)
     
     # Get TWDTW call used in the training step 
     fun_sits_TWDTW_matches <- (best_model.tb %>% 
          dplyr::select(twdtw_call))[[1]][[1]]
          
     # Get temporal patterns used in the training step 
     patterns.tb <- (best_model.tb %>% 
                          dplyr::select(patterns))[[1]][[1]]

     # Get categories 
     categories <- patterns.tb$label
     
     # Apply TWDTW analysis 
     alignments.tb <- fun_sits_TWDTW_matches(series.tb = data.tb, patterns.tb = patterns.tb)
     
     # Spread TWDTW matches  
     matches.tb <- .sits_spread_matches(alignments.tb)
     
     xTest <- matches.tb %>% 
          dplyr::select(categories) %>% 
          as.matrix()

     res <- (best_model.tb %>% 
               dplyr::rowwise() %>% 
               dplyr::do(classification = try(as.character(predict(.$fit, newdata = xTest)))))[[1]][[1]]

     out.tb <- data.tb %>% 
          dplyr::mutate(classification = res)
               
     return(out.tb)

}


#' @title Predict class based on the trained models 
#' @name sits_predict_stack
#' 
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a raster \code{\link[raster]{stack}} object and a model trained 
#' by \code{\link[sits]{sits_train}}, returns a raster stack object with the classification. 
#' 
#' @param x a raster \code{\link[raster]{stack}} object with TWDTW distances for 
#' each class in the layers
#' @param model.fit a model trained by \code{\link[sits]{sits_train}}
#' \code{\link[sits]{sits_TWDTW_matches}}
#' @param labels class labels in the order of the raster stack layers 
#' @param filename a character with the file name. Optional
#' @param progress a character. See \code{\link[raster]{pbCreate}}. Default is \code{'text'}
#' @param parallel perform parallel processing. Default is TRUE 
#' @param ... other arguments to pass to  \code{\link[raster]{beginCluster}} and 
#' \code{\link[raster]{writeStart}}
#' 
#' @export
sits_predict_stack <- function(x, 
                               model.fit, 
                               labels,
                               filename = "",
                               progress = 'text',
                               parallel = TRUE, ...) {
     
     # Get best model based on the f.score 
     i <- which.max(sapply(model.fit$accuracy, function(xx) try(xx$f.score)))
     fit <- (model.fit %>% 
                  dplyr::slice(i) %>% 
                  dplyr::select(fit))[[1]][[1]]
     
     if(parallel){
          raster::beginCluster(...)
          out <- .apply_stack_parallel(x,
                                       fun = .sits_predict_stack,
                                       args.list = list(fit = fit, labels = labels),
                                       filename = "", progress = 'text', ...)
          raster::endCluster()
     } else {
          stop("Not implemented yet", call. = TRUE)
     }
     
     return(out)
     
}



#' @title Predict class based on the trained models 
#' @name .sits_predict_stack
#' 
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a raster \code{\link[raster]{stack}} object and 
#' a model trained by \code{\link[sits]{sits_train}}, returns a raster matrix with the classification. 
#' 
#' @param k is the chunk index
#' @param x is the raster stack object
#' @param bs is is the chnuk information created by raster::blockSize
#' @param args.list is a list of other arguments used in the processing.
#' 
#' @noRd
#' 
.sits_predict_stack <- function(k, x, bs, args.list){
     
     # Get data from raster stack 
     v <- raster::getValues(x, bs$row[k], bs$nrows[k])
     
     # Prediction class using trained model  
     out <- as.character(predict(args.list$fit, newdata = v))
     
     # Convert class label to integer 
     out <- match(out, args.list$labels)
     
     # Return predicted class 
     return(out)
     
}

