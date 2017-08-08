#' Functions for machine learning associated to the SITS package
#' The attributes for the training functions are the DTW distances 
#' computed by the TWTDW function (see documentation on sits_TWDTW_matches)
#'
#'
#' models supported: 'svm', 'random forests', 'boosting', 'lda', 
#'                   'multinomial logit', 'lasso', 'ridge', 'elnet', 'best model'

#' @title Train SITS classifiction models using support vector machines
#' @name sits_train_svm 
#' 
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Given a SITS tibble time series with DTW matches produced by TWDTW, 
#'              returns trained models using support vector machines. This function will 
#'              use the TWDTW alignment information for all classes as the attributes
#'              of the SVM. Please use this function in the following way:
#'              1. call sits_patterns to produce a set a labelled patters from a reference data set
#'              2. call sits_TWDTW_matches to produce a set of alignements
#'              3. use the alignement response as an input to the training function
#' 
#' @param alignments.tb a SITS tibble time series 
#' @param 
#' 
#' @export
#' 
#' 
#' 
#'

sits_train <- function(data.tb, model_class, ...){
     
     # Create temporal patterns 
     patterns.tb <- sits_patterns(data.tb, ...)
     
     # Apply TWDTW analysis 
     alignments.tb <- sits_TWDTW_matches(data.tb, patterns.tb, ...)
     
     # Spread TWDTW matches  
     matches.tb <- .sits_spread_matches(alignments.tb)
     
     dados <- matches.tb[, !(colnames(matches.tb) %in% c('longitude', 
                                                         'latitude', 'start_date',
                                                         'end_date', 'label',
                                                         'coverage', 'time_series',
                                                         'matches'))]
     categorias <- labels(table(dados$reference))[[1]]
     
     ynn <- nnet::class.ind(as.factor(dados$reference))
     colunas_classes <- paste0('l', 1:length(categorias))
     colnames(ynn) <- colunas_classes
     dados <- cbind(dados, ynn)

     dados <- dados[,colnames(dados) %in% c('reference', categorias, colunas_classes)]
     dados <- rename(dados, c('reference'='categoria'))
     
     dados$categorianum <- 0
     for (i in 1:length(categorias)) { dados[dados$categoria == categorias[i],'categorianum'] <-  i}
     
     # Splitting samples 
     dadosTest <- dados
     dadosTrain <- dados
     
     set.seed(2104);
     
     if (model_class %in% c('lda', 'svm', 'boosting', 'multinomial logit',
                            'random forest', 'lasso', 'ridge', 'elnet'))
     {
          trainIndex <- caret::createDataPartition(dados$categoria, p = .8, 
                                            list = FALSE, 
                                            times = 1)
          dadosTrain <- dados[ trainIndex,]
          dadosTest  <- dados[-trainIndex,]
     }
     
     # Defining models
     
     nomes <- names(dados)
     lognomes <- paste0('log(', nomes[!nomes %in% c('categoria', 'categorianum', 
                                                    colunas_classes)], ')'); 
     orinomes <- paste0(nomes[!nomes %in% c('categoria', 'categorianum', 
                                            colunas_classes)]); 

     yneunets <- paste0(nomes[!nomes %in% c('categoria', 'categorianum', categorias)]); 

     formulann <- stats::as.formula(paste0(paste(yneunets, collapse = " + "), " ~ ",
                                    paste(orinomes, collapse = " + ")));
     formula1 <- stats::as.formula(paste("factor(categoria) ~ ", 
                                  paste(lognomes, collapse = " + ")));
     formula2 <- stats::as.formula(paste("categorianum ~ ", 
                                  paste(lognomes, collapse = " + ")));

     yTrain <- data.matrix(dadosTrain[,1])
     yTest <-  data.matrix(dadosTest[,1])
     
     xTrain <- log(data.matrix(dadosTrain[,c(2:(length(categorias)+1))]))
     xTest <-   log(data.matrix(dadosTest[,c(2:(length(categorias)+1))]))
     
     # Training the models
     
     if (model_class == 'svm')
     {
          model.fit <- e1071::svm(formula1, data=dadosTrain, 
                           kernel = "linear", type="C-classification", 
                           epsilon = 0.1, cost = 100)
     }
     if (model_class == 'lda')
     {
          model.fit <- MASS::lda(formula1, data=dadosTrain)
     }
     if (model_class == 'lasso')
     {
          model.fit.cv <- glmnet::cv.glmnet(y = factor(yTrain), x = xTrain, 
                                    family="multinomial", alpha=1)
     }
     if (model_class == 'lasso')
     {
          model.fit.cv <- glmnet::cv.glmnet(y = factor(yTrain), x = xTrain, 
                                    family="multinomial", alpha=0)
     }
     if (model_class == 'lasso')
     {
          model.fit.cv <- glmnet:: cv.glmnet(y = factor(yTrain), x = xTrain, 
                                    family="multinomial", alpha=.5)
     }
     if (model_class == 'multinomial logit')
     {
          model.fit <- nnet::multinom(formula1, data=dadosTrain)
     }
     if (model_class == 'boosting')
     {
          model.fit <- gbm::gbm(formula1, data=dadosTrain, 
                           distribution="multinomial", 
                           n.trees=500,interaction.depth=4)
     }
     if (model_class == 'random forest')
     {
          model.fit <- randomForest::randomForest(y = factor(yTrain), 
                                    x = xTrain, data=NULL, ntree=200, 
                                    norm.votes=FALSE)
     }
     
     # TO INCLUDE - Model selection  
     
     # TO INCLUDE - RETURN BEST MODEL 
     
     # RETURN OBJECT
     return (model.fit)
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
#' @param model a model trained by \code{\link[sits]{sits_train}}
#' @param ... other parameters to pass to \code{\link[sits]{sits_patterns}} and 
#' \code{\link[sits]{sits_TWDTW_matches}}
#' 
#' @export
sits_predict <- function(data.tb, model, ...){

     # Create temporal patterns 
     patterns.tb <- sits_patterns(data.tb, ...)
     
     # Apply TWDTW analysis 
     alignments.tb <- sits_TWDTW_matches(data.tb, patterns.tb, ...)
     
     # Spread TWDTW matches 
     matches.tb <- .sits_spread_matches(alignments.tb)
     
     # TO INCLUDE - Use the model to predict the class for ts 
     
     # TO INCLUDE - rturn tibble with reference and predicted 
     
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
#' @param model a model trained by \code{\link[sits]{sits_train}}
#' \code{\link[sits]{sits_TWDTW_matches}}
#' @param ... other arguments to pass to  \code{\link[raster]{beginCluster}} and 
#' \code{\link[raster]{writeStart}}
#' 
#' @export
sits_predict_stack <- function(x, 
                               model, 
                               filename = "",
                               progress = 'text',
                               parallel = TRUE, ...) {
     
     if(parallel){
          raster::beginCluster(...)
          out <- .apply_stack_parallel(x,
                                       fun = .sits_predict_stack,
                                       args.list = list(model),
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
     
     v <- raster::getValues(x, bs$row[k], bs$nrows[k])
     
     # TO INCLUDE - Prediction algorithm 
     
     # TO INCLUDE - Return predicted class 
     
}

.sits_organise_train <- function (matches.tb) {

     data <- matches.tb[, !(colnames(matches.tb) %in% c('longitude', 
                                                   'latitude', 'start_date',
                                                   'end_date', 'label',
                                                   'coverage', 'time_series',
                                                   'matches'))]
     categories <- labels(table(dados$reference))[[1]]

     ynn <- nnet::class.ind(as.factor(dados$reference))
     columns_classes <- paste0('l', 1:length(categories))
     colnames(ynn) <- columns_classes

     data <- data %>% 
          cbind(ynn) %>% 
          .[,colnames(.) %in% c('reference', categorias, colunas_classes)] %>% 
          rename(c('reference'='category'))

     data$categorianum <- 0
     for (i in 1:length(categories)) { data[data$category == categories[i],'categorynum'] <-  i}

     return (data)
}
