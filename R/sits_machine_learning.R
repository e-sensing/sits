#' @title Train classifiction models using the TWDTW distances for each class
#' @name sits_train
#' 
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Given a SITS tibble time series, returns trained models using Machine Learning.
#' 
#' @param data.tb a SITS tibble time series 
#' @param model a vector of character with the models to be tested. The options are: XXXXXXXX
#' @param ... other parameters to pass to \code{\link[sits]{sits_patterns}} and 
#' \code{\link[sits]{sits_TWDTW_matches}}
#' 
#' @export
sits_train <- function(data.tb, models = c("svm", "random forests", "lda"), ...){

     # Create temporal patterns 
     patterns.tb <- sits_patterns(data.tb, ...)
     
     # Apply TWDTW analysis 
     alignments.tb <- sits_TWDTW_matches(data.tb, patterns.tb, ...)
     
     # Spread TWDTW matches  
     matches.tb <- .sits_spread_matches(alignments.tb)
     
     # TO INCLUDE - Model selection  
     
     # TO INCLUDE - RETURN BEST MODEL 
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

