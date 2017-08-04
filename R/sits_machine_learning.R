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
#' 
#' @export
sits_train <- function(data.tb, models = c("svm", "random forests", "lda"), ...){

     # Create temporal patterns 
     patterns.tb <- sits_patterns(data.tb, ...)
     
     # Apply TWDTW analysis 
     alignments.tb <- sits_TWDTW_matches(data.tb, patterns.tb, ...)
     
     # Spread TWDTW matches  
     matches.tb <- .sits_spread_matches(alignments.tb)
     
     # 
     
     
}


