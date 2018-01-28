#' @title Train a SITS classification models using generalised linear models (lasso and ridge)
#' @name sits_glm
#'
#' @author Alexandre Xavier Ywata de Carvalho, \email{alexandre.ywata@@ipea.gov.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Use generalized liner model (glm) via penalized maximim likelihood to classify data.
#' These attributes are distance metrics between patterns and observations, obtained by a distance
#' function in SITS (either sits_distances or sits_TWTDTW_distances).
#' This function is a front-end to the "cv.glmnet" method in the "glmnet" package.
#' Please refer to the documentation in that package for more details.
#'
#' @param distances.tb     a time series with a set of distance measures for each training sample
#' @param family           Response type. Can be either "gaussian", "binomial", "poisson", "multinomial", "cox", or "mgaussian". (default: "multinomial")
#' @param alpha            the elasticnet mixing parameter, with 0<=alpha<=1.
#'                         Set alpha = 0.0 to obtain ridge model, and alpha = 1.0 to obtain lasso model.
#'                         (refer to `glmnet::glmnet` function for more details)
#' @param ...              other parameters to be passed to `glmnet::glmnet` function
#' @return result          a model function to be passed in sits_predict
#' @examples
#' \donttest{
#' # Retrieve the set of samples for the Mato Grosso region (provided by EMBRAPA)
#' data(samples_MT_ndvi)
#' # get a point with a 16 year time series
#' data(point_ndvi)
#' # classify the point
#' class.tb <- sits_classify (point_ndvi, samples_MT_ndvi, sits_glm(alpha = 1.0))
#' }
#' @export
#'
sits_glm <- function(distances.tb = NULL, family = "multinomial", alpha = 1.0, ...) {

    # function that returns glmnet::multinom model based on a sits sample tibble
    result_fun <- function(train_data.tb){


        # is the input data the result of a TWDTW matching function?
        ensurer::ensure_that(train_data.tb, "reference" %in% names(.),
                             err_desc = "sits_glm: input data does not contain distance")

        # call glmnet::multinom method and return the trained multinom model
        result_glm <- glmnet::cv.glmnet(y = factor(data.matrix(train_data.tb$reference)),
                                        x = log(data.matrix(train_data.tb[,3:NCOL(train_data.tb)])),
                                        family = family, alpha = alpha, ...)

        # construct model predict enclosure function and returns
        model_predict <- function(values.tb){
            return(stats::predict(result_glm,
                                  s = result_glm$lambda.min,
                                  newx = log(data.matrix(values.tb[,3:NCOL(values.tb)])), type = "class"))
        }
        return(model_predict)
    }

    result <- .sits_factory_function(distances.tb, result_fun)
    return(result)
}
