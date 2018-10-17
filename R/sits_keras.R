#' @title Save a Keras model for later processing in sits
#' @name sits_save_keras
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a Keras model trained by \code{\link[sits]{sits_deeplearning}} and saves it in two files.
#' One file is the keras model itself (saved as hdf5) and the other is the R enviroment required for sits
#' to work with the model.
#'
#' @param  model           An R Keras model trained by \code{\link[sits]{sits_deeplearning}}.
#' @param  hdffile         An hdf5 file where the keras model is to be saved.
#' @param  rdsfile         A rds file where the R environment is to be saved.
#'
#' @export
sits_save_keras <-  function(model, hdffile = "./model_keras.h5", rdsfile = "./model_keras.rds"){
    # retrieve the keras model from the sits model object
    model.keras <- environment(model)$model.keras
    # save the keras model in a HDF5 file
    keras::save_model_hdf5(model.keras, hdffile)
    # save the sits model in an RDS file
    saveRDS(model, rdsfile)
}

#' @title Load a Keras model for processing in sits
#' @name sits_load_keras
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a save Keras model trained by \code{\link[sits]{sits_deeplearning}}, which has
#' been saved in two files and loads it in memory for further processing.
#' One file is the keras model itself (saved as hdf5) and the other is the R enviroment required for sits
#' to work with the model.
#'
#' @param  hdffile         An hdf5 file where the keras model is to be saved.
#' @param  rdsfile         A rds file where the R environment is to be saved.
#' @return An R Keras model trained by \code{\link[sits]{sits_deeplearning}}.
#'
#' @export
sits_load_keras <-  function(hdffile, rdsfile){
    # loads the keras model from an hdf5 file
    model.keras <- keras::load_model_hdf5(hdffile)
    # loads the sits model object from an RDS file
    dl_model <- readRDS(rdsfile)
    # load the Keras model in the sits model environment
    environment(dl_model)$model.keras <- environment(model.keras)$x
    # returns the dl_model
    return(dl_model)
}
