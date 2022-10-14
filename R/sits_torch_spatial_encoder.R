#' @title Torch module for spatial encoder
#' @name .torch_pixel_spatial_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Defines a torch module for spatial encoding.
#'
#' This function is based on the paper by Vivien Garnot referenced below
#' and code available on github at
#' https://github.com/VSainteuf/pytorch-psetae.
#'
#' We also used the code made available by Maja Schneider in her work with
#' Marco Körner referenced below and available at
#' https://github.com/maja601/RC2020-psetae.
#'
#' There is an important difference: the model
#' proposed by Garnot assumes that the samples are available by parcel.
#' In his model, the samples from the same parcel are averaged using an MLP.
#' The current function implements an alternative to Garnot's pixel set
#' encoder for the case when only individual pixels are available.
#'
#' The spatial encoder is run for each temporal instance of the observations.
#' Thus it transforms a pixel with n bands to a pixel associated with an output
#' dimension of a linear encoder.
#'
#' The input of the PSE is a 3D tensors with shape
#' (batch_size x n_times x n_bands]
#'
#' Since the input tensors have a temporal dimension, this dimension
#' will be combined with the batch dimension so that the complete sequences
#' are processed at once. Then the temporal dimension is separated back
#' to produce a tensor of shape batch_size x n_times x embedding_dim
#'
#' Embedding dimension is the number of nodes in the last layer of the
#' MLP used to process the input sequence.
#'
#' If you use this method, please cite Garnot's and Schneider's work.
#'
#' @references
#' Vivien Garnot, Loic Landrieu, Sebastien Giordano, and Nesrine Chehata,
#' "Satellite Image Time Series Classification with Pixel-Set Encoders
#' and Temporal Self-Attention",
#' 2020 Conference on Computer Vision and Pattern Recognition.
#' pages 12322-12331.
#' DOI: 10.1109/CVPR42600.2020.01234
#'
#' Schneider, Maja; Körner, Marco,
#' "[Re] Satellite Image Time Series Classification
#' with Pixel-Set Encoders and Temporal Self-Attention."
#' ReScience C 7 (2), 2021.
#' DOI: 10.5281/zenodo.4835356
#'
#'
#' @param n_bands                   Number of bands per pixel.
#' @param layers_spatial_encoder    Layers of MLP spatial encoder
#'
#' @return A 3D shape tensor block.
#'

# module for spatial encoding
.torch_pixel_spatial_encoder <- torch::nn_module(
    classname = "torch_pixel_spatial_encoder",
    initialize = function(n_bands,
                          layers_spatial_encoder = c(32, 64, 128)) {
        self$layers_spatial_encoder <- layers_spatial_encoder
        self$spatial_encoder <- .torch_multi_linear_batch_norm_relu(
            input_dim = n_bands,
            hidden_dims = layers_spatial_encoder
        )
    },
    forward = function(values) {
        # batch size is the first dimension of the input tensor
        batch_size <- values$shape[[1]]
        # n_times is the second dimension
        n_times <- values$shape[[2]]
        # n_bands is the third dimension
        n_bands <- values$shape[[3]]
        # reshape the input
        # from a 3D shape [batch_size, n_times, n_bands]
        # to a 2D shape [(batch_size * n_times), n_bands]
        values <- values$view(c(batch_size * n_times, n_bands))
        # run the the 2D shape by a multi-layer perceptron
        # input is 2D shape [(batch_size * n_times), n_bands]
        dim_enc <-
            self$layers_spatial_encoder[[length(self$layers_spatial_encoder)]]
        values <- self$spatial_encoder(values)
        # output is a 2D shape[(batch_size * n_times), dim_enc]
        # reshape the output
        # from a 2D shape [(batch_size * n_times), n_bands]
        # to a 3D shape [batch_size, n_times, dim_enc]
        values <- values$view(c(batch_size, n_times, dim_enc))
        return(values)
    }
)
#' @title Torch module for positional encoder
#' @name .torch_positional_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @noRd
#' @description Defines a torch module for positional encoding, based on
#' the concepts of Vaswani et al (2017) and Garnot et al ()
#'
#' This function part of the implementation of the paper by Vivien Garnot
#' referenced below. We used the code made available by Maja Schneider in her
#' work with Marco Körner referenced below and available at
#' https://github.com/maja601/RC2020-psetae.
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention for Classifying Satellite Image
#' Time Series", https://arxiv.org/abs/2007.00586
#'
#' Schneider, Maja; Körner, Marco,
#' "[Re] Satellite Image Time Series Classification
#' with Pixel-Set Encoders and Temporal Self-Attention." ReScience C7(2), 2021.
#'
#' This function part of the implementation of the paper by Vivien Garnot
#' referenced below.
#'
#' We used the code made available by Maja Schneider in her work with
#' Marco Körner referenced below and available at
#' https://github.com/maja601/RC2020-psetae.
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention for Classifying Satellite Image
#' Time Series", https://arxiv.org/abs/2007.00586
#'
#' Schneider, Maja; Körner, Marco,
#' "[Re] Satellite Image Time Series Classification
#' with Pixel-Set Encoders and Temporal Self-Attention." ReScience C7(2), 2021.
#'
#' @param timeline          Timeline of input time series.
#' @param dim_encoder       Dimension of the positional encoder.
#'
#' @return A tensor block.
#'

# Positional Encoder
.torch_positional_encoding <- torch::nn_module(
    classname = "positional_encoding",
    # timeline is a vector with the observation dates
    initialize = function(timeline, dim_encoder = 128) {

        # length of positional encoder is the length of dates vector
        max_len <- length(timeline)
        # keep the dates vector
        self$dates <- timeline
        # initialize 'days' vector as the difference in days btw
        # each date and the first date
        days <- unlist(purrr::map(
            timeline,
            function(d) {
                lubridate::interval(timeline[[1]], d) / lubridate::days(1)
            }
        ))
        # create a days tensor
        days_t <- torch::torch_tensor(days)
        days_t <- torch::torch_unsqueeze(days_t, 2)

        # Calculate the positional encoding p
        # 2D shape [(max_len, dim_encoder:128)]
        p <- torch::torch_zeros(max_len, dim_encoder)
        # calculate an exponential distance measure for the positions
        div_term <- torch::torch_exp(
            torch::torch_arange(
                start = 0,
                end = dim_encoder - 1,
                step = 2
            )
            * (-log(1000.0) / dim_encoder)
        )
        div_term <- torch::torch_unsqueeze(div_term, 1)
        # fill the tensor p
        p[, seq(1, dim_encoder, 2)] <- torch::torch_sin(days_t * div_term)
        p[, seq(2, dim_encoder, 2)] <- torch::torch_cos(days_t * div_term)
        # here p is a 2D shape [(max_len, dim_encoder:128)]
        p <- torch::torch_unsqueeze(p, 1)
        # after unsqueeze p is a 3D shape [(1, max_len, dim_encoder:128)]
        self$register_buffer("p", p)
    },
    forward = function(x) {
        x <- x + self$p
        return(x)
    }
)
