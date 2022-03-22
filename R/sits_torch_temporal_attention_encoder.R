#' @title Torch module for positional encoder
#' @name .torch_positional_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module for positional encoding based on the
#' work by Vivien Garnot.
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
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#' Schneider, Maja; Körner, Marco,
#' "[Re] Satellite Image Time Series Classification
#' with Pixel-Set Encoders and Temporal Self-Attention." ReScience C 7 (2), 2021.
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
    initialize = function(timeline, dim_encoder = 128){

        # length of positional encoder is the length of dates vector
        max_len <- length(timeline)
        # keep the dates vector
        self$dates <- timeline
        # initialize 'days' vector as the difference in days btw
        # each date and the first date
        days <- unlist(purrr::map(
            timeline,
            function(d){
                lubridate::interval(timeline[[1]], d) / lubridate::days(1)
            }
        )
        )
        # create a days tensor
        days_t <- torch::torch_tensor(days)
        days_t <- torch::torch_unsqueeze(days_t, 2)

        # Calculate the positional encoding p
        # 2D shape [(max_len, dim_encoder:128)]
        p <- torch::torch_zeros(max_len, dim_encoder)
        # calculate an exponential distance measure for the positions
        div_term <-  torch::torch_exp(
            torch::torch_arange(
                start = 0,
                end = dim_encoder - 1,
                step = 2
            )
            * (-log(1000.0) / dim_encoder)
        )
        div_term <- torch::torch_unsqueeze(div_term, 1)
        # fill the tensor p
        p[ , seq(1, dim_encoder, 2)] <- torch::torch_sin(days_t * div_term)
        p[ , seq(2, dim_encoder, 2)] <- torch::torch_cos(days_t * div_term)
        # here p is a 2D shape [(max_len, dim_encoder:128)]
        p <- torch::torch_unsqueeze(p, 1)
        # after unsqueeze p is a 3D shape [(1, max_len, dim_encoder:128)]
        self$register_buffer('p', p)
    },
    forward = function(x){
        x = x + self$p
        return(x)
    }
)
#' @title Torch module for temporal attention encoder
#' @name .torch_temporal_attention_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module for temporal attention encoding.
#'
#' This function part of the implementation of the paper by Vivien Garnot
#' referenced below.
#' We used the code made available by Maja Schneider in her work with
#' Marco Körner referenced below and available at
#' https://github.com/maja601/RC2020-psetae.
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#' Schneider, Maja; Körner, Marco,
#' "[Re] Satellite Image Time Series Classification
#' with Pixel-Set Encoders and Temporal Self-Attention." ReScience C 7 (2), 2021.
#'
#' @param timeline                  Timeline of input time series.
#' @param dim_encoder               Dimension of the positional enconder.
#' @param n_heads                   Number of attention heads.
#' @param dim_q_k                   Shared dimensions of keys and queries for attention.
#' @param dim_input_mlp3            Dimension of input to multi-layer perceptron
#'                                  (MLP3 in Garnot's paper)
#' @param dim_hidden_nodes_mlp3     Dimensions of hidden nodes in multi-layer
#'                                  perceptrons (MLP3 in Garnot's paper)
#'
#' @return A linear tensor block.
#'
.torch_temporal_attention_encoder <- torch::nn_module(
    classname = "torch_temporal_attention_encoder",

    initialize = function(timeline,
                          dim_encoder = 128,
                          n_heads = 4,
                          dim_input_mlp3 = 512,
                          dim_layers_mlp3 = c(128, 128)){
        # store parameters
        self$dim_encoder <-  dim_encoder
        self$n_heads     <-  n_heads
        # calculate the dimension of split enconder
        self$dim_k <-  dim_encoder %/% n_heads
        # obtain the positional encoding
        self$pos_encoding <-  .torch_positional_encoding(
            timeline = timeline,
            dim_encoder = dim_encoder
        )
        # fully connected layer for queries (FC1)
        self$fc1_q <- torch::nn_linear(dim_encoder, dim_encoder)
        # fully connected layer for keys (FC1)
        self$fc1_k <- torch::nn_linear(dim_encoder, dim_encoder)
        # fully connected layer for mean queries (FC2)
        self$fc2   <- torch::nn_linear(dim_encoder, dim_encoder)
        # final multi-layer perceptron
        self$mlp3  <- .torch_multi_linear_batch_norm_relu(
            dim_input_mlp3,
            dim_layers_mlp3
        )
    },
    forward = function(x){
        # Follows figure 2 of Garnot's paper
        # "Satellite Image Time Series Classification
        # with Pixel-Set Encoders and Temporal Self-Attention"
        #
        # obtain the input parameters
        batch_size   <- x$shape[[1]]
        # seq_len is the
        seq_len      <- x$shape[[2]]
        hidden_state <- x$shape[[3]]
        # Calculate the positional encoding
        # result is 3D shape [batch_size x seq_len x dim_encoder:128]
        e_p <-  self$pos_encoding(x)

        # Calculate the query and key tensors
        # FC1 is the fully-connected layer generating key-query pairs
        # for each head

        # Calculate the query tensor
        # Run the encoded position through FC1
        # input is a tensor of shape [batch_size x seq_len x dim_encoder:128]
        # result is a tensor of shape [batch_size x seq_len x dim_encoder:128]
        q <-  self$fc1_q(e_p)
        # Calculate the mean of query tensor along dimension 2
        # result is a tensor of shape [batch_size x dim_encoder:128]
        q_mean <-  torch::torch_mean(q, dim = 2)
        # Run the mean by a FC2 (fully connected layer 2)
        q_hat <- self$fc2(q_mean)
        # Reorganize the result as a 3D tensor
        # result is a tensor [batch_size x n_heads:4 x dim_k:32]
        q_hat <- q_hat$view(c(batch_size, self$n_heads, self$dim_k))
        q_hat <- q_hat$contiguous()
        # Reorganize the result as a 2D tensor
        # output shape is 2D [(batch_size * n_heads:4) x dim_k:32]
        q_hat <- q_hat$view(c(-1, self$dim_k))
        # Create an additional dimension
        # output shape is 3D [(batch_size * n_heads:4) x 1 x dim_k:32]
        q_hat <- q_hat$unsqueeze(dim = 2)

        # Calculate the key tensor
        # Run the encoded position through FC1
        # result is a tensor of shape [batch_size x seq_len x dim_encoder:128]
        k <-  self$fc1_k(e_p)
        # Reorganize the tensor as a 4D shape
        # shape is 4D [batch_size x seq_len x n_heads:4 x dim_k:32]
        k <- k$view(c(batch_size, seq_len, self$n_heads, self$dim_k))
        # Permute dimensions (2,3) of the 4D tensor
        k <- k$permute(c(1, 3, 2, 4))
        # shape is 4D [batch_size x n_heads:4 x seq_len x dim_k:32]
        k <- k$contiguous()
        # Reduce the key tensor to 3D merging dimensions (1,2)
        # shape is 3D [(batch_size * n_heads:4) x seq_len x dim_k:32]
        k <- k$view(c(batch_size *  self$n_heads, seq_len, self$dim_k))

        # transpose key tensor dimensions 2 and 3
        # input shape is 3D [(batch_size * n_heads) x seq_len x dim_k]
        # output shape is 3D [(batch_size * n_heads) x dim_k x seq_len]
        k <- torch::torch_transpose(k, dim0 = -2, dim1 = -1)

        # Calculate attention
        # Attention scores =  averaged product of query and key tensor
        # q_hat has shape 3D [(batch_size * n_heads) x 1 x dim_k]
        # k transposed has 3D shape  [(batch_size * n_heads) x dim_k x seq_len]
        # output scores has 3D shape [(batch_size * n_heads) x 1 x seq_len]
        attention_scores = torch::torch_matmul(q_hat, k)/sqrt(self$dim_k)

        # Attention probs are calculated as
        # softmax of the normalized query * key product using the last dimension
        # input shape is 3D  [(batch_size * n_heads) x 1 x seq_len]
        # output_shape is 3D [(batch_size * n_heads) x 1 x seq_len]
        attention_probs = torch::nnf_softmax(attention_scores, dim = -1)

        # Values (position encoding)
        # input 3D shape [batch_size x seq_len x hidden_state:128]
        # output 3D shape [(batch_size * num_heads) x seq_len x hidden:128]
        v = e_p$`repeat`(c(self$n_heads, 1, 1))

        # Multi-head self-attention
        # multiply values by product of query * key
        # attention_probs - 3D shape [(batch_size * n_heads) x 1 x seq_len]
        # v - 3D shape [(batch_size * num_heads) x seq_len x hidden:128]
        # result has 3D shape [(batch_size * num_heads) x 1 x hidden:128]
        attention_output = torch::torch_matmul(attention_probs, v)

        # squeeze attention output
        # input shape 3D [(batch_size * n_heads) x 1 x hidden:128]
        # output shape 2D [(batch_size * n_heads) x hidden:128]
        attention_output <- torch::torch_squeeze(attention_output)

        # reshape attention output to 3D shape
        # input shape is 2D [(batch_size * n_heads) x hidden:128]
        # output shape is 3D [batch_size x n_heads x hidden_state:128]
        attention_output = attention_output$contiguous()
        attention_output = attention_output$view(c(batch_size, self$n_heads, -1))

        # reshape attention output to 2D shape
        # input shape is 3D [batch_size x n_heads x dim_encoder:128]
        # output shape is 2D [batch_size x (n_heads:4 * dim_encoder:128)]
        attention_output = attention_output$contiguous()
        attention_output = attention_output$view(c(batch_size, -1))

        # Run the output by a multi-layer perceptron
        # input shape is 2D [batch_size x (n_heads:4 * dim_encoder:128)]
        # output shape is 2D [batch_size x dim_encoder:128]
        o_hat = self$mlp3(attention_output)
        return(o_hat)
    }
)





