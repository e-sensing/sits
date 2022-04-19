
#' @title Torch module for temporal attention encoder
#' @name .torch_temporal_attention_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal

#' @description
#' Defines a torch module for temporal attention encoding, inspired by the
#' work of Vaswani et al(2017). Since Attention models contain
#' no convolution, the model injects information about the relative
#' position of the tokens in the sequence. Vaswani et al use
#' sine and cosine functions of different frequencies.
#'
#' This function is based on the paper by Vivien Garnot referenced below
#' and code available on github at
#' https://github.com/VSainteuf/pytorch-psetae.
#'
#' We also used the code made available by Maja Schneider in her work with
#' Marco Körner referenced below and available at
#' https://github.com/maja601/RC2020-psetae.
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
#' @param timeline                  Timeline of input time series.
#' @param dim_encoder               Dimension of the positional encoder.
#' @param n_heads                   Number of attention heads..
#' @param input_out_enc_mlp         Dimensions of multi-layer perceptron
#'                                  used to encode the output
#'                                  (MLP3 in Garnot's paper)
#' @param hidden_nodes_out_enc_mlp  Hidden nodes in MLP used for output encoding
#'                                  (MLP3 in Garnot's paper)
#'
#' @return A linear tensor block.
#'
.torch_temporal_attention_encoder <- function() {
    torch::nn_module(
        classname = "torch_temporal_attention_encoder",

        initialize = function(timeline,
                              dim_encoder = 128,
                              n_heads = 4,
                              input_out_enc_mlp = 512,
                              hidden_nodes_out_enc_mlp = c(128, 128)){
            # store parameters
            self$dim_encoder <-  dim_encoder
            self$n_heads     <-  n_heads
            # calculate the dimension of split encoder
            self$dim_k <-  dim_encoder %/% n_heads
            # obtain the positional encoding
            self$pos_encoding <-  .torch_positional_encoding()(
                timeline = timeline,
                dim_encoder = dim_encoder
            )
            # fully connected layer for keys and queries processing
            self$fc <- torch::nn_linear(dim_encoder, dim_encoder)
            # final multi-layer perceptron
            self$mlp  <- .torch_multi_linear_batch_norm_relu()(
                input_dim = input_out_enc_mlp,
                hidden_dims = hidden_nodes_out_enc_mlp
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
            # FC is the fully-connected layer generating key-query pairs
            # for each head

            # Calculate the query tensor
            # Run the encoded position through FC
            # input is a tensor of shape [batch_size x seq_len x dim_encoder:128]
            # result is a tensor of shape [batch_size x seq_len x dim_encoder:128]
            q <-  self$fc(e_p)
            # Calculate the mean of query tensor along dimension 2
            # result is a tensor of shape [batch_size x dim_encoder:128]
            q_mean <- torch::torch_mean(q, dim = 2)
            # Run the mean by a FC (fully connected layer)
            q_hat <- self$fc(q_mean)
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
            # Run the encoded position through FC
            # result is a tensor of shape [batch_size x seq_len x dim_encoder:128]
            k <-  self$fc(e_p)
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

            # Values with positional encoding repeated over attention heads
            # input 3D shape [batch_size x seq_len x hidden_state:128]
            # output 3D shape [(batch_size * num_heads) x seq_len x hidden:128]
            values = e_p$`repeat`(c(self$n_heads, 1, 1))

            # Multi-head self-attention
            # multiply values by product of query * key
            # attention_probs - 3D shape [(batch_size * n_heads) x 1 x seq_len]
            # values - 3D shape [(batch_size * num_heads) x seq_len x hidden:128]
            # result has 3D shape [(batch_size * num_heads) x 1 x hidden:128]
            attention_output = torch::torch_matmul(attention_probs, values)

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
            o_hat = self$mlp(attention_output)
            return(o_hat)
        }
    )
}
#' @title Torch module for temporal attention encoder
#' @name .torch_light_temporal_attention_encoder
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module for temporal attention encoding.
#'
#' This implementation is based on the code made available by Vivien Garnot
#' https://github.com/VSainteuf/lightweight-temporal-attention-pytorch
#'
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#'
#' @param timeline                  Timeline of input time series.
#' @param in_channels               Dimension of the positional encoder.
#' @param n_heads                   Number of attention heads.
#' @param n_neurons                 Dimensions of MLP that processes the
#'                                  output of the attention heads.
#' @param dropout_rate              Dropout_rate.
#'
#' @return A linear tensor block.
#'
.torch_light_temporal_attention_encoder <- function() {
    torch::nn_module(
        classname = "torch_temporal_attention_encoder",

        initialize = function(timeline,
                              in_channels = 128,
                              n_heads = 16,
                              n_neurons = c(256, 128),
                              dropout_rate = 0.2){
            # store parameters
            self$in_channels <-  in_channels
            self$n_heads     <-  n_heads
            # calculate the dimension of key and query vectors
            self$d_key_query <-  in_channels %/% n_heads
            # calculate the length of the time sequence
            seq_len <- length(timeline)

            # input layer normalization
            self$in_layer_norm <- torch::nn_layer_norm(
                normalized_shape = in_channels
            )

            # Do a 1D convolution on the input sequence
            # input is 3D shape (batch_size x seq_len x in_channels:128)
            # output is 3D shape (batch_size x seq_len x d_model:256)
            self$d_model <- n_neurons[1]
            self$inconv  <- torch::nn_sequential(
                torch::nn_conv1d(
                    in_channels   = in_channels,
                    out_channels  = self$d_model,
                    kernel_size   = 1),
                torch::nn_layer_norm(
                    normalized_shape = c(self$d_model, seq_len)
                )
            )

            # obtain the positional encoding
            self$pos_encoding <-  .torch_positional_encoding()(
                timeline = timeline,
                dim_encoder = self$d_model
            )

            # calculate the attention heads
            self$attention_heads <- .torch_multi_head_attention()(
                n_heads   = n_heads,
                d_k       = self$d_key_query,
                d_in      = self$d_model
            )
            # output enconding
            # multi-layer perceptron with batch norm and relu
            hidden_dims <- n_neurons[-1]
            self$mlp  <- .torch_multi_linear_batch_norm_relu()(
                input_dim = n_neurons[1],
                hidden_dims = hidden_dims
            )
            # dropout node
            self$dropout  <- torch::nn_dropout(dropout_rate)

            # output layer normalization
            last_neuron <- n_neurons[length(n_neurons)]
            self$out_layer_norm <- torch::nn_layer_norm(
                normalized_shape = last_neuron
            )
        },
        forward = function(x){
            # Follows figure 1 of Garnot's paper
            # "Lightweight Temporal Self-Attention
            #' for Classifying Satellite Image Time Series"
            #
            # obtain the input parameters
            batch_size   <- x$shape[[1]]
            # seq_len is the size of the timeline
            seq_len      <- x$shape[[2]]

            # normalize the input layer
            # [batch_size x seq_len x in_channels:128]
            x <- self$in_layer_norm(x)

            # apply 1D conv to the reshaped input
            # convolution is performed in 3D shape [batch_size x in_channels:128 x seq_len]
            # and returns a 3D shape [batch_size x d_model:256 x seq_len]
            x <- self$inconv(x$permute(c(1, 3, 2)))
            # reshape the input again
            # to 3D shape [batch_size x seq_len x d_model:256]
            x <- x$permute(c(1, 3, 2))

            # Calculate the positional encoding
            # result is 3D shape [batch_size x seq_len x d_model:256]
            values <-  self$pos_encoding(x)

            # calculate multi-head attention
            # output is 3D shape [n_heads x  batch_size x d_model:256]
            enc_output <- self$attention_heads(
                values = values
            )
            # permute dimensions of the output
            # result is 3D shape [batch_size x n_heads x d_model:256]
            enc_output <- enc_output$permute(c(2, 1, 3))$contiguous()
            # reshape the output
            enc_output <- enc_output$view(c(batch_size, -1))

            # apply multilayer processor
            enc_output <- self$mlp(enc_output)
            # apply dropout
            enc_output <-  self$dropout(enc_output)
            # normalize output layer
            enc_output <- self$out_layer_norm(enc_output)

            return(enc_output)
        }
    )
}
#' @title Torch module for calculating attention from query, keys and values
#' @name .torch_scaled_dot_product_attention
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description Defines a torch module for temporal attention encoding.
#'
#' In order to calculate attentions with a query, as I said in the last article,
#' this function takes the dot product of query with the keys
#' and gets scores/weights for the values.
#' Each score/weight the relevance between the query and each key.
#' And you reweight the values with the scores/weights,
#' and take the summation of the reweighted values.
#'
#' This implementation is based on the code made available by Vivien Garnot
#' https://github.com/VSainteuf/lightweight-temporal-attention-pytorch
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#'
#' @param temperature               Weight score of the attention module.
#' @param attn_dropout              Dropout rate to be applied to the attention module.
#' @param query                     Query tensor.
#' @param keys                      Tensor with keys.
#' @param values                    Tensor with values.
#'
#' @return A list with the .
#'
.torch_scaled_dot_product_attention <- function() {
    torch::nn_module(
        classname = "scaled_dot_product_attention",
        initialize = function(temperature, attn_dropout = 0.1){
            self$temperature <-  temperature
            self$dropout <-  torch::nn_dropout(attn_dropout)
            self$softmax <- torch::nn_softmax(dim = 3)
        },
        forward = function(query, keys, values){
            # calculate the dot product between query and keys
            # query has 2D shape [(n_heads * batch_size) x d_k]
            # after unsqueezing a 3D shape [(n_heads * batch_size) x 1 x d_k]
            # keys has 3D shape [(n_heads * batch_size) x  seq_len x d_k]
            # after transpose a 3D shape [(n_heads * batch_size) x d_k x seq_len]
            attn <- torch::torch_matmul(
                query$unsqueeze(dim = 2),
                keys$transpose(dim0 = 2, dim1 = 3)
            )
            # attention tensor has 3D shape [(n_heads * batch_size) x 1 x seq_len]
            # weight the attention module
            attn <- attn / self$temperature
            # apply a software layer to the weighted query * keys product
            attn <- self$softmax(attn)
            # apply a dropout value to the attention tensor
            attn <- self$dropout(attn)
            # calculate the product attention * values
            # split_value <- dim_encoder %/% n_heads
            # attention tensor has 3D shape [(n_heads * batch_size) x 1 x seq_len]
            # values has 3D shape [(num_heads * batch_size) x seq_len x split_value]
            # output has a 3D shape [(num_heads * batch_size) x 1 x split_value]
            output <- torch::torch_matmul(attn, values)

            return(output)
        }
    )
}
#' @title Torch module for calculating multi-head attention
#' @name .torch_multi_head_attention
#'
#' @author Charlotte Pelletier, \email{charlotte.pelletier@@univ-ubs.fr}
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @author Felipe Souza, \email{lipecaso@@gmail.com}
#' @keywords internal
#' @description
#' In order to calculate attentions with a query,
#' this function takes the dot product of query with the keys
#' and gets scores/weights for the values.
#' Each score/weight the relevance between the query and each key.
#' And you reweight the values with the scores/weights,
#' and take the summation of the reweighted values.
#'
#' This implementation is based on the code made available by Vivien Garnot
#' https://github.com/VSainteuf/lightweight-temporal-attention-pytorch
#'
#' @references
#' Vivien Sainte Fare Garnot and Loic Landrieu,
#' "Lightweight Temporal Self-Attention
#' for Classifying Satellite Image Time Series", https://arxiv.org/abs/2007.00586
#'
#'
#' @param n_heads         Number of attention heads.
#' @param d_k             Dimension of key tensor.
#' @param d_in            Dimension of input values.
#'
#' @return An output encoder tensor.
#'
.torch_multi_head_attention <- function() {
    torch::nn_module(
        classname = "multi_head_attention",
        initialize = function(n_heads, d_k, d_in){
            self$n_heads <- n_heads
            self$d_k     <- d_k
            self$d_in    <- d_in

            # create a base vector for queries
            # shape [n_heads x d_k]
            self$Q <- torch::nn_parameter(
                torch::torch_zeros(c(n_heads, d_k), requires_grad = TRUE)
            )
            # initialization with a gaussian distribution
            torch::nn_init_normal_(self$Q, mean = 0, std = sqrt(2.0 / (d_k)))

            # d_in is equal to d_model:256
            # FC module for calculating keys
            self$fc_k <- torch::nn_linear(
                in_features  = d_in,
                out_features = n_heads * d_k
            )
            # initialization with a gaussian distribution
            torch::nn_init_normal_(self$fc_k$weight, mean = 0, std = sqrt(2.0 / (d_k)))

            # module to calculate attention by product of keys and queries.
            self$attention <- .torch_scaled_dot_product_attention()(
                temperature = sqrt(d_k)
            )
        },
        forward = function(values){
            d_k     <- self$d_k
            d_in    <- self$d_in
            n_heads <- self$n_heads

            # input values tensor is 3D [batch_size x seq_len x d_model:256]
            batch_size <- values$shape[[1]]
            seq_len    <- values$shape[[2]]

            # calculate the query tensor
            # concatenate a sequence of tensors to match input batch_size
            tensors <- purrr::map(seq_len(batch_size), function(i){
                return(self$Q)
            })
            # the query tensor has 3D shape [n_heads x batch_size x d_k]
            query <- torch::torch_stack(tensors, dim = 2)
            # drop a dimension in the query tensor
            # from 3D shape [n_heads x batch_size x d_k]
            # to 2D shape [(n_heads * batch_size) x d_k]
            query <- query$view(c(-1, d_k))

            # create the keys tensor by replicating the values tensor
            # keys tensor has 3D shape [batch_size x seq_len x d_model:256]
            # output tensor has 3D shape [batch_size x seq_len x (n_heads * d_k)]
            keys <- self$fc_k(values)
            # reshape the keys vector to 4D shape [batch_size, seq_len, n_heads, d_k]
            keys <- keys$view(c(batch_size, seq_len, n_heads, d_k))
            # permute shape of keys tensor
            # from 4D shape [batch_size, seq_len, n_heads, d_k]
            # to 4D shape [n_heads, batch_size, seq_len, d_k]
            keys = keys$permute(c(3, 1, 2, 4))$contiguous()
            # Reshape keys tensor to 3D [(n_heads * batch_size) x  seq_len x d_k]
            keys <- keys$view(c(-1, seq_len, d_k))

            # split the values tensor by attention heads
            dim_encoder <- values$shape[length(values$shape)]
            split_value <- dim_encoder %/% n_heads
            # reshape the values tensor by splitting
            # from 3D shape[batch_size x seq_len x dim_encoder:256]
            # to a 4D shape [n_heads x batch_size x seq_len x (dim_encoder %/% n_heads)]
            values <- torch::torch_stack(values$split(split_value, dim = -1))
            # reshape the values tensor
            # from 4D shape [n_heads x batch_size x seq_len x (dim_encoder %/% n_heads)]
            # to 3D shape [(n_heads * batch_size) x seq_len x (dim_encoder %/% n_heads)]
            values <- values$view(c(n_heads * batch_size, seq_len, -1))
            # calculate the attention values
            output <- self$attention(query, keys, values)
            # output has 3D shape [(num_heads * batch_size) x seq_len x (dim_encoder %/% n_heads)]
            # d_in = 256 and n_heads = 16, d_in %/% n_heads = 16
            # reshape to 4D shape [num_heads x batch_size x 1 x d_in %/% n_heads:16]
            output <-  output$view(c(n_heads, batch_size, 1, d_in %/% n_heads))
            # reshape to 3D shape [num_heads:16 x  batch_size x dim_encoder:256]
            output <- output$squeeze(dim = 3)
            return(output)
        }
    )
}



