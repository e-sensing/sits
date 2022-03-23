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
#' @param in_channels               Dimension of the positional encoder.
#' @param n_heads                   Number of attention heads.
#' @param n_neurons                 Dimensions of MLP that processes the
#'                                  output of the attention heads.
#' @param dropout_rate              Dropout_rate.
#' @param d_model                   Dimension of the feature space that
#'                                  the input tensors will projected after
#'                                  being processed by a fully connected layer.
#'
#' @return A linear tensor block.
#'
.torch_light_temporal_attention_encoder <- torch::nn_module(
    classname = "torch_temporal_attention_encoder",

    initialize = function(timeline,
                          in_channels = 128,
                          n_heads = 16,
                          n_neurons = c(256, 128),
                          dropout_rate = 0.2,
                          d_model = 256){
        # store parameters
        self$in_channels <-  in_channels
        self$n_heads     <-  n_heads
        # calculate the dimension of split enconder
        self$d_k <-  in_channels %/% n_heads
        # calculate the length of the time sequence
        self$seq_len <- length(timeline)
        # Do a 1D convolution on the input sequence
        self$d_model <- d_model
        self$inconv  <- torch::nn_sequential(
            torch::nn_conv1d(in_channels, self$d_model, 1),
            torch::nn_layer_norm(list(self$d_model, self$seq_len))
        )
        # obtain the positional encoding
        self$pos_encoding <-  .torch_positional_encoding(
            timeline = timeline,
            dim_encoder = in_channels
        )

        # input layer normalization
        self$in_layer_norm <- torch::nn_layer_norm(self$in_channels)

        # output layer normalization
        last_neuron <- n_neurons[length(n_neurons)]
        self$out_layer_norm <- torch::nn_layer_norm(last_neuron)

        # calculate the attention heads
        self$attention_heads = .torch_multi_head_attention(
            n_heads   = n_heads,
            d_k       = self$dim_k,
            d_in      = self$d_model
            )

        # multi-layer perceptron with batch norm and relu
        hidden_dims = n_neurons[-1]
        self$mlp  <- .torch_multi_linear_batch_norm_relu(
            input_dim = n_neurons[1],
            hidden_dims = hidden_dims
        )
    },
    forward = function(x){
        # Follows figure 2 of Garnot's paper
        # "Satellite Image Time Series Classification
        # with Pixel-Set Encoders and Temporal Self-Attention"
        #
        # obtain the input parameters
        batch_size   <- x$shape[[1]]
        # seq_len is the size of the timeline
        seq_len      <- x$shape[[2]]
        # in_channels is the size of the transformed input sequence
        in_channels  <- x$shape[[3]]

        # normalize the input layer
        # [batch_size x seq_len x in_channels:128]
        x <- self$in_layer_norm(x)
        # reshape the tensor
        # from [batch_size x seq_len x in_channels:128]
        # to [batch_size x in_channels:128 x seq_len]
        x <- x$permute(c(1,3,2))
        # Apply a 1D convolution to the permuted normalized input
        # shape [batch_size x in_channels:128 x seq_len]
        x <- self$inconv()
        # Reshape the tensor
        # from [batch_size x in_channels:128 x seq_len]
        # to [batch_size x seq_len x in_channels:128]
        x <- x$permute(c(1,3,2))

        # Calculate the positional encoding
        # result is 3D shape [batch_size x seq_len x in_channels:128]
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
        values = e_p$`repeat`(c(self$n_heads, 1, 1))

        # Multi-head self-attention
        # multiply values by product of query * key
        # attention_probs - 3D shape [(batch_size * n_heads) x 1 x seq_len]
        # v - 3D shape [(batch_size * num_heads) x seq_len x hidden:128]
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
        o_hat = self$mlp3(attention_output)
        return(o_hat)
    }
)
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
#' this function takes the dot product of ‘query’ with the ‘keys’
#' and gets scores/weights for the ‘values.’
#' Each score/weight the relevance between the ‘query’ and each ‘key’.
#' And you reweight the ‘values’ with the scores/weights,
#' and take the summation of the reweighted ‘values.
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
.torch_scaled_dot_product_attention <- torch::nn_module(
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

        return(list(output = output, attn = attn))
    }
)


.torch_multi_head_attention <- torch::nn_module(
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
        # change self$Q in-place
        torch::nn_init_normal_(self$Q, mean = 0, std = sqrt(2.0 / (d_k)))

        self$fc_k <- torch::nn_linear(d_in, n_heads * d_k)
        torch::nn_init_normal_(self$fc_k$weight, mean = 0, std = sqrt(2.0 / (d_k)))

        self$attention <- .torch_scaled_dot_product_attention(
            temperature = sqrt(d_k)
        )
    },
    forward = function(query, key, values){
        d_k     <- self$d_k
        d_in    <- self$d_in
        n_heads <- self$n_heads

        # query tensor is of
        batch_size <- query$size[[1]]
        seq_len    <- query$size[[2]]

        # calculate the query tensor
        # concatenate a sequence of tensors to match input batch_size
        tensors <- purrr::map(seq_len(batch_size), function(i){
            return(self$Q)
        })
        # the query tensor has shape [batch_size x n_heads x d_k]
        query <- torch::torch_stack(tensors, dim = 2)
        # drop a dimension in the query tensor
        # from 3D shape [n_heads x batch_size x d_k]
        # to 2D shape [(n_heads * batch_size) x d_k]
        query <- query$view(c(-1, d_k))

        # calculate the keys tensor
        # tensor has 4D shape [batch_size, seq_len, n_heads, d_k]
        keys = self$fc_k(values)$view(c(batch_size, seq_len, n_heads, d_k))
        # permute shape of keys tensor
        # from 4D shape [batch_size, seq_len, n_heads, d_k]
        # to 4D shape [n_heads, batch_size, seq_len, d_k]
        keys = keys$permute(c(3, 1, 2, 4))$contiguous()
        # Reshape keys tensor to 3D [(n_heads * batch_size) x  seq_len x d_k]
        keys <- keys$view(c(-1, seq_len, d_k))

        # calculate the values tensor
        # original tensor from positional encoding
        # has 3D shape [batch_size x seq_len x dim_encoder:128]
        dim_encoder <- values$shape[length(values$shape)]
        # reshape the values tensor
        # from 3D shape[batch_size x seq_len x  dim_encoder:128]
        split_value <- dim_encoder %/% n_heads
        # to 4D shape [split_value x seq_len x dim_encoder x  (batch_size %/% split_value)]
        values <- torch::torch_stack(values$split(split_value), dim = -1)
        # reshape the values tensor
        # from 4D shape [split_value x seq_len x dim_encoder x  (batch_size %/% split_value)]
        # to 3D shape [(num_heads * batch_size) x seq_len x split_value]
        values <- values$view(c(n_heads * batch_size, seq_len, -1))
        # calculate the attention values
        output_attn_lst <- self$attention(query, keys, values)
        # recover the attention tensor
        attn <- output_attn_lst$attn
        # reshape the attention tensor
        # from 3D shape [(n_heads * batch_size) x 1 x seq_len]
        # to 4D shape [n_heads x batch_size x 1 x seq_len]
        attn <- attn$view(c(n_heads, batch_size, 1, seq_len))
        # squeeze the attention tensor on the third dimension
        # result is a 3D shape [n_heads x batch_size x seq_len]
        attn <- attn$squeeze(dim = 3)
        #  retrieve the output tensor
        output <- output_attn_lst$output
        # output has 3D shape [(num_heads * batch_size) x seq_len x split_value]
        # d_in = 256 and n_heads = 16, d_in %/% n_heads = 16
        # reshape to 4D shape [num_heads, batch_size x 1 x dim_encoder]
        output = output$view(c(n_heads, batch_size, 1, d_in %/% n_heads))
        output = output$squeeze(dim = 3)

        return(list(output = output, attn = attn))
    }

)




