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
#' referenced below. The particular implementation of the positional encoder
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
#' @param pixel         Input dimension of neural net.
#' @param output_dim        Output dimension of neural net.
#'
#' @return A linear tensor block.
#'

# Positional Encoder
.torch_positional_encoding <- torch::nn_module(
    classname = "positional_encoding",
    # timeline is a vector with the observation dates
    initialize = function(dim_encoder = 128, dates){

        # length of positional encoder is the length of dates vector
        max_len <- length(dates)
        # keep the dates vector
        self$dates <- dates
        # initialize the torch 'days' tensor
        days <- torch::torch_tensor(dates)
        days <- torch::torch_unsqueeze(days, 2)

        # Calculate the positional encoding p
        p <- torch::torch_zeros(max_len, d_e)
        div_term <-  torch::torch_exp(torch::torch_arange(1, dim_encoder, 2)
                                      * (-log(1000.0) / dim_encoder))
        p[ , seq(1, max_len, 2)] <- torch::torch_sin(days * div_term)
        p[ , seq(2, max_len, 2)] <- torch::torch_cos(days * div_term)
        p = p.unsqueeze(0)
        self$register_buffer('p', p)
    },
    forward = function(x){
        x = x + self$p
        return(x)
    }
)

.torch_temporal_attention_encoder <- torch::nn_module(
    classname = "torch_temporal_attention_encoder",

    initialize = function(dim_encoder, heads){
        self$dim_encoder <-  dim_enconder
        self$dim_k <-  dim_encoder %/% heads
        self$heads <-  heads
        self$pos_encoding <-  .torch_positional_encoding()
        self$fc1_q <- torch::nn_linear(128, 128)
        self$fc1_k <-  torch::nn_linear(128, 128)
        self$fc2   <- torch::nn_linear(128, 128)
        self$mlp3  <- MLP3()
    }
)




#
# def forward(self, x):  # [batch_size x seq_len x hidden_state:128]
#
#     batch_size, seq_len, hidden_state = x.size()
# e_p = self.pos_encoding(x)  # [batch_size x seq_len x hidden_state:128]
#
# # Queries
# q = self.fc1_q(e_p)         # [batch_size x seq_len x hidden_state:128]
# q_mean = torch.mean(q, 1)   # [batch_size x hidden_state:128]
# q_hat = self.fc2(q_mean).view(batch_size, self.h, self.d_k)     # [batch_size x num_heads x d_k]
# q_hat = q_hat.contiguous().view(-1, self.d_k).unsqueeze(1)      # [batch_size * num_heads x 1 x d_k]
#
# # Keys
# k = self.fc1_k(e_p)                                                 # [batch_size x seq_len x hidden_state:128]
# k = k.view(batch_size, seq_len, self.h, self.d_k)                   # [batch_size x seq_len x num_heads x d_k]
# k = k.permute(0, 2, 1, 3).contiguous().view(-1, seq_len, self.d_k)  # [batch_size * num_heads x seq_len x d_k]
#
# # Values (like original garnot code)
# v = e_p.repeat(self.h, 1, 1)    # [batch_size * num_heads x seq_len x hidden:128]
#
# # Attention
# attention_scores = q_hat.matmul(k.transpose(-2, -1)) / math.sqrt(self.d_k)      # [batch_size * num_heads x 1 x seq_len]
# attention_probs = F.softmax(attention_scores, dim=-1)               # [batch_size * num_heads x 1 x seq_len]
# attention_output = torch.matmul(attention_probs, v).squeeze()       # [batch_size * num_heads x hidden_state:128]
# attention_output = attention_output.contiguous().view(batch_size, self.h, -1)   # [batch_size x num_h x hidden_state:128]
# attention_output = attention_output.contiguous().view(batch_size, -1)           # [batch_size x hidden:512]
#
# # Output
# o_hat = self.mlp3(attention_output)                                 # [batch_size x hidden_state:128]
#
# return
