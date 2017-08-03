<<<<<<< HEAD:inst/extdata/PosProcessamentoDTW/ProgramasR/Modelagem_classificacao_dados_simulados.R

library(graphics);
library(MASS);
library(plyr);
library(caret);
library(nnet);
library(e1071);

rm(list=ls());

#--------- creating data ----------#

set.seed(2104);
categorias <- c('Soy-Sorgo', 'Corn', 'Forest', 'Pasture', 'Water')

nsamples <- c(200, 150, 200, 200, 150)
k <- length(nsamples); k

mu <- matrix(c(0.001, 0.3, 0.6, 0.4, 0.8,
               0.4, 0.001, 0.4, 0.4, 0.8,
               0.6, 0.3, 0.001, 0.4, 0.8,
               0.4, 0.3, 0.4, 0.001, 0.8,
               0.5, 0.6, 0.4, 0.4, 0.001), nrow = k, ncol = k, byrow = T)
mu
Sigma <- diag(k) + matrix(0.1, nrow = k, ncol = k)
Sigma

for (i in 1:k)
{
    u <- exp(mvrnorm(n=nsamples[i], mu[i,], Sigma));u
    colmeans <- colSums(u)/nsamples[i]
    print(colmeans)
    if (i == 1) 
    {
        dados <- data.frame(categoria = rep(categorias[i], nsamples[i]), u)
    } 
    else {
        dados <- rbind(dados, data.frame(categoria = rep(categorias[i], nsamples[i]), u))
    }
}
dados <- rename(dados, c('X1'='dist1', 'X2'='dist2', 'X3'='dist3', 
                         'X4'='dist4', 'X5'='dist5'))

#------ splitting samples ---------#

trainIndex <- createDataPartition(dados$categoria, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

dadosTrain <- dados[ trainIndex,]
dadosTest  <- dados[-trainIndex,]

#------ classificando por menor distancia ---------#

categorias.dtw.pred <- rep(categorias[1], nrow(dadosTest))
for (i in 1:nrow(dadosTest))
{
    min_dist <- dadosTest[i,c('dist1')];
    if (min_dist > dadosTest[i,c('dist2')]) 
    { 
        min_dist <- dadosTest[i,c('dist2')];
        categorias.dtw.pred[i] <- categorias[2]
    }
    if (min_dist > dadosTest[i,c('dist3')]) 
    { 
        min_dist <- dadosTest[i,c('dist3')];
        categorias.dtw.pred[i] <- categorias[3]
    }
    if (min_dist > dadosTest[i,c('dist4')]) 
    { 
        min_dist <- dadosTest[i,c('dist4')];
        categorias.dtw.pred[i] <- categorias[4]
    }
    if (min_dist > dadosTest[i,c('dist5')]) 
    { 
        min_dist <- dadosTest[i,c('dist5')];
        categorias.dtw.pred[i] <- categorias[5]
    }
}

#-------- rodando os modelos --------------#

categorias.qda <- qda(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                      data=dadosTrain)
categorias.lda <- lda(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                      data=dadosTrain)
categorias.mlr <- multinom(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                           data=dadosTrain)
categorias.svm1 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "radial", type="C-classification", k = 5)
categorias.svm2 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "linear", type="C-classification", k = 5)
categorias.svm3 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "polynomial", type="C-classification", k = 5)
categorias.svm4 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "sigmoid", type="C-classification", k = 5)

categorias.lda
categorias.qda
categorias.mlr
categorias.svm1
categorias.svm2
categorias.svm3
categorias.svm4

categorias.lda.pred <- predict(categorias.lda, newdata = dadosTest)$class
categorias.qda.pred <- predict(categorias.qda, newdata = dadosTest)$class
categorias.mlr.pred <- predict(categorias.mlr, newdata = dadosTest)
categorias.svm1.pred <- predict(categorias.svm1, newdata = dadosTest)
categorias.svm2.pred <- predict(categorias.svm2, newdata = dadosTest)
categorias.svm3.pred <- predict(categorias.svm3, newdata = dadosTest)
categorias.svm4.pred <- predict(categorias.svm4, newdata = dadosTest)

table(categorias.dtw.pred, dadosTest$categoria)
table(categorias.lda.pred, dadosTest$categoria)
table(categorias.qda.pred, dadosTest$categoria)
table(categorias.mlr.pred, dadosTest$categoria)
table(categorias.svm1.pred, dadosTest$categoria)
table(categorias.svm2.pred, dadosTest$categoria)
table(categorias.svm3.pred, dadosTest$categoria)
table(categorias.svm4.pred, dadosTest$categoria)

sum(diag(table(categorias.dtw.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.lda.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.qda.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.mlr.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm1.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm2.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm3.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm4.pred, dadosTest$categoria))) / nrow(dadosTest)

#----------------------------------------------------------------------------#
#---- the end                                                            ----#
#----------------------------------------------------------------------------#
=======

#install.packages(c('graphics', 'MASS', 'plyr', 'caret', 'e1071', 'glmnet'))

library(graphics);
library(MASS);
library(plyr);
library(caret);
library(nnet);
library(e1071);
library(glmnet);

rm(list=ls());

#--------- creating data ----------#

set.seed(2104);
categorias <- c('Soy-Sorgo', 'Corn', 'Forest', 'Pasture', 'Water')

nsamples <- c(200, 250, 200, 400, 150)
k <- length(nsamples); k

mu <- matrix(c(0.001, 0.3, 0.6, 0.4, 0.8,
               0.4, 0.001, 0.4, 0.4, 0.8,
               0.6, 0.3, 0.001, 0.4, 0.8,
               0.4, 0.3, 0.4, 0.001, 0.8,
               0.5, 0.6, 0.4, 0.4, 0.001), nrow = k, ncol = k, byrow = T)
mu
Sigma <- 0.1*(diag(k) + matrix(0.01, nrow = k, ncol = k))
Sigma

for (i in 1:k)
{
    u <- exp(mvrnorm(n=nsamples[i], mu[i,], Sigma));u
    colmeans <- colSums(u)/nsamples[i]
    print(colmeans)
    if (i == 1) 
    {
        dados <- data.frame(categoria = rep(categorias[i], nsamples[i]), u)
    } 
    else {
        dados <- rbind(dados, data.frame(categoria = rep(categorias[i], nsamples[i]), u))
    }
}
dados <- rename(dados, c('X1'='dist1', 'X2'='dist2', 'X3'='dist3', 
                         'X4'='dist4', 'X5'='dist5'))

#------ splitting samples ---------#

trainIndex <- createDataPartition(dados$categoria, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

dadosTrain <- dados[ trainIndex,]
dadosTest  <- dados[-trainIndex,]

#-------- rodando os modelos --------------#

categorias.qda <- qda(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                      data=dadosTrain)
categorias.lda <- lda(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                      data=dadosTrain)
categorias.mlr <- multinom(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                           data=dadosTrain)
categorias.svm1 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "radial", type="C-classification", k = 5)
categorias.svm2 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "linear", type="C-classification", k = 5)
categorias.svm3 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "polynomial", type="C-classification", k = 5)
categorias.svm4 <- svm(categoria ~ log(dist1)+log(dist2)+log(dist3)+log(dist4)+log(dist5), 
                       data=dadosTrain, kernel = "sigmoid", type="C-classification", k = 5)

categorias.lasso <- glmnet(y = data.matrix(dadosTrain[,1]), 
                           x = data.matrix(dadosTrain[,c(2,3,4,5,6)]), 
                           family="multinomial", alpha=1)
categorias.ridge <- glmnet(y = data.matrix(dadosTrain[,1]), 
                           x = data.matrix(dadosTrain[,c(2,3,4,5,6)]), 
                           family="multinomial", alpha=0)
categorias.elnet <- glmnet(y = data.matrix(dadosTrain[,1]), 
                           x = data.matrix(dadosTrain[,c(2,3,4,5,6)]), 
                           family="multinomial", alpha=.5)

categorias.lda
categorias.qda
categorias.mlr
categorias.svm1
categorias.svm2
categorias.svm3
categorias.svm4
summary(categorias.lasso)
summary(categorias.ridge)
summary(categorias.elnet)

categorias.lda.pred <- predict(categorias.lda, newdata = dadosTest)$class
categorias.qda.pred <- predict(categorias.qda, newdata = dadosTest)$class
categorias.mlr.pred <- predict(categorias.mlr, newdata = dadosTest)
categorias.svm1.pred <- predict(categorias.svm1, newdata = dadosTest)
categorias.svm2.pred <- predict(categorias.svm2, newdata = dadosTest)
categorias.svm3.pred <- predict(categorias.svm3, newdata = dadosTest)
categorias.svm4.pred <- predict(categorias.svm4, newdata = dadosTest)

categorias.lasso.pred <- predict(categorias.lasso, s=categorias.lasso$lambda.1se, 
                                 newx=data.matrix(dadosTest[,c(2,3,4,5,6)]))
categorias.ridge.pred <- predict(categorias.ridge, s=categorias.ridge$lambda.1se, 
                                 newx=data.matrix(dadosTest[,c(2,3,4,5,6)]))
categorias.elnet.pred <- predict(categorias.elnet, s=categorias.elnet$lambda.1se, 
                                 newx=data.matrix(dadosTest[,c(2,3,4,5,6)]))

#------ classificando por menor distancia ---------#

categorias.dtw.pred <- categorias.lda.pred
for (i in 1:nrow(dadosTest))
{
     categorias.dtw.pred[i] <- categorias[1]
     min_dist <- dadosTest[i,c('dist1')];
     if (min_dist > dadosTest[i,c('dist2')]) 
     { 
          min_dist <- dadosTest[i,c('dist2')];
          categorias.dtw.pred[i] <- categorias[2]
     }
     if (min_dist > dadosTest[i,c('dist3')]) 
     { 
          min_dist <- dadosTest[i,c('dist3')];
          categorias.dtw.pred[i] <- categorias[3]
     }
     if (min_dist > dadosTest[i,c('dist4')]) 
     { 
          min_dist <- dadosTest[i,c('dist4')];
          categorias.dtw.pred[i] <- categorias[4]
     }
     if (min_dist > dadosTest[i,c('dist5')]) 
     { 
          min_dist <- dadosTest[i,c('dist5')];
          categorias.dtw.pred[i] <- categorias[5]
     }
}

dadosTest$categoriadtw <- categorias.dtw.pred

#---- avaliando performance

table(categorias.dtw.pred, dadosTest$categoria)
table(categorias.lda.pred, dadosTest$categoria)
table(categorias.qda.pred, dadosTest$categoria)
table(categorias.mlr.pred, dadosTest$categoria)
table(categorias.svm1.pred, dadosTest$categoria)
table(categorias.svm2.pred, dadosTest$categoria)
table(categorias.svm3.pred, dadosTest$categoria)
table(categorias.svm4.pred, dadosTest$categoria)

sum(diag(table(categorias.dtw.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.lda.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.qda.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.mlr.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm1.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm2.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm3.pred, dadosTest$categoria))) / nrow(dadosTest)
sum(diag(table(categorias.svm4.pred, dadosTest$categoria))) / nrow(dadosTest)

#----------------------------------------------------------------------------#
#---- the end                                                            ----#
#----------------------------------------------------------------------------#
>>>>>>> be7ae2d8abd519b5ff237e573e1f1325068836b3:PosProcessamentoDTW/ProgramasR/Modelagem_classificacao_dados_simulados.R
