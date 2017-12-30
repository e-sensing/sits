
# cran <- getOption("repos")
# cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
# options(repos = cran)
# install.packages("mxnet")
# 
# library(mxnet)
# a <- mx.nd.ones(c(2,3), ctx = mx.cpu())
# b <- a * 2 + 1
# b

library(graphics);
library(MASS);
library(plyr);
library(caret);
library(nnet);
library(e1071);
library(glmnet);
library(randomForest);
library(neuralnet);
library(nnet);
library(gbm);
library(splitstackshape);
library(mxnet)

#---- packages for deep learning
library(deepnet);
library(h2o);
library(darch);
library(DEEPR);
library(deepboost);

rm(list=ls());

#---------------------------------------------#
#--- data used for estimations             ---#
#---------------------------------------------#

#dir_base <- "D:\\Alex\\Pesquisa\\LandUseChange\\IIASA_GLOBIOM\\DTWMachineLearning"
dir_base <- "c:\\Alex\\Pesquisa\\LandUseChange\\IIASA_GLOBIOM\\DTWMachineLearning"
#dir_base <- "//storage3/usuarios/AlexandreYwata/Pesquisa/LandUseChange/IIASA_GLOBIOM/SITS/inst/extdata/examples/PosProcessamentoDTW"

dir_dados <- paste0(dir_base, "\\Dados");
dir_prog <- paste0(dir_base, "\\ProgramasR")

file_dados <- paste0(dir_dados, "\\mt_twdtw_distances.csv")

dados_originais <- read.csv(file_dados, stringsAsFactors=FALSE, header=TRUE, sep = ",");
str(dados_originais)

table(dados_originais$ref)
labels(table(dados_originais$ref))

list_refs <- labels(table(dados_originais$ref))[[1]]
list_refs

dados <- dados_originais[,colnames(dados_originais) %in% c('ref', list_refs)]
dados$idrow <- 1:nrow(dados)
str(dados)

#---------------------------------------------#
#--- finding class with smallest distance  ---#
#---------------------------------------------#

dados$dtw_min_ref <- "";
for (i in 1:nrow(dados))
{
    min_dist <- 1e20;
    for (j in 1:length(list_refs))
    {
        variavel <- list_refs[j]
        
        if (dados[i,variavel] < min_dist)
        {
            min_dist <- dados[i,variavel]
            dados[i, 'dtw_min_ref'] <- variavel
        }
    }
}

#---------------------------------------------#
#--- relabeling classes                    ---#
#---------------------------------------------#

conv.lst <- c("Fallow_Cotton"  = "Cotton",
              "NonComerc_Cotton" = "Cotton",
              "Pasture2" = "Pasture",
              "Soybean_Comerc1" = "Double_Cropping",
              "Soybean_Comerc2" = "Double_Cropping",
              "Soybean_Cotton" = "Soybean_Cotton",
              "Soybean_Fallow1" = "Single_Cropping", 
              "Soybean_Fallow2" = "Single_Cropping", 
              "Soybean_NonComerc1" = "Double_Cropping",
              "Soybean_NonComerc2" = "Double_Cropping",
              "Soybean_Pasture" = "Pasture",
              "Water" = "Water",
              "Cerrado" = "Cerrado",
              "Pasture2" = "Pasture",
              "Forest"   = "Forest")

dados$class <- revalue(dados$ref, conv.lst, warn_missing = FALSE)
dados$dtw_min_class <- revalue(dados$dtw_min_ref, conv.lst, warn_missing = FALSE)

list_classes <- labels(table(dados$class))[[1]]
list_classes

#--------------------------------------------------#
#--- evaluating DTW min distance classification ---#
#--------------------------------------------------#

table(dados$dtw_min_class, dados$class)
mean(dados$dtw_min_class == dados$class)

#--------------------------------------------------#
#--- samples for k-fold cross-validation        ---#
#--------------------------------------------------#

kfolds <- 5
table(dados$class)

set.seed(2104)
dados$folds <- createFolds(dados$class, k = kfolds, returnTrain = FALSE, list = FALSE)

table(dados$class, dados$folds)

#-----------------------------------------------------------#
#--- creating numeric columns for some of the algorithms ---#
#-----------------------------------------------------------#

ynn <- class.ind(as.factor(dados$class))
colunas_classes <- paste0('l', 1:length(list_classes)); colunas_classes
colnames(ynn) <- colunas_classes
dados <- cbind(dados, ynn)

dados$classnum <- 0
for (i in 1:length(list_classes)) { dados[dados$class == list_classes[i],'classnum'] <-  i}

table(dados$class, dados$classnum)

#-----------------------------------------------------------#
#--- specifying the models to be estimated               ---#
#-----------------------------------------------------------#

nomes <- names(dados); nomes
lognomes <- paste0('log(', nomes[nomes %in% c(list_refs)], ')'); 
paste(lognomes, collapse = " + ")

orinomes <- paste0(nomes[nomes %in% c(list_refs)]); 
paste(orinomes, collapse = " + ")

formula1 <- as.formula(paste("factor(class) ~ ", paste(lognomes, collapse = " + ")));
formula1

formula2 <- as.formula(paste("classnum ~ ", paste(lognomes, collapse = " + ")));
formula2

formula3 <- as.formula(paste("factor(class) ~ ", paste(orinomes, collapse = " + ")));
formula3

formula4 <- as.formula(paste("classnum ~ ", paste(orinomes, collapse = " + ")));
formula4

formulann <- as.formula(paste0(paste(colunas_classes, collapse = " + "), " ~ ",
                               paste(orinomes, collapse = " + ")));
formulann

#-----------------------------------------------------------#
#--- linear discriminant analysis - lda                  ---#
#-----------------------------------------------------------#

dados$pred_lda <- ""
dados$entropy_lda <- NA
dados$gini_lda <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
        
    categorias.lda <- lda(formula1, data=dadosTrain)
    summary(categorias.lda)
    
    categorias.lda.pred <- as.character(predict(categorias.lda, newdata = dadosTest)$class)
    dados[dados$folds == k, 'pred_lda'] <- categorias.lda.pred

    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.lda.pred.probs <- predict(categorias.lda, newdata = dadosTest)$posterior
    p <- categorias.lda.pred.probs
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_lda'] <- entropia
    dados[dados$folds == k, 'gini_lda'] <- gini
}

table(dados$class, dados$pred_lda)
mean(dados$class == dados$pred_lda)

#-----------------------------------------------------------#
#--- multinomial logit - mlr                             ---#
#-----------------------------------------------------------#

dados$pred_mlr <- ""
dados$entropy_mlr <- NA
dados$gini_mlr <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    categorias.mlr <- multinom(formula1, data=dadosTrain)
    summary(categorias.mlr)
    
    categorias.mlr.pred <- as.character(predict(categorias.mlr, newdata = dadosTest))
    dados[dados$folds == k, 'pred_mlr'] <- categorias.mlr.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.mlr.pred.probs <- predict(categorias.mlr, newdata = dadosTest, type="probs")
    p <- categorias.mlr.pred.probs
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_mlr'] <- entropia
    dados[dados$folds == k, 'gini_mlr'] <- gini
}

table(dados$class, dados$pred_mlr)
mean(dados$class == dados$pred_mlr)

#-----------------------------------------------------------#
#--- support vector machines - svm - kernel linear       ---#
#-----------------------------------------------------------#

list_epsilons <- c(0.1, 0.3)
list_costs <- c(10, 25, 50, 100, 200, 500, 700)
cv_svm_linear <- matrix(nrow = length(list_epsilons) * length(list_costs), ncol = 3)
counter <- 1

for (costsvm in list_costs)
{
    for (epsilonsvm in list_epsilons)
    {
        dados$pred_svm_linear <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]
            
            categorias.svm.linear <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                         kernel = "linear", 
                                         type="C-classification", epsilon = epsilonsvm, cost = costsvm)     
            summary(categorias.svm.linear)
            
            categorias.svm.linear.pred <- as.character(predict(categorias.svm.linear, 
                                                       newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
            dados[dados$folds == k, 'pred_svm_linear'] <- categorias.svm.linear.pred
        }
        
        cv_svm_linear[counter, 1] <- costsvm
        cv_svm_linear[counter, 2] <- epsilonsvm
        cv_svm_linear[counter, 3] <- mean(dados$class == dados$pred_svm_linear)
        counter <- counter + 1
    }
}

cv_svm_linear
cost_opt <- cv_svm_linear[which.max(cv_svm_linear[,3]),1]; cost_opt
epsilon_opt <- cv_svm_linear[which.max(cv_svm_linear[,3]),2]; epsilon_opt

dados$pred_svm_linear <- NA
dados$entropy_svm_linear <- NA
dados$gini_svm_linear <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    categorias.svm.linear <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                 kernel = "linear", 
                                 probability = T,
                                 type="C-classification", epsilon = epsilon_opt, cost = cost_opt)     
    summary(categorias.svm.linear)
    
    categorias.svm.linear.pred <- as.character(predict(categorias.svm.linear, 
                                                       newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_linear'] <- categorias.svm.linear.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.svm.linear.probs <- predict(categorias.svm.linear, 
                                           probability = T,
                                           newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")])
    p <- attr(categorias.svm.linear.probs, "probabilities")
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_svm_linear'] <- entropia
    dados[dados$folds == k, 'gini_svm_linear'] <- gini
}

table(dados$class, dados$pred_svm_linear)
mean(dados$class == dados$pred_svm_linear)

#-----------------------------------------------------------#
#--- support vector machines - svm - kernel radial       ---#
#-----------------------------------------------------------#

list_epsilons <- c(0.1, 0.3)
list_costs <- c(10, 25, 50, 100, 200, 500, 700)
cv_svm_radial <- matrix(nrow = length(list_epsilons) * length(list_costs), ncol = 3)
counter <- 1

for (costsvm in list_costs)
{
    for (epsilonsvm in list_epsilons)
    {
        dados$pred_svm_radial <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]
            
            categorias.svm.radial <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                         kernel = "radial", 
                                         type="C-classification", epsilon = epsilonsvm, cost = costsvm)     
            summary(categorias.svm.radial)
            
            categorias.svm.radial.pred <- as.character(predict(categorias.svm.radial, 
                                                               newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
            dados[dados$folds == k, 'pred_svm_radial'] <- categorias.svm.radial.pred
        }
        
        cv_svm_radial[counter, 1] <- costsvm
        cv_svm_radial[counter, 2] <- epsilonsvm
        cv_svm_radial[counter, 3] <- mean(dados$class == dados$pred_svm_radial)
        counter <- counter + 1
    }
}

cv_svm_radial
cost_opt <- cv_svm_radial[which.max(cv_svm_radial[,3]),1]; cost_opt
epsilon_opt <- cv_svm_radial[which.max(cv_svm_radial[,3]),2]; epsilon_opt

dados$pred_svm_radial <- NA
dados$entropy_svm_radial <- NA
dados$gini_svm_radial <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    categorias.svm.radial <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                 kernel = "radial", 
                                 probability = T,
                                 type="C-classification", epsilon = epsilon_opt, cost = cost_opt)     
    summary(categorias.svm.radial)
    
    categorias.svm.radial.pred <- as.character(predict(categorias.svm.radial, 
                                                       newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_radial'] <- categorias.svm.radial.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.svm.radial.probs <- predict(categorias.svm.radial, 
                                           probability = T,
                                           newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")])
    p <- attr(categorias.svm.radial.probs, "probabilities")
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_svm_radial'] <- entropia
    dados[dados$folds == k, 'gini_svm_radial'] <- gini
}

table(dados$class, dados$pred_svm_radial)
mean(dados$class == dados$pred_svm_radial)

#-----------------------------------------------------------#
#--- support vector machines - svm - kernel polynomial   ---#
#-----------------------------------------------------------#

list_epsilons <- c(0.1, 0.3)
list_costs <- c(10, 25, 50, 100, 200, 500, 700)
cv_svm_polynomial <- matrix(nrow = length(list_epsilons) * length(list_costs), ncol = 3)
counter <- 1

for (costsvm in list_costs)
{
    for (epsilonsvm in list_epsilons)
    {
        dados$pred_svm_polynomial <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]
            
            categorias.svm.polynomial <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                             kernel = "polynomial", 
                                             probability = T,
                                             type="C-classification", epsilon = epsilonsvm, cost = costsvm)     
            summary(categorias.svm.polynomial)
            
            categorias.svm.polynomial.pred <- as.character(predict(categorias.svm.polynomial, 
                                                                   newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
            dados[dados$folds == k, 'pred_svm_polynomial'] <- categorias.svm.polynomial.pred
        }
        
        cv_svm_polynomial[counter, 1] <- costsvm
        cv_svm_polynomial[counter, 2] <- epsilonsvm
        cv_svm_polynomial[counter, 3] <- mean(dados$class == dados$pred_svm_polynomial)
        counter <- counter + 1
    }
}

cv_svm_polynomial
cost_opt <- cv_svm_polynomial[which.max(cv_svm_polynomial[,3]),1]; cost_opt
epsilon_opt <- cv_svm_polynomial[which.max(cv_svm_polynomial[,3]),2]; epsilon_opt

dados$pred_svm_polynomial <- NA
dados$entropy_svm_polynomial <- NA
dados$gini_svm_polynomial <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    categorias.svm.polynomial <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                     kernel = "polynomial", 
                                     probability = T,
                                     type="C-classification", epsilon = epsilon_opt, cost = cost_opt)     
    summary(categorias.svm.polynomial)
    
    categorias.svm.polynomial.pred <- as.character(predict(categorias.svm.polynomial, 
                                                           newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_polynomial'] <- categorias.svm.polynomial.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.svm.polynomial.probs <- predict(categorias.svm.polynomial, 
                                           probability = T,
                                           newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")])
    p <- attr(categorias.svm.polynomial.probs, "probabilities")
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_svm_polynomial'] <- entropia
    dados[dados$folds == k, 'gini_svm_polynomial'] <- gini
}

table(dados$class, dados$pred_svm_polynomial)
mean(dados$class == dados$pred_svm_polynomial)

#-----------------------------------------------------------#
#--- support vector machines - svm - kernel sigmoid      ---#
#-----------------------------------------------------------#

list_epsilons <- c(0.1, 0.3)
list_costs <- c(10, 25, 50, 100, 200, 500, 700)
cv_svm_sigmoid <- matrix(nrow = length(list_epsilons) * length(list_costs), ncol = 3)
counter <- 1

for (costsvm in list_costs)
{
    for (epsilonsvm in list_epsilons)
    {
        dados$pred_svm_sigmoid <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]
            
            categorias.svm.sigmoid <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                          kernel = "sigmoid", 
                                          type="C-classification", epsilon = epsilonsvm, cost = costsvm)     
            summary(categorias.svm.sigmoid)
            
            categorias.svm.sigmoid.pred <- as.character(predict(categorias.svm.sigmoid, 
                                                                newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
            dados[dados$folds == k, 'pred_svm_sigmoid'] <- categorias.svm.sigmoid.pred
        }
        
        cv_svm_sigmoid[counter, 1] <- costsvm
        cv_svm_sigmoid[counter, 2] <- epsilonsvm
        cv_svm_sigmoid[counter, 3] <- mean(dados$class == dados$pred_svm_sigmoid)
        counter <- counter + 1
    }
}

cv_svm_sigmoid
cost_opt <- cv_svm_sigmoid[which.max(cv_svm_sigmoid[,3]),1]; cost_opt
epsilon_opt <- cv_svm_sigmoid[which.max(cv_svm_sigmoid[,3]),2]; epsilon_opt

dados$pred_svm_sigmoid <- NA
dados$entropy_svm_sigmoid <- NA
dados$gini_svm_sigmoid <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    categorias.svm.sigmoid <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")], 
                                  kernel = "sigmoid", 
                                  probability = T,
                                  type="C-classification", epsilon = epsilon_opt, cost = cost_opt)     
    summary(categorias.svm.sigmoid)
    
    categorias.svm.sigmoid.pred <- as.character(predict(categorias.svm.sigmoid, 
                                                        newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_sigmoid'] <- categorias.svm.sigmoid.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.svm.sigmoid.probs <- predict(categorias.svm.sigmoid, 
                                               probability = T,
                                               newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")])
    p <- attr(categorias.svm.sigmoid.probs, "probabilities")
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_svm_sigmoid'] <- entropia
    dados[dados$folds == k, 'gini_svm_sigmoid'] <- gini
}

table(dados$class, dados$pred_svm_sigmoid)
mean(dados$class == dados$pred_svm_sigmoid)

#-----------------------------------------------------------#
#--- ridge multinomial regression                        ---#
#-----------------------------------------------------------#

yDados <- data.matrix(dados[,length(list_refs)+4]); head(yDados)
xDados <- log(data.matrix(dados[,c(2:(length(list_refs)+1))])); head(xDados)

set.seed(2104)
categorias.ridge <- cv.glmnet(y = factor(yDados), x = xDados, family="multinomial", alpha=0, k = kfolds)
summary(categorias.ridge)
ridge_lambda_opt <- categorias.ridge$lambda.min; ridge_lambda_opt

dados$pred_ridge <- NA
dados$entropy_ridge <- NA
dados$gini_ridge <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    yTrain <- data.matrix(dadosTrain[,length(list_refs)+4])
    yTest <-  data.matrix(dadosTest[,length(list_refs)+4])
    
    xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
    xTest <-   log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))
    
    categorias.ridge.pred <- predict(categorias.ridge, s=categorias.ridge$lambda.min, newx=xTest, type='class')
    dados[dados$folds == k, 'pred_ridge'] <- categorias.ridge.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.ridge.pred.probs <- predict(categorias.ridge, 
                                           s=categorias.ridge$lambda.min, newx=xTest, type='response')
    p <- categorias.ridge.pred.probs
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_ridge'] <- entropia
    dados[dados$folds == k, 'gini_ridge'] <- gini
}

table(dados$class, dados$pred_ridge)
mean(dados$class == dados$pred_ridge)

#-----------------------------------------------------------#
#--- lasso multinomial regression                        ---#
#-----------------------------------------------------------#

yDados <- data.matrix(dados[,length(list_refs)+4]); head(yDados)
xDados <- log(data.matrix(dados[,c(2:(length(list_refs)+1))])); head(xDados)

set.seed(2104)
categorias.lasso <- cv.glmnet(y = factor(yDados), x = xDados, family="multinomial", alpha=1.0, k = kfolds)
summary(categorias.lasso)
lasso_lambda_opt <- categorias.lasso$lambda.min; lasso_lambda_opt
categorias.lasso$lambda[which.min(categorias.lasso$cvm)]
min(categorias.lasso$cvm)

dados$pred_lasso <- NA
dados$entropy_lasso <- NA
dados$gini_lasso <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    yTrain <- data.matrix(dadosTrain[,length(list_refs)+4])
    yTest <-  data.matrix(dadosTest[,length(list_refs)+4])
    
    xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
    xTest <-   log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))
    
    categorias.lasso.pred <- predict(categorias.lasso, s=categorias.lasso$lambda.min, newx=xTest, type='class')
    dados[dados$folds == k, 'pred_lasso'] <- categorias.lasso.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.lasso.pred.probs <- predict(categorias.lasso, 
                                           s=categorias.lasso$lambda.min, newx=xTest, type='response')
    p <- categorias.lasso.pred.probs
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_lasso'] <- entropia
    dados[dados$folds == k, 'gini_lasso'] <- gini
}

table(dados$class, dados$pred_lasso)
mean(dados$class == dados$pred_lasso)

#-----------------------------------------------------------#
#--- elastic net multinomial regression                  ---#
#-----------------------------------------------------------#

yDados <- data.matrix(dados[,length(list_refs)+4]); head(yDados)
xDados <- log(data.matrix(dados[,c(2:(length(list_refs)+1))])); head(xDados)

list_alpha <- c(0.1, 0.3, 0.5, 0.7, 0.9)
cv_elnet_alpha <- matrix(nrow = length(list_alpha), ncol = 3)

contador <- 1
for (alphaelnet in list_alpha)
{
    set.seed(2104)
    categorias.elnet <- cv.glmnet(y = factor(yDados), x = xDados, family="multinomial", alpha=alphaelnet, k = kfolds)
    summary(categorias.elnet)
    elnet_lambda_opt <- categorias.elnet$lambda.min; elnet_lambda_opt
    
    cv_elnet_alpha[contador, 1] <- alphaelnet
    cv_elnet_alpha[contador, 2] <- elnet_lambda_opt
    cv_elnet_alpha[contador, 3] <- min(categorias.elnet$cvm)
    
    contador <- contador + 1
}

cv_elnet_alpha
alpha_opt <- cv_elnet_alpha[which.min(cv_elnet_alpha[,3]),1]; alpha_opt

set.seed(2104)
categorias.elnet <- cv.glmnet(y = factor(yDados), x = xDados, family="multinomial", alpha=alpha_opt, k = kfolds)
summary(categorias.elnet)
elnet_lambda_opt <- categorias.elnet$lambda.min; elnet_lambda_opt

dados$pred_elnet <- NA
dados$entropy_elnet <- NA
dados$gini_elnet <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    yTrain <- data.matrix(dadosTrain[,length(list_refs)+4])
    yTest <-  data.matrix(dadosTest[,length(list_refs)+4])
    
    xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
    xTest <-   log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))
    
    categorias.elnet.pred <- predict(categorias.elnet, s=categorias.elnet$lambda.min, newx=xTest, type='class')
    dados[dados$folds == k, 'pred_elnet'] <- categorias.elnet.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.elnet.pred.probs <- predict(categorias.elnet, 
                                           s=categorias.elnet$lambda.min, newx=xTest, type='response')
    p <- categorias.elnet.pred.probs
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p)) / log(ncol(p))
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_elnet'] <- entropia
    dados[dados$folds == k, 'gini_elnet'] <- gini
}

table(dados$class, dados$pred_elnet)
mean(dados$class == dados$pred_elnet)

#-----------------------------------------------------------#
#--- random forest                                       ---#
#-----------------------------------------------------------#

list_ntrees <- c(100, 500, 1000)
list_nodesizes <- c(1, 3, 5)
cv_rfore <- matrix(nrow = length(list_ntrees) * length(list_nodesizes), ncol = 3)

counter <- 1
for (ntreesrf in list_ntrees)
{
    for (nodesizerf in list_nodesizes)
    {
        dados$pred_rfore <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]
            
            yTrain <- data.matrix(dadosTrain[,length(list_refs)+4])
            yTest <-  data.matrix(dadosTest[,length(list_refs)+4])
            
            xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
            xTest <-   log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))
            
            categorias.rfore <- randomForest(y = factor(yTrain), x = xTrain, data=NULL, 
                                             ntree=ntreesrf, nodesize = nodesizerf, norm.votes=FALSE)    
            summary(categorias.rfore)
            
            categorias.rfore.pred <- as.character(predict(categorias.rfore, newdata = xTest, type = 'response'))
            dados[dados$folds == k, 'pred_rfore'] <- categorias.rfore.pred
        }
        
        cv_rfore[counter, 1] <- ntreesrf
        cv_rfore[counter, 2] <- nodesizerf
        cv_rfore[counter, 3] <- mean(dados$class == dados$pred_rfore)
        counter <- counter + 1
    }
}

cv_rfore
ntrees_opt <- cv_rfore[which.max(cv_rfore[,3]),1]; ntrees_opt
nodesize_opt <- cv_rfore[which.max(cv_rfore[,3]),2]; nodesize_opt

dados$pred_rfore <- NA
dados$entropy_rfore <- NA
dados$gini_rfore <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    yTrain <- data.matrix(dadosTrain[,length(list_refs)+4])
    yTest <-  data.matrix(dadosTest[,length(list_refs)+4])
    
    xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
    xTest <- log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))
    
    categorias.rfore <- randomForest(y = factor(yTrain), x = xTrain, data=NULL, 
                                     ntree=ntrees_opt, nodesize = nodesize_opt, norm.votes=FALSE)      
    summary(categorias.rfore)
    
    categorias.rfore.pred <- as.character(predict(categorias.rfore, newdata = xTest, type = 'response'))
    dados[dados$folds == k, 'pred_rfore'] <- categorias.rfore.pred
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.rfore.pred.probs <- predict(categorias.rfore, newdata = xTest, type='prob')
    p <- as.matrix(categorias.rfore.pred.probs)
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p) / log(ncol(p)), na.rm = T)
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_rfore'] <- entropia
    dados[dados$folds == k, 'gini_rfore'] <- gini
}

table(dados$class, dados$pred_rfore)
mean(dados$class == dados$pred_rfore)

#-----------------------------------------------------------#
#--- boosting                                            ---#
#-----------------------------------------------------------#

#list_ntrees <- c(500, 5000)
#list_depths <- c(5, 10, 15, 20, 30)

list_ntrees <- c(1000)
list_depths <- c(10, 20, 30)

cv_gbm <- matrix(nrow = length(list_ntrees) * length(list_depths), ncol = 3)

counter <- 1
for (ntreesgbm in list_ntrees)
{
    for (depthgbm in list_depths)
    {
        dados$pred_gbm <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]
            
            categorias.gbm <- gbm(formula1, data=dadosTrain[, colnames(dadosTrain) %in% c('class', list_refs)], 
                                  distribution="multinomial", 
                                  n.trees=ntreesgbm, interaction.depth=depthgbm)
            
            categorias.gbm.pred <- predict(categorias.gbm, newdata=dadosTest[,colnames(dadosTest) %in% c('class', list_refs)], 
                                           n.trees=ntreesgbm, type="response");
            
            dados[dados$folds == k, 'pred_gbm'] <- list_classes[max.col(data.frame(categorias.gbm.pred))]
        }
        
        cv_gbm[counter, 1] <- ntreesgbm
        cv_gbm[counter, 2] <- depthgbm
        cv_gbm[counter, 3] <- mean(dados$class == dados$pred_gbm)
        counter <- counter + 1
    }
}

cv_gbm
ntrees_opt <- cv_gbm[which.max(cv_gbm[,3]),1]; ntrees_opt
depth_opt <- cv_gbm[which.max(cv_gbm[,3]),2]; depth_opt

#ntrees_opt <- 500; depth_opt <- 10

dados$pred_gbm <- NA
dados$entropy_gbm <- NA
dados$gini_gbm <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    categorias.gbm <- gbm(formula1, data=dadosTrain[, colnames(dadosTrain) %in% c('class', list_refs)], 
                          distribution="multinomial", 
                          n.trees=ntrees_opt, interaction.depth=depth_opt)
    #summary(categorias.gbm)
    
    categorias.gbm.pred <- predict(categorias.gbm, newdata=dadosTest[,colnames(dadosTest) %in% c('class', list_refs)], 
                                   n.trees=ntrees_opt, type="response");
    
    dados[dados$folds == k, 'pred_gbm'] <- list_classes[max.col(data.frame(categorias.gbm.pred))]
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.gbm.pred.probs <- predict(categorias.gbm, newdata=dadosTest[,colnames(dadosTest) %in% c('class', list_refs)], 
                                         n.trees=ntrees_opt, type="link");
    t1 <- data.frame(categorias.gbm.pred.probs)
    p <- ((exp(t1) / rowSums(exp(t1))))
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p) / log(ncol(p)), na.rm = T)
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_gbm'] <- entropia
    dados[dados$folds == k, 'gini_gbm'] <- gini
}

table(dados$class, dados$pred_gbm)
mean(dados$class == dados$pred_gbm)

#-----------------------------------------------#
#--- deep learning - gbm                     ---#
#-----------------------------------------------#

h2o.init(nthreads = -1)

dados$pred_dplgbm <- NA
dados$entropy_dplgbm <- NA
dados$gini_dplgbm <- NA

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]
    
    h2o_dadosTrain <- as.h2o(dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")])
    h2o_dadosTrain$class <- as.factor(h2o_dadosTrain$class)
    h2o_dadosTest <- as.h2o(dadosTest[,colnames(dadosTest) %in% list_refs])
    
    categorias.dplgbm <- h2o.gbm(y = "class", x = list_refs, 
                                 training_frame = h2o_dadosTrain, distribution = "multinomial");
    categorias.dplgbm.pred <- h2o.predict(categorias.dplgbm, newdata = h2o_dadosTest)$predict;
    
    dados[dados$folds == k, 'pred_dplgbm'] <- as.character(as.vector(categorias.dplgbm.pred));
    
    #---- predição e calculo de incerteza (entropia e Gini)
    
    categorias.dpgbm.pred.probs <- h2o.predict(categorias.dplgbm, newdata = h2o_dadosTest);

    t1 <- as.data.frame(categorias.dpgbm.pred.probs)[,-1]
    p <- as.matrix(t1)
    
    print(sum(p))
    print(nrow(p))
    
    entropia <- - rowSums(p * log(p) / log(ncol(p)), na.rm = T)
    entropia
    
    gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
    gini 
    
    dados[dados$folds == k, 'entropy_dpgbm'] <- entropia
    dados[dados$folds == k, 'gini_dpgbm'] <- gini
}

table(dados$class, dados$pred_dplgbm)
mean(dados$class == dados$pred_dplgbm)

h2o.shutdown()

#--------------------------------------------------------#
#--- histograma dos valores dos ginis e das entropias ---#
#--------------------------------------------------------#

#--- entropy ---#

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

hist(dados$entropy_lda, breaks = 20, col = "green", main = "Entropy - LDA", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_mlr, breaks = 20, col = "blue", main = "Entropy - MLR", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_lasso, breaks = 20, col = "red", main = "Entropy - Lasso", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_ridge, breaks = 20, col = "orange", main = "Entropy - Ridge", 
     xlab = "Entropy", ylab = "Frequency")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

hist(dados$entropy_svm_linear, breaks = 20, col = "green", main = "Entropy - SVM Linear", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_svm_polynomial, breaks = 20, col = "blue", main = "Entropy - SVM Polynomial", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_svm_sigmoid, breaks = 20, col = "red", main = "Entropy - SVM Sigmoid", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_svm_radial, breaks = 20, col = "orange", main = "Entropy - SVM Radial", 
     xlab = "Entropy", ylab = "Frequency")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

hist(dados$entropy_elnet, breaks = 20, col = "green", main = "Entropy - Elnet", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_rfore, breaks = 20, col = "blue", main = "Entropy - Random Forest", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_gbm, breaks = 20, col = "red", main = "Entropy - Boosting", 
     xlab = "Entropy", ylab = "Frequency")

hist(dados$entropy_dpgbm, breaks = 20, col = "orange", main = "Entropy - DP Boosting", 
     xlab = "Entropy", ylab = "Frequency")

#--- Gini ----#

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

hist(dados$gini_lda, breaks = 20, col = "green", main = "Gini - LDA", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_mlr, breaks = 20, col = "blue", main = "Gini - MLR", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_lasso, breaks = 20, col = "red", main = "Gini - Lasso", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_ridge, breaks = 20, col = "orange", main = "Gini - Ridge", 
     xlab = "Gini", ylab = "Frequency")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

hist(dados$gini_svm_linear, breaks = 20, col = "green", main = "Gini - SVM Linear", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_svm_polynomial, breaks = 20, col = "blue", main = "Gini - SVM Polynomial", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_svm_sigmoid, breaks = 20, col = "red", main = "Gini - SVM Sigmoid", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_svm_radial, breaks = 20, col = "orange", main = "Gini - SVM Radial", 
     xlab = "Gini", ylab = "Frequency")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

hist(dados$gini_elnet, breaks = 20, col = "green", main = "Gini - Elnet", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_rfore, breaks = 20, col = "blue", main = "Gini - Random Forest", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_gbm, breaks = 20, col = "red", main = "Gini - Boosting", 
     xlab = "Gini", ylab = "Frequency")

hist(dados$gini_dpgbm, breaks = 20, col = "orange", main = "Gini - DP Boosting", 
     xlab = "Gini", ylab = "Frequency")

#--------------------------------------------------------#
#--- boxplots das entropias e do Gini por acerto      ---#
#--------------------------------------------------------#

dados$acerto_mlr <- dados$class == dados$pred_mlr

boxplot(dados$entropy_mlr ~ dados$acerto_mlr)

boxplot(dados$gini_mlr ~ dados$acerto_mlr)

#-----------------------------------------------------------#
#--- mxnet - neural networks                             ---#
#-----------------------------------------------------------#

dados$pred_ridge <- NA
dados$entropy_ridge <- NA
dados$gini_ridge <- NA

for (k in 1:kfolds)
{
  dadosTrain <- dados[dados$folds != k,]
  dadosTest <- dados[dados$folds == k,]
  
  ynTrain <- data.matrix(dadosTrain[,length(list_refs)+15])
  ynTest <-  data.matrix(dadosTest[,length(list_refs)+15])
  
  xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
  xTest <-   log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))
  
  categorias.ridge.pred <- predict(categorias.ridge, s=categorias.ridge$lambda.min, newx=xTest, type='class')
  dados[dados$folds == k, 'pred_ridge'] <- categorias.ridge.pred
  
  #---- predição e calculo de incerteza (entropia e Gini)
  
  categorias.ridge.pred.probs <- predict(categorias.ridge, 
                                         s=categorias.ridge$lambda.min, newx=xTest, type='response')
  p <- categorias.ridge.pred.probs
  
  print(sum(p))
  print(nrow(p))
  
  entropia <- - rowSums(p * log(p)) / log(ncol(p))
  entropia
  
  gini = (1 - rowSums(p * p)) / (1 - 1/ncol(p))
  gini 
  
  dados[dados$folds == k, 'entropy_ridge'] <- entropia
  dados[dados$folds == k, 'gini_ridge'] <- gini
}

table(dados$class, dados$pred_ridge)
mean(dados$class == dados$pred_ridge)


mx.set.seed(0)
model <- mx.mlp(xTrain, ynTrain, out_node=8)

table(ynTrain)


#--------------------------------------------------------------------------#
#--- THE END                                                            ---#
#--------------------------------------------------------------------------#


