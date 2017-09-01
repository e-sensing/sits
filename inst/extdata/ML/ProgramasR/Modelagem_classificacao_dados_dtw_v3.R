
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

dir_base <- "~/sits/inst/extdata/ML"
#dir_base <- "//storage3/usuarios/AlexandreYwata/Pesquisa/LandUseChange/IIASA_GLOBIOM/SITS/inst/extdata/examples/PosProcessamentoDTW"

dir_dados <- paste0(dir_base, "/Dados");
dir_prog <- paste0(dir_base, "/ProgramasR")

file_dados <- paste0(dir_dados, "/mt_twdtw_distances.csv")

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

# conv.lst <- c("Fallow_Cotton"  = "Cotton",
#               "NonComerc_Cotton" = "Cotton",
#               "Pasture2" = "Pasture",
#               "Soybean_Comerc1" = "Double_Cropping",
#               "Soybean_Comerc2" = "Double_Cropping",
#               "Soybean_Cotton" = "Soybean_Cotton",
#               "Soybean_Fallow1" = "Single_Cropping",
#               "Soybean_Fallow2" = "Single_Cropping",
#               "Soybean_NonComerc1" = "Double_Cropping",
#               "Soybean_NonComerc2" = "Double_Cropping",
#               "Soybean_Pasture" = "Pasture",
#               "Water" = "Water",
#               "Cerrado" = "Cerrado",
#               "Pasture2" = "Pasture",
#               "Forest"   = "Forest")

conv.lst <- c("Fallow_Cotton"  = "Cotton",
              "NonComerc_Cotton" = "Cotton",
              "Pasture2" = "Pasture",
              "Soybean_Comerc1" = "Double_Cropping",
              "Soybean_Comerc2" = "Double_Cropping",
              "Soybean_Cotton" = "Double_Cropping",
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

formulann <- as.formula(paste0(paste(colunas_classes, collapse = " + "), " ~ ",
                               paste(orinomes, collapse = " + ")));
formulann

#-----------------------------------------------------------#
#--- linear discriminant analysis - lda                  ---#
#-----------------------------------------------------------#

dados$pred_lda <- ""

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    categorias.lda <- lda(formula1, data=dadosTrain)
    summary(categorias.lda)

    categorias.lda.pred <- as.character(predict(categorias.lda, newdata = dadosTest)$class)
    dados[dados$folds == k, 'pred_lda'] <- categorias.lda.pred
}

table(dados$class, dados$pred_lda)
mean(dados$class == dados$pred_lda)

#-----------------------------------------------------------#
#--- multinomial logit - mlr                             ---#
#-----------------------------------------------------------#

dados$pred_mlr <- ""

for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    categorias.mlr <- multinom(formula1, data=dadosTrain)
    summary(categorias.mlr)

    categorias.mlr.pred <- as.character(predict(categorias.mlr, newdata = dadosTest))
    dados[dados$folds == k, 'pred_mlr'] <- categorias.mlr.pred
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
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    categorias.svm.linear <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")],
                                 kernel = "linear",
                                 type="C-classification", epsilon = epsilon_opt, cost = cost_opt)
    summary(categorias.svm.linear)

    categorias.svm.linear.pred <- as.character(predict(categorias.svm.linear,
                                                       newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_linear'] <- categorias.svm.linear.pred
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
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    categorias.svm.radial <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")],
                                 kernel = "radial",
                                 type="C-classification", epsilon = epsilon_opt, cost = cost_opt)
    summary(categorias.svm.radial)

    categorias.svm.radial.pred <- as.character(predict(categorias.svm.radial,
                                                       newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_radial'] <- categorias.svm.radial.pred
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
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    categorias.svm.polynomial <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")],
                                     kernel = "polynomial",
                                     type="C-classification", epsilon = epsilon_opt, cost = cost_opt)
    summary(categorias.svm.polynomial)

    categorias.svm.polynomial.pred <- as.character(predict(categorias.svm.polynomial,
                                                           newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_polynomial'] <- categorias.svm.polynomial.pred
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
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    categorias.svm.sigmoid <- svm(formula1, data=dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")],
                                  kernel = "sigmoid",
                                  type="C-classification", epsilon = epsilon_opt, cost = cost_opt)
    summary(categorias.svm.sigmoid)

    categorias.svm.sigmoid.pred <- as.character(predict(categorias.svm.sigmoid,
                                                        newdata = dadosTest[,colnames(dadosTest) %in% c(list_refs, "class")]))
    dados[dados$folds == k, 'pred_svm_sigmoid'] <- categorias.svm.sigmoid.pred
}

table(dados$class, dados$pred_svm_sigmoid)
mean(dados$class == dados$pred_svm_sigmoid)

#-----------------------------------------------------------#
#--- ridge multinomial regression                        ---#
#-----------------------------------------------------------#

yDados <- data.matrix(dados[,"class"]); head(yDados)
xDados <- log(data.matrix(dados[,c(2:(length(list_refs)+1))])); head(xDados)

set.seed(2104)
categorias.ridge <- cv.glmnet(y = factor(yDados), x = xDados, family="multinomial", alpha=0, k = kfolds)
summary(categorias.ridge)
ridge_lambda_opt <- categorias.ridge$lambda.min; ridge_lambda_opt

dados$pred_ridge <- NA
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    yTrain <- data.matrix(dadosTrain[,"class"])
    yTest <-  data.matrix(dadosTest[,"class"])

    xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
    xTest <-   log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))

    categorias.ridge.pred <- predict(categorias.ridge, s=categorias.ridge$lambda.min, newx=xTest, type='class')
    dados[dados$folds == k, 'pred_ridge'] <- categorias.ridge.pred
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
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    yTrain <- data.matrix(dadosTrain[,length(list_refs)+4])
    yTest <-  data.matrix(dadosTest[,length(list_refs)+4])

    xTrain <- log(data.matrix(dadosTrain[,c(2:(length(list_refs)+1))]))
    xTest <-   log(data.matrix(dadosTest[,c(2:(length(list_refs)+1))]))

    categorias.rfore <- randomForest(y = factor(yTrain), x = xTrain, data=NULL,
                                     ntree=ntrees_opt, nodesize = nodesize_opt, norm.votes=FALSE)
    summary(categorias.rfore)

    categorias.rfore.pred <- as.character(predict(categorias.rfore, newdata = xTest, type = 'response'))
    dados[dados$folds == k, 'pred_rfore'] <- categorias.rfore.pred
}

table(dados$class, dados$pred_rfore)
mean(dados$class == dados$pred_rfore)

#-----------------------------------------------------------#
#--- gradient boosting with h2o                          ---#
#-----------------------------------------------------------#

h2o.init(nthreads = -1)

list_ntrees <- c(100, 200, 500)
list_depths <- c(10, 20, 30, 40)

cv_h2ogbm <- matrix(nrow = length(list_ntrees) * length(list_depths), ncol = 3)

counter <- 1
for (ntreesgbm in list_ntrees)
{
    for (depthgbm in list_depths)
    {
        dados$pred_h2ogbm <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]

            h2o_dadosTrain <- as.h2o(dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")])
            h2o_dadosTrain$class <- as.factor(h2o_dadosTrain$class)
            h2o_dadosTest <- as.h2o(dadosTest[,colnames(dadosTest) %in% list_refs])

            categorias.h2ogbm <- h2o.gbm(y = "class", x = list_refs,
                                         training_frame = h2o_dadosTrain, distribution = "multinomial",
                                         ntrees = ntreesgbm, max_depth = depthgbm);

            categorias.h2ogbm.pred <- h2o.predict(categorias.h2ogbm, newdata = h2o_dadosTest)$predict;

            dados[dados$folds == k, 'pred_h2ogbm'] <- as.character(as.vector(categorias.h2ogbm.pred));
        }

        cv_h2ogbm[counter, 1] <- ntreesgbm
        cv_h2ogbm[counter, 2] <- depthgbm
        cv_h2ogbm[counter, 3] <- mean(dados$class == dados$pred_h2ogbm)
        counter <- counter + 1
    }
}

cv_h2ogbm
ntrees_opt <- cv_h2ogbm[which.max(cv_h2ogbm[,3]),1]; ntrees_opt
depth_opt <- cv_h2ogbm[which.max(cv_h2ogbm[,3]),2]; depth_opt

#ntrees_opt <- 500; depth_opt <- 10

dados$pred_h2ogbm <- NA
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    h2o_dadosTrain <- as.h2o(dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")])
    h2o_dadosTrain$class <- as.factor(h2o_dadosTrain$class)
    h2o_dadosTest <- as.h2o(dadosTest[,colnames(dadosTest) %in% list_refs])

    categorias.h2ogbm <- h2o.gbm(y = "class", x = list_refs,
                                 training_frame = h2o_dadosTrain, distribution = "multinomial",
                                 ntrees = ntrees_opt, max_depth = depth_opt);

    categorias.h2ogbm.pred <- h2o.predict(categorias.h2ogbm, newdata = h2o_dadosTest)$predict;

    dados[dados$folds == k, 'pred_h2ogbm'] <- as.character(as.vector(categorias.h2ogbm.pred));
}

table(dados$class, dados$pred_h2ogbm)
mean(dados$class == dados$pred_h2ogbm)

h2o.shutdown()

#-----------------------------------------------------------#
#--- distributed random forests with h2o                 ---#
#-----------------------------------------------------------#

h2o.init(nthreads = -1)

list_ntrees <- c(100, 200, 500)
list_depths <- c(20, 30, 40)

cv_h2orfore <- matrix(nrow = length(list_ntrees) * length(list_depths), ncol = 3)

counter <- 1
for (ntreesrfore in list_ntrees)
{
    for (depthrfore in list_depths)
    {
        dados$pred_h2orfore <- NA
        for (k in 1:kfolds)
        {
            dadosTrain <- dados[dados$folds != k,]
            dadosTest <- dados[dados$folds == k,]

            h2o_dadosTrain <- as.h2o(dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")])
            h2o_dadosTrain$class <- as.factor(h2o_dadosTrain$class)
            h2o_dadosTest <- as.h2o(dadosTest[,colnames(dadosTest) %in% list_refs])

            categorias.h20rfore <- h2o.randomForest(y = "class", x = list_refs,
                                                    training_frame = h2o_dadosTrain,
                                                    ntrees = ntreesrfore, max_depth = depthrfore)

            categorias.h20rfore.pred <- h2o.predict(categorias.h20rfore, newdata = h2o_dadosTest)$predict;

            dados[dados$folds == k, 'pred_h2orfore'] <- as.character(as.vector(categorias.h20rfore.pred));
        }

        cv_h2orfore[counter, 1] <- ntreesrfore
        cv_h2orfore[counter, 2] <- depthrfore
        cv_h2orfore[counter, 3] <- mean(dados$class == dados$pred_h2orfore)
        counter <- counter + 1
    }
}

cv_h2orfore
ntrees_opt <- cv_h2orfore[which.max(cv_h2orfore[,3]),1]; ntrees_opt
depth_opt <- cv_h2orfore[which.max(cv_h2orfore[,3]),2]; depth_opt

#ntrees_opt <- 100; depth_opt <- 30

dados$pred_h2orfore <- NA
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    h2o_dadosTrain <- as.h2o(dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")])
    h2o_dadosTrain$class <- as.factor(h2o_dadosTrain$class)
    h2o_dadosTest <- as.h2o(dadosTest[,colnames(dadosTest) %in% list_refs])

    categorias.h20rfore <- h2o.randomForest(y = "class", x = list_refs,
                                            training_frame = h2o_dadosTrain,
                                            ntrees = ntrees_opt, max_depth = depth_opt)

    categorias.h20rfore.pred <- h2o.predict(categorias.h20rfore, newdata = h2o_dadosTest)$predict;

    dados[dados$folds == k, 'pred_h2orfore'] <- as.character(as.vector(categorias.h20rfore.pred));
}

table(dados$class, dados$pred_h2orfore)
mean(dados$class == dados$pred_h2orfore)

h2o.shutdown()

#-----------------------------------------------------------#
#--- deep learning with h2o                              ---#
#-----------------------------------------------------------#

h2o.init(nthreads = -1)

dados$pred_h2odpl <- NA
for (k in 1:kfolds)
{
    dadosTrain <- dados[dados$folds != k,]
    dadosTest <- dados[dados$folds == k,]

    h2o_dadosTrain <- as.h2o(dadosTrain[,colnames(dadosTrain) %in% c(list_refs, "class")])
    h2o_dadosTrain$class <- as.factor(h2o_dadosTrain$class)
    h2o_dadosTest <- as.h2o(dadosTest[,colnames(dadosTest) %in% list_refs])

    categorias.h2odpl <- h2o.deeplearning(y = "class", x = list_refs,
                                          training_frame = h2o_dadosTrain, distribution = "multinomial");

    categorias.h2odpl.pred <- h2o.predict(categorias.h2odpl, newdata = h2o_dadosTest)$predict;

    dados[dados$folds == k, 'pred_h2odpl'] <- as.character(as.vector(categorias.h2odpl.pred));
}

table(dados$class, dados$pred_h2odpl)
mean(dados$class == dados$pred_h2odpl)

h2o.shutdown()

#--------------------------------------------------------------------------#
#--- THE END                                                            ---#
#--------------------------------------------------------------------------#


