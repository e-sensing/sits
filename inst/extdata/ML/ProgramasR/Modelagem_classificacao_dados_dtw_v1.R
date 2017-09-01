
#install.packages(c('graphics', 'MASS', 'plyr', 'caret', 'e1071', 'glmnet', 
#                   'randomForest', 'neuralnet', 'gbm'))

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

rm(list=ls());

#--------- creating data ----------#

#dir_base <- "D:\\Alex\\Pesquisa\\LandUseChange\\IIASA_GLOBIOM\\SITS\\inst\\extdata\\examples\\PosProcessamentoDTW"
dir_base <- "//storage3/usuarios/AlexandreYwata/Pesquisa/LandUseChange/IIASA_GLOBIOM/SITS/inst/extdata/examples/PosProcessamentoDTW"

dir_dados <- paste0(dir_base, "\\Dados");
dir_prog <- paste0(dir_base, "\\ProgramasR")

source(paste0(dir_prog, "\\Modelagem_classificacao_funcoes_auxiliares.R"))

file_dados <- paste0(dir_dados, "\\mt_twdtw_distances.csv")

dados_originais <- read.csv(file_dados, stringsAsFactors=FALSE, header=TRUE, sep = ",");
str(dados_originais)

table(dados_originais$ref)
labels(table(dados_originais$ref))

dados <- dados_originais
table(dados$ref)

categorias <- labels(table(dados$ref))[[1]]
categorias

ynn <- class.ind(as.factor(dados$ref))
colunas_classes <- paste0('l', 1:length(categorias))
colnames(ynn) <- colunas_classes
dados <- cbind(dados, ynn)

dados[,colnames(dados) %in% categorias]

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

categorias_relabeled <- labels(table(revalue(dados_originais$ref, conv.lst, warn_missing = FALSE)))[[1]];
categorias_relabeled

dados <- dados[,colnames(dados) %in% c('ref', categorias, colunas_classes)]
dados <- rename(dados, c('ref'='categoria'))

dados$categorianum <- 0
for (i in 1:length(categorias)) { dados[dados$categoria == categorias[i],'categorianum'] <-  i}

table(dados$categoria)
table(dados$categorianum)

#------ splitting samples ---------#

set.seed(1)
out <- stratified(dados, c("categoria"), 3)

set.seed(2104);
trainIndex <- createDataPartition(dados$categoria, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dadosTrain <- dados[ trainIndex,]
dadosTest  <- dados[-trainIndex,]

#-------- definindo os modelos --------------#

nomes <- names(dados)
lognomes <- paste0('log(', nomes[!nomes %in% c('categoria', 'categorianum', colunas_classes)], ')'); 
paste(lognomes, collapse = " + ")

orinomes <- paste0(nomes[!nomes %in% c('categoria', 'categorianum', colunas_classes)]); 
paste(orinomes, collapse = " + ")

yneunets <- paste0(nomes[!nomes %in% c('categoria', 'categorianum', categorias)]); 
paste(yneunets, collapse = " + ")

formulann <- as.formula(paste0(paste(yneunets, collapse = " + "), " ~ ",
                        paste(orinomes, collapse = " + ")));
formulann

formula1 <- as.formula(paste("factor(categoria) ~ ", paste(lognomes, collapse = " + ")));
formula1

formula2 <- as.formula(paste("categorianum ~ ", paste(lognomes, collapse = " + ")));
formula2

yTrain <- data.matrix(dadosTrain[,1])
yTest <-  data.matrix(dadosTest[,1])

xTrain <- log(data.matrix(dadosTrain[,c(2:(length(categorias)+1))]))
xTest <-   log(data.matrix(dadosTest[,c(2:(length(categorias)+1))]))

#-------- rodando os modelos --------------#





# https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/

# categorias.nenet1 <- neuralnet(formulann, data = dadosTrain, 
#                                hidden = 1, 
#                                act.fct = "logistic", 
#                                linear.output = F)
# categorias.nenet1
# summary(categorias.nenet1)
# 
# categorias.nenet1.pred <- compute(categorias.nenet1, covariate = xTest)
# pr.nn_ <- categorias.nenet1.pred$net.result
# head(pr.nn_)
# pr.nn_2 <- max.col(pr.nn_)
# head(pr.nn_2)
# 
# original_values <- max.col(dadosTrain[, 17:31])
# 
# categorias.nenet1.pred$net.result
# 
# original_values <- max.col(categorias.nenet1.pred$net.result)
# original_values1 <- categorias.nenet1.pred$net.result







categorias.lda <- lda(formula1, data=dadosTrain)
#categorias.qda <- qda(formula1, data=dadosTrain)
categorias.mlr <- multinom(formula1, data=dadosTrain)

categorias.svm1 <- svm(formula1, data=dadosTrain, kernel = "radial", type="C-classification", epsilon = 0.1, cost = 100)
categorias.svm2 <- svm(formula1, data=dadosTrain, kernel = "linear", type="C-classification", epsilon = 0.1, cost = 100)
categorias.svm3 <- svm(formula1, data=dadosTrain, kernel = "polynomial", type="C-classification", epsilon = 0.1, cost = 100)
categorias.svm4 <- svm(formula1, data=dadosTrain, kernel = "sigmoid", type="C-classification", epsilon = 0.1, cost = 100)
categorias.svm5 <- svm(formula1, data=dadosTrain, kernel = "radial", type="C-classification", epsilon = 0.1, cost = 200)
categorias.svm6 <- svm(formula1, data=dadosTrain, kernel = "linear", type="C-classification", epsilon = 0.1, cost = 200)
categorias.svm7 <- svm(formula1, data=dadosTrain, kernel = "polynomial", type="C-classification", epsilon = 0.1, cost = 200)
categorias.svm8 <- svm(formula1, data=dadosTrain, kernel = "sigmoid", type="C-classification", epsilon = 0.1, cost = 200)

categorias.lasso <- cv.glmnet(y = factor(yTrain), x = xTrain, family="multinomial", alpha=1)
categorias.ridge <- cv.glmnet(y = factor(yTrain), x = xTrain, family="multinomial", alpha=0)
categorias.elnet <- cv.glmnet(y = factor(yTrain), x = xTrain, family="multinomial", alpha=.5)

categorias.lasso$lambda.min
categorias.ridge$lambda.min
categorias.elnet$lambda.min

categorias.rfore1 <- randomForest(y = factor(yTrain), x = xTrain, data=NULL, ntree=50, norm.votes=FALSE)
categorias.rfore2 <- randomForest(y = factor(yTrain), x = xTrain, data=NULL, ntree=200, norm.votes=FALSE)
categorias.rfore3 <- randomForest(y = factor(yTrain), x = xTrain, data=NULL, ntree=1000, norm.votes=FALSE)

categorias.gbm1 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=500,interaction.depth=4)
categorias.gbm2 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=500,interaction.depth=6)
categorias.gbm3 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=500,interaction.depth=10)
categorias.gbm4 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=500,interaction.depth=15)
categorias.gbm5 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=500,interaction.depth=20)
categorias.gbm6 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=500,interaction.depth=30)
categorias.gbm7 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=5000,interaction.depth=4)
categorias.gbm8 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=5000,interaction.depth=8)
categorias.gbm9 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=5000,interaction.depth=15)
categorias.gbm10 <- gbm(formula1, data=dadosTrain, distribution="multinomial", n.trees=5000,interaction.depth=20)

categorias.rfore1
categorias.rfore2
categorias.rfore3

categorias.lda
#categorias.qda
categorias.mlr
categorias.svm1
categorias.svm2
categorias.svm3
categorias.svm4
summary(categorias.lasso)
summary(categorias.ridge)
summary(categorias.elnet)

#----------- predicting classes 

categorias.lda.pred <- revalue(predict(categorias.lda, newdata = dadosTest)$class, conv.lst, warn_missing = FALSE);
#categorias.qda.pred <- revalue(predict(categorias.qda, newdata = dadosTest)$class, conv.lst, warn_missing = FALSE);
categorias.mlr.pred <- revalue(predict(categorias.mlr, newdata = dadosTest), conv.lst, warn_missing = FALSE);

categorias.svm1.pred <- revalue(predict(categorias.svm1, newdata = dadosTest), conv.lst, warn_missing = FALSE);
categorias.svm2.pred <- revalue(predict(categorias.svm2, newdata = dadosTest), conv.lst, warn_missing = FALSE);
categorias.svm3.pred <- revalue(predict(categorias.svm3, newdata = dadosTest), conv.lst, warn_missing = FALSE);
categorias.svm4.pred <- revalue(predict(categorias.svm4, newdata = dadosTest), conv.lst, warn_missing = FALSE);
categorias.svm5.pred <- revalue(predict(categorias.svm5, newdata = dadosTest), conv.lst, warn_missing = FALSE);
categorias.svm6.pred <- revalue(predict(categorias.svm6, newdata = dadosTest), conv.lst, warn_missing = FALSE);
categorias.svm7.pred <- revalue(predict(categorias.svm7, newdata = dadosTest), conv.lst, warn_missing = FALSE);
categorias.svm8.pred <- revalue(predict(categorias.svm8, newdata = dadosTest), conv.lst, warn_missing = FALSE);

categorias.lasso.pred <- revalue(factor(predict(categorias.lasso, s=categorias.lasso$lambda.min, newx=xTest, type='class')), conv.lst, warn_missing = FALSE);
categorias.ridge.pred <- revalue(factor(predict(categorias.ridge, s=categorias.ridge$lambda.min, newx=xTest, type='class')), conv.lst, warn_missing = FALSE);
categorias.elnet.pred <- revalue(factor(predict(categorias.elnet, s=categorias.elnet$lambda.min, newx=xTest, type='class')), conv.lst, warn_missing = FALSE);

categorias.rfore1.pred <- revalue(predict(categorias.rfore1, newdata = xTest, type = 'response'), conv.lst, warn_missing = FALSE); 
categorias.rfore2.pred <- revalue(predict(categorias.rfore2, newdata = xTest, type = 'response'), conv.lst, warn_missing = FALSE); 
categorias.rfore3.pred <- revalue(predict(categorias.rfore3, newdata = xTest, type = 'response'), conv.lst, warn_missing = FALSE); 

categorias.gbm1.pred <- predict(categorias.gbm1, newdata=dadosTest, n.trees=500, type="response");
categorias.gbm2.pred <- predict(categorias.gbm2, newdata=dadosTest, n.trees=500, type="response");
categorias.gbm3.pred <- predict(categorias.gbm3, newdata=dadosTest, n.trees=500, type="response");
categorias.gbm4.pred <- predict(categorias.gbm4, newdata=dadosTest, n.trees=500, type="response");
categorias.gbm5.pred <- predict(categorias.gbm5, newdata=dadosTest, n.trees=500, type="response");
categorias.gbm6.pred <- predict(categorias.gbm6, newdata=dadosTest, n.trees=500, type="response");
categorias.gbm7.pred <- predict(categorias.gbm7, newdata=dadosTest, n.trees=5000, type="response");
categorias.gbm8.pred <- predict(categorias.gbm8, newdata=dadosTest, n.trees=5000, type="response");
categorias.gbm9.pred <- predict(categorias.gbm9, newdata=dadosTest, n.trees=5000, type="response");
categorias.gbm10.pred <- predict(categorias.gbm10, newdata=dadosTest, n.trees=5000, type="response");

categorias.gbm1.pred <- revalue(categorias[max.col(data.frame(categorias.gbm1.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm2.pred <- revalue(categorias[max.col(data.frame(categorias.gbm2.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm3.pred <- revalue(categorias[max.col(data.frame(categorias.gbm3.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm4.pred <- revalue(categorias[max.col(data.frame(categorias.gbm4.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm5.pred <- revalue(categorias[max.col(data.frame(categorias.gbm5.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm6.pred <- revalue(categorias[max.col(data.frame(categorias.gbm6.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm7.pred <- revalue(categorias[max.col(data.frame(categorias.gbm7.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm8.pred <- revalue(categorias[max.col(data.frame(categorias.gbm8.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm9.pred <- revalue(categorias[max.col(data.frame(categorias.gbm9.pred))], conv.lst, warn_missing = FALSE); 
categorias.gbm10.pred <- revalue(categorias[max.col(data.frame(categorias.gbm10.pred))], conv.lst, warn_missing = FALSE); 

#------ classificando por menor distancia ---------#

categorias.dtw.pred <- predict(categorias.lda, newdata = dadosTest)$class
for (i in 1:nrow(dadosTest))
{
     min_dist <- 1e20;
     for (j in 1:length(categorias))
     {
         variavel <- categorias[j]
         
         if (dadosTest[i,variavel] < min_dist)
         {
             min_dist <- dadosTest[i,variavel]
             categorias.dtw.pred[i] <- variavel
         }
     }
}

dadosTest$categoriadtw <- revalue(categorias.dtw.pred, conv.lst, warn_missing = F)
categorias.dtw.pred <- revalue(categorias.dtw.pred, conv.lst, warn_missing = F)

#---- avaliando performance

categorias_ref <- revalue(factor(dadosTest$categoria), conv.lst, warn_missing = F)

table(categorias.dtw.pred, categorias_ref)
table(categorias.lda.pred, categorias_ref)
#table(categorias.qda.pred, categorias_ref)
table(categorias.mlr.pred, categorias_ref)

table(categorias.svm1.pred, categorias_ref)
table(categorias.svm2.pred, categorias_ref)
table(categorias.svm3.pred, categorias_ref)
table(categorias.svm4.pred, categorias_ref)
table(categorias.svm5.pred, categorias_ref)
table(categorias.svm6.pred, categorias_ref)
table(categorias.svm7.pred, categorias_ref)
table(categorias.svm8.pred, categorias_ref)

table(categorias.lasso.pred, categorias_ref)
table(categorias.ridge.pred, categorias_ref)
table(categorias.elnet.pred, categorias_ref)

table(categorias.rfore1.pred, categorias_ref)
table(categorias.rfore2.pred, categorias_ref)
table(categorias.rfore3.pred, categorias_ref)

table(categorias.gbm1.pred, categorias_ref)
table(categorias.gbm2.pred, categorias_ref)
table(categorias.gbm3.pred, categorias_ref)
table(categorias.gbm4.pred, categorias_ref)
table(categorias.gbm5.pred, categorias_ref)
table(categorias.gbm6.pred, categorias_ref)

mean(categorias.dtw.pred == categorias_ref)
mean(categorias.lda.pred == categorias_ref)
#mean(categorias.qda.pred == categorias_ref)
mean(categorias.mlr.pred == categorias_ref)

mean(categorias.svm1.pred == categorias_ref)
mean(categorias.svm2.pred == categorias_ref)
mean(categorias.svm3.pred == categorias_ref)
mean(categorias.svm4.pred == categorias_ref)
mean(categorias.svm5.pred == categorias_ref)
mean(categorias.svm6.pred == categorias_ref)
mean(categorias.svm7.pred == categorias_ref)
mean(categorias.svm8.pred == categorias_ref)

mean(categorias.lasso.pred == categorias_ref)
mean(categorias.ridge.pred == categorias_ref)
mean(categorias.elnet.pred == categorias_ref)

mean(categorias.rfore1.pred == categorias_ref)
mean(categorias.rfore2.pred == categorias_ref)
mean(categorias.rfore3.pred == categorias_ref)

mean(categorias.gbm1.pred == categorias_ref)
mean(categorias.gbm2.pred == categorias_ref)
mean(categorias.gbm3.pred == categorias_ref)
mean(categorias.gbm4.pred == categorias_ref)
mean(categorias.gbm5.pred == categorias_ref)
mean(categorias.gbm6.pred == categorias_ref)

#----------------------------------------------------------------------------#
#---- the end                                                            ----#
#----------------------------------------------------------------------------#
