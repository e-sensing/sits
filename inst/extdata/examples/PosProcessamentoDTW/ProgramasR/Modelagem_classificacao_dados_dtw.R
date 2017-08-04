
#install.packages(c('graphics', 'MASS', 'plyr', 'caret', 'e1071', 'glmnet', 
#                   'randomForest', 'neuralnet '))

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

rm(list=ls());

#--------- creating data ----------#

dir_base <- "D:\\Alex\\Pesquisa\\LandUseChange\\IIASA_GLOBIOM\\SITS\\inst\\extdata\\examples\\PosProcessamentoDTW"
dir_dados <- paste0(dir_base, "\\Dados");
dir_prog <- paste0(dir_base, "\\ProgramasR")

file_dados <- paste0(dir_dados, "\\mt_twdtw_distances.csv")

dados_originais <- read.csv(file_dados, stringsAsFactors=FALSE, header=TRUE, sep = ",");
str(dados_originais)

table(dados_originais$ref)
labels(table(dados_originais$ref))

# dados <- dados_originais[dados_originais$ref != 'Fallow_Cotton' 
#                          & dados_originais$ref != 'Soybean_NonComerc2' 
#                          & dados_originais$ref != 'Water'
#                          & dados_originais$ref != 'Soybean_Fallow1'
#                          & dados_originais$ref != 'Soybean_Pasture'
#                          & dados_originais$ref != 'Pasture2'
#                          & dados_originais$ref != 'Soybean_Comerc1',]

dados <- dados_originais
table(dados$ref)

categorias <- labels(table(dados$ref))[[1]]
categorias

ynn <- class.ind(as.factor(dados$ref))
colunas_classes <- paste0('l', 1:length(categorias))
colnames(ynn) <- colunas_classes
dados <- cbind(dados, ynn)

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

dados <- dados[,colnames(dados) %in% c('ref', categorias, colunas_classes)]
dados <- rename(dados, c('ref'='categoria'))

dados$categorianum <- 0
for (i in 1:length(categorias)) { dados[dados$categoria == categorias[i],'categorianum'] <-  i}

table(dados$categoria)
table(dados$categorianum)

#------ splitting samples ---------#

set.seed(2104);
trainIndex <- createDataPartition(dados$categoria, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dadosTrain <- dados[ trainIndex,]
dadosTest  <- dados[-trainIndex,]

#-------- definindo os modelos --------------#

n <- names(dados)
logn <- paste0('log(', n[!n %in% c('categoria', 'categorianum', colunas_classes)], ')'); logn
f1 <- as.formula(paste("l1 + l2 + l3 ~", 
                       paste(logn[!logn %in% c("l1","l2","l3")], collapse = " + ")))
f1

f1 <- as.formula(paste(paste(colunas_classes, collapse = " + "), 
                       "l1 + l2 + l3 ~", 
                       paste(logn[!logn %in% c("l1","l2","l3")], collapse = " + ")))
f1

formula1 <- factor(categoria) ~ log(Cerrado) + log(Fallow_Cotton) + log(Forest) +
                                log(NonComerc_Cotton) + log(Pasture) + log(Pasture2) +
                                log(Soybean_Comerc1) + log(Soybean_Comerc2) +
                                log(Soybean_Cotton) + log(Soybean_Fallow1) +
                                log(Soybean_Fallow2) + log(Soybean_NonComerc1) +
                                log(Soybean_NonComerc2) + log(Soybean_Pasture) + log(Water);

formula2 <- categorianum ~ log(Cerrado) + log(Fallow_Cotton) + log(Forest) +
                        log(NonComerc_Cotton) + log(Pasture) + log(Pasture2) +
                        log(Soybean_Comerc1) + log(Soybean_Comerc2) +
                        log(Soybean_Cotton) + log(Soybean_Fallow1) +
                        log(Soybean_Fallow2) + log(Soybean_NonComerc1) +
                        log(Soybean_NonComerc2) + log(Soybean_Pasture) + log(Water);

yTrain <- data.matrix(dadosTrain[,1])
yTest <-  data.matrix(dadosTest[,1])

xTrain <- log(data.matrix(dadosTrain[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]))
xTest <-   log(data.matrix(dadosTest[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]))

#-------- rodando os modelos --------------#

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





categorias.nenet1 <- neuralnet(formula2, data = dadosTrain, 
                               hidden = c(13, 10, 3), act.fct = "logistic", 
                               linear.output = F)

categorias.nenet1

categorias.nenet1.pred <- compute(categorias.nenet1, covariate = xTest)
categorias.nenet1.pred <- compute(categorias.nenet1, covariate = xTrain)

original_values <- max.col(categorias.nenet1.pred$net.result)
original_values1 <- categorias.nenet1.pred$net.result



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

#----------- predicting with neural networks 


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
table(categorias.rfore.pred, categorias_ref)

sum(diag(table(categorias.dtw.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.lda.pred, categorias_ref))) / nrow(dadosTest)
#sum(diag(table(categorias.qda.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.mlr.pred, categorias_ref))) / nrow(dadosTest)

sum(diag(table(categorias.svm1.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.svm2.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.svm3.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.svm4.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.svm5.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.svm6.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.svm7.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.svm8.pred, categorias_ref))) / nrow(dadosTest)

sum(diag(table(categorias.lasso.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.ridge.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.elnet.pred, categorias_ref))) / nrow(dadosTest)

sum(diag(table(categorias.rfore1.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.rfore2.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.rfore3.pred, categorias_ref))) / nrow(dadosTest)

#----------------------------------------------------------------------------#
#---- the end                                                            ----#
#----------------------------------------------------------------------------#
