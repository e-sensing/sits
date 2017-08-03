
#install.packages(c('graphics', 'MASS', 'plyr', 'caret', 'e1071', 'glmnet', 'randomForest'))

library(graphics);
library(MASS);
library(plyr);
library(caret);
library(nnet);
library(e1071);
library(glmnet);
library(randomForest);

rm(list=ls());

#--------- creating data ----------#

dir_base <- "D:\\Alex\\Pesquisa\\LandUseChange\\IIASA_GLOBIOM\\SITS\\PosProcessamentoDTW"
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

# categorias_relabel <- categorias
# categorias_relabel[2] <- 'Cotton'
# categorias_relabel[4] <- 'Cotton'
# categorias_relabel[5] <- 'Pasture'
# categorias_relabel[6] <- 'Pasture'
# categorias_relabel[7] <- 'Double_Cropping'
# categorias_relabel[8] <- 'Double_Cropping'
# categorias_relabel[10] <- 'Single_Cropping'
# categorias_relabel[11] <- 'Single_Cropping'
# categorias_relabel[12] <- 'Double_Cropping'
# categorias_relabel[13] <- 'Double_Cropping'
# categorias_relabel[14] <- 'Pasture'
# 
# dt.relabel <- data.frame(categoria = categorias, cat_agregada = categorias_relabel)
# dt.relabel

dados <- dados[,colnames(dados) %in% c('ref', categorias)]
dados <- rename(dados, c('ref'='categoria'))

#------ splitting samples ---------#

set.seed(2104);
trainIndex <- createDataPartition(dados$categoria, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dadosTrain <- dados[ trainIndex,]
dadosTest  <- dados[-trainIndex,]

#-------- definindo os modelos --------------#

# formula1 <- categoria ~ log(Cerrado) + log(Forest) + log(NonComerc_Cotton) +
#                         log(Pasture) + log(Soybean_Comerc2) + log(Soybean_Cotton) +
#                         log(Soybean_Fallow2) + log(Soybean_NonComerc1);

formula1 <- categoria ~ log(Cerrado) + log(Fallow_Cotton) + log(Forest) +
                        log(NonComerc_Cotton) + log(Pasture) + log(Pasture2) +
                        log(Soybean_Comerc1) + log(Soybean_Comerc2) +
                        log(Soybean_Cotton) + log(Soybean_Fallow1) +
                        log(Soybean_Fallow2) + log(Soybean_NonComerc1) +
                        log(Soybean_NonComerc2) + log(Soybean_Pasture) + log(Water);

yTrain <- data.matrix(dadosTrain[,1])
yTest <-  data.matrix(dadosTest[,1])

# xTrain <- log(data.matrix(dadosTrain[,c(2,3,4,5,6,7,8,9)]))
# xTest <-   log(data.matrix(dadosTest[,c(2,3,4,5,6,7,8,9)]))

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

categorias.lasso <- cv.glmnet(y = yTrain, x = xTrain, family="multinomial", alpha=1)
categorias.ridge <- cv.glmnet(y = yTrain, x = xTrain, family="multinomial", alpha=0)
categorias.elnet <- cv.glmnet(y = yTrain, x = xTrain, family="multinomial", alpha=.5)

# categorias.rfore <- randomForest(y = yTrain, x = xTrain, data=NULL, ntree=50, norm.votes=FALSE)

categorias.lasso$lambda.min
categorias.ridge$lambda.min
categorias.elnet$lambda.min

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

sum(diag(table(categorias.dtw.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.lda.pred, categorias_ref))) / nrow(dadosTest)
sum(diag(table(categorias.qda.pred, categorias_ref))) / nrow(dadosTest)
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

#----------------------------------------------------------------------------#
#---- the end                                                            ----#
#----------------------------------------------------------------------------#
