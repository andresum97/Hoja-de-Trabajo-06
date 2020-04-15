#library(e1071)
#install.packages("dummy) Esto es para convertir todas las variables categ?ricas del set de datos
#install.packages("dummies") Este es para convertir solo una variable

library(caret)
library(dummies)
library(plyr)
library(dplyr)
library(e1071)
library(lattice)
library(rpart)
library(randomForest)
#Modelo de Regresi?n log?stica

setwd("C:/Users/Gustavo/Desktop/SEPTIMO SEMESTRE/MINERIA/HDT6/Hoja-de-Trabajo-06")
#setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Hoja-Trabajo-6/Hoja-de-trabajo-06")

porcentaje<-0.7
set.seed(123)
datos <- read.csv("train.csv", stringsAsFactors = FALSE)

trainImportantes <- datos[c("MSSubClass","LotFrontage","LotArea","OverallCond","YearBuilt","YearRemodAdd","X2ndFlrSF","FullBath","TotRmsAbvGrd","GarageCars","SalePrice")]
trainImportantes[is.na(trainImportantes)]<-0

km<-kmeans(trainImportantes,3)
trainImportantes$grupo<-km$cluster

g1<- trainImportantes[trainImportantes$grupo==1,]
g2<- trainImportantes[trainImportantes$grupo==2,]
g3<- trainImportantes[trainImportantes$grupo==3,]
trainImportantes$grupo <- mapvalues(trainImportantes$grupo, c(1,2,3), c("Intermedio","Barato","Caro"))

trainImportantes<-cbind(trainImportantes,dummy(trainImportantes$grupo,verbose = T))
colnames(trainImportantes)[13] <- "EsBarata"
colnames(trainImportantes)[14] <- "EsCara"
colnames(trainImportantes)[15] <- "EsIntermedia"

porcentaje<-0.7
corte <- sample(nrow(trainImportantes),nrow(trainImportantes)*porcentaje)
train<-trainImportantes[corte,]
test<-trainImportantes[-corte,]

#Queremos saber si una casa es cara o no
modelo<-glm(EsCara~., data = train[,c(1:10,14)],family = binomial(), maxit=100)

#-------------------------------------------------
# Regresion Logistica 
#-------------------------------------------------

##Modelo con todas las variables
pred<-predict(modelo,newdata = test[,1:10], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$EsCara),as.factor(prediccion))
#-------------------------------------------------------
#GRAFICA PARA ANALISIS DE DATOS
plot(modelo)
#-------------------------------------------------------
#GRAFICA DE REGRESION LOGISTICA
#-------------------------------------------------------
precios <- test$SalePrice
dat=as.data.frame(cbind(precios,prediccion))
plot(precios,prediccion,xlab="Precio de Casa",ylab="Probability of Ser Cara") 
g=glm(prediccion~precios,family=binomial,dat) 
curve(predict(g,data.frame(precios=x),type="resp"),add=TRUE) 
points(precios,fitted(g),pch=20)


#COMPARACION CON NAIVES BAYES
#------------------------------------------------------
modelo_naive<-naiveBayes(as.factor(EsCara)~.,data=trainImportantes)
predBayes<-predict(modelo_naive, newdata = test[,1:10])
confusionMatrix(table(predBayes,test$EsCara))

#COMPARACION CON ARBOL DE PREDICCION (CLASIFICACION)
#-----------------------------------------------------
modelo_class<-rpart(EsCara~.,trainImportantes,method = "class")
prediccion <- predict(modelo, newdata = test[,1:10])

#columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
#test1$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
#View(test1)
cfm<-confusionMatrix(table(test1$prediccion, test1$grupo))
cfm


#RANDOM FOREST
#------------------------------------------------------
modelo_RF<-randomForest(EsCara~.,data=trainImportantes)
prediccionRF1<-predict(modeloRF1,newdata = test[,1:10])
testCompleto<-test1
testCompleto$predRF<-round(prediccionRF1)
cfmRandomForest <- confusionMatrix(table(testCompleto$predRF, testCompleto$grupo))
cfmRandomForest
###############################################################################
#---------------------------------------------------------------------
#TODOS LOS DATOS
datos$grupo<-km$cluster

#PARA TRAIN TREE SON OTRAS VARIABLES
trainTree <- datos[c("LotArea","YearBuilt","YearRemodAdd","X2ndFlrSF","FullBath","KitchenAbvGr","GarageCars","grupo")]
porciento <- 70/100

trainRowsNumber<-sample(1:nrow(trainTree),porciento*nrow(trainTree))
train1<-trainTree[trainRowsNumber,]
test1<-trainTree[-trainRowsNumber,]

modeloRF1<-randomForest(grupo~.,data=train1)
prediccionRF1<-predict(modeloRF1,newdata = test1[,1:7])
testCompleto<-test1
testCompleto$predRF<-round(prediccionRF1)
cfmRandomForest <- confusionMatrix(table(testCompleto$predRF, testCompleto$grupo))
cfmRandomForest
#-------------------------------------------------------------------------------------
#ANALISIS CORPLOT
library(corrplot)

variables <- trainImportantes
variables$grupo <- NULL
matriz_cor <- cor(variables)
matriz_cor
corrplot(matriz_cor)
