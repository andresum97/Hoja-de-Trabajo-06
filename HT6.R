#library(e1071)
#install.packages("dummy) Esto es para convertir todas las variables categóricas del set de datos
#install.packages("dummies") Este es para convertir solo una variable

library(caret)
library(dummies)
library(plyr)
library(dplyr)

#Modelo de Regresión logística

setwd("C:/Users/Gustavo/Desktop/SEPTIMO SEMESTRE/MINERIA/HDT6/Hoja-de-Trabajo-06")
#setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Hoja-Trabajo-5/Hoja-de-trabajo-5")

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
# Regresión Logistica 
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
