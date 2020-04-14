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
modelo<-glm(train$trainImportantesCaro~., data = train[,c(1:10,15)],family = binomial(), maxit=100)

#-------------------------------------------------
# Regresión Logistica 
#-------------------------------------------------

##Modelo con todas las variables
pred<-predict(modelo,newdata = test[,1:10], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$datosvirginica),as.factor(prediccion))

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1
# 0 27  2
# 1  1 15
# 
# Accuracy : 0.9333         
# 95% CI : (0.8173, 0.986)
# No Information Rate : 0.6222         
# P-Value [Acc > NIR] : 1.906e-06      
# 
# Kappa : 0.8565         
# Mcnemar's Test P-Value : 1              
# 
# Sensitivity : 0.9643         
# Specificity : 0.8824         
# Pos Pred Value : 0.9310         
# Neg Pred Value : 0.9375         
# Prevalence : 0.6222         
# Detection Rate : 0.6000         
# Detection Prevalence : 0.6444         
# Balanced Accuracy : 0.9233         
# 
# 'Positive' Class : 0  