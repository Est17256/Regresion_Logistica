# Universidad del Valle de Guatemala
# Miner铆a de Datos - Secci贸n 10
# Integrantes: Oscar Ju谩rez, Jos茅 Cifuentes, Luis Esturb谩n
# Fecha: 15/04/20

# HOJA DE TRABAJO 6: Regresion logistica

# Setear directorio de trabajo
setwd("./")

# Importar librer铆as
library(e1071)
library(caret)
library(corrplot)
library(caret)
library(dummies)

# Leer datos del csv
data <- read.csv("./Data/train.csv", stringsAsFactors = FALSE)

# Se hace una categoria para el precio de cada casa.
grupoRespuesta <- c()
for (value in data[,"SalePrice"]) {
  if (value <= 260400) {
    grupoRespuesta <- c(grupoRespuesta, "economica")
  } else if (value >= 410000) {
    grupoRespuesta <- c(grupoRespuesta, "cara") 
  } else {
    grupoRespuesta <- c(grupoRespuesta, "intermedia")
  }
}
data$grupoRespuesta <- grupoRespuesta

#Se crean las variables dicotomicas
data<-cbind(data,dummy(data$grupoRespuesta,verbose = T))

# Datos a utilizar
set.seed(69)
porcentaje<-0.7
corte <- sample(nrow(data),nrow(data)*porcentaje)
train<-data[corte,]
test<-data[-corte,]

# Obtener vectores de columnas num茅ricas
numericRows <- c()
varNames <- c()
for(name in colnames(data)){
  if(is.numeric(data[1,name]) 
     && name != "Id" && name != "LotFrontage"
     && name != "MasVnrArea" && name != "GarageYrBlt"
     && name != "TotalBsmtSF" && name != "GrLivArea"
  )
  {
    numericRows <- c(numericRows, which(colnames(data)==name))
    varNames <- c(varNames, name)
  }
}

# Hacer matriz de correlaci贸n
matriz_cor <- cor(data[,varNames])
matriz_cor
corrplot(matriz_cor)

# En la matriz de correlaci贸n, nos podemos dar cuenta que hay ciertas variables
# cuyo valor es muy cercano a cero. Esto quiere decir que no poseen correlaci贸n
# con la variable de SalePrice

# Por lo tanto, hacemos un segundo modelo con las variables que s铆 aportan al modelo
varNames <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr","GarageCars","grupoRespuesta")


#############################################################################################
modelo<-glm(datacara~., data = train[,varNames],family = binomial(), maxit=100)

################################################################################################
modelo <- naiveBayes(as.factor(grupoRespuesta)~.,data=train[,varNames])
summary(modelo)

data2<-data[,varNames]
# Ahora, hacemos un vector sin la variable que queremos predecir
testVarNames <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr","GarageCars")

varNames2 <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr","grupoRespuesta")
testVarNames2 <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr")


# Usamos el modelo naive para predecir
predBayes <- predict(modelo, test[,testVarNames])

# 驴Que tan bien predijo el modelo? Usamos la matriz de confusi贸n
confusionMatrix(predBayes, as.factor(test$grupoRespuesta))


#############################################################################################
varNames <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr","GarageCars","grupoRespuesta","datacara","dataeconomica","dataintermedia")
train2<-train[,varNames]
test2<-test[,varNames]

#Modelo logistico para datacara
modelo<-glm(datacara~., data = train2[,c(1:8,10)],family = binomial(), maxit=100)
pred<-predict(modelo,newdata = test2[,1:8], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(prediccion),as.factor(test2$datacara))

ggplot(data = test2[,1:8], aes(x = test2$datacara, y = prediccion)) +
  geom_point(aes(color = as.factor(prediccion)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  labs(title = "Regresi髇 log韘tica de Casas Caras",
       y = "Probabilidad") +
  theme(legend.position = "none")


#Modelo logistico para dataeconomica
modelo<-glm(dataeconomica~., data = train2[,c(1:8,11)],family = binomial(), maxit=100)
pred<-predict(modelo,newdata = test2[,1:8], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(prediccion),as.factor(test2$dataeconomica))

ggplot(data = test2[,1:8], aes(x = test2$dataeconomica, y = prediccion)) +
  geom_point(aes(color = as.factor(prediccion)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  labs(title = "Regresi髇 log韘tica de Casas Economicas",
       y = "Probabilidad") +
  theme(legend.position = "none")

#Modelo logistico para dataintermedia
modelo<-glm(dataintermedia~., data = train2[,c(1:8,12)],family = binomial(), maxit=100)
pred<-predict(modelo,newdata = test2[,1:8], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(prediccion),as.factor(test2$dataintermedia))

ggplot(data = test2[,1:8], aes(x = test2$dataintermedia, y = prediccion)) +
  geom_point(aes(color = as.factor(prediccion)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  labs(title = "Regresi髇 log韘tica de Casas Intermedias",
       y = "Probabilidad") +
  theme(legend.position = "none")
