# Universidad del Valle de Guatemala
# Minería de Datos - Sección 10
# Integrantes: Oscar Juárez, José Cifuentes, Luis Esturbán
# Fecha: 26/03/20

# HOJA DE TRABAJO 5: Redes bayesianas

# Setear directorio de trabajo
setwd("./")

# Importar librerías
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

# Datos a utilizar
set.seed(69)
porcentaje<-0.7
corte <- sample(nrow(data),nrow(data)*porcentaje)
train<-data[corte,]
test<-data[-corte,]

# Obtener vectores de columnas numéricas
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

# Hacer matriz de correlación
matriz_cor <- cor(data[,varNames])
matriz_cor
corrplot(matriz_cor)

# En la matriz de correlación, nos podemos dar cuenta que hay ciertas variables
# cuyo valor es muy cercano a cero. Esto quiere decir que no poseen correlación
# con la variable de SalePrice

# Por lo tanto, hacemos un segundo modelo con las variables que sí aportan al modelo
varNames <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr","GarageCars","grupoRespuesta")
# varNames <- c("LotArea","OverallQual", "BedroomAbvGr","GarageCars","grupoRespuesta")

modelo <- naiveBayes(as.factor(grupoRespuesta)~.,data=train[,varNames])
summary(modelo)


# Ahora, hacemos un vector sin la variable que queremos predecir
testVarNames <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr","GarageCars")

varNames2 <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr","grupoRespuesta")
testVarNames2 <- c("MSSubClass","LotArea","OverallQual","OverallCond","X1stFlrSF","BsmtFullBath","BedroomAbvGr")


# Usamos el modelo naive para predecir
predBayes <- predict(modelo, test[,testVarNames])

# ¿Que tan bien predijo el modelo? Usamos la matriz de confusión
confusionMatrix(predBayes, as.factor(test$grupoRespuesta))