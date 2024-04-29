##############################
# Limpiar espacio de trabajo #
##############################

rm(list=ls())

####################
####################
# Lectura de datos #
####################
####################

# Obtiene el directorio de trabajo actual
directorio_actual <- getwd()

setwd(directorio_actual)
train_table <- read.csv("datamart_morosidad_training.csv", fileEncoding="utf-8", stringsAsFactors = TRUE)
print(train_table)
test_table <- read.csv("datamart_morosidad_test.csv", fileEncoding="utf-8", stringsAsFactors = TRUE)
print(test_table)

###########################
###########################
# 1. Anàlisis descriptivo #
###########################
###########################

############################
# 1.1 Exploraciòn numèrica #
############################

summary(train_table)

install.packages("moments")
library(moments)

# Para usar las funciones de asimetría y curtosis

# An?lisis de las explicativas

descriptivoManual <- function(explanatory_Table){
  
  # Obtiene los nombres de las variables de la tabla de datos
  vars_names <- names(explanatory_Table)
  
  # Itera sobre cada nombre de variable
  for (i in vars_names){
    
    # Imprime el nombre de la variable
    print(paste0("Variable: ",i))
    
    # Imprime el tipo de variable (factor, numérico, etc.)
    print(paste0("Tipo de variable: ", class(explanatory_Table[, i])))
    
    # Verifica si la variable es de tipo factor
    if (class(explanatory_Table[, i])=="factor"){
      
      # Calcula la moda de la variable factor
      valores <- unique(explanatory_Table[, i])
      moda <- valores[which.max(tabulate(match(explanatory_Table[,i], valores)))]
      print(paste0("Moda: ", moda))
      
      # Imprime el número de niveles de la variable factor
      print(paste0("Nº de niveles: ", length(valores)))
      
      # Calcula y muestra el porcentaje de valores faltantes (NA)
      print(paste0("% de missings: ", 100*(sum(is.na(explanatory_Table[, i]))/length(explanatory_Table[, i])), "%"))
      
    } else{
      
      # Si la variable no es factor, calcula y muestra estadísticas descriptivas numéricas
      print(paste0("Mínimo: ", min(explanatory_Table[, i], na.rm = T)))
      print(paste0("Máximo: ", max(explanatory_Table[, i], na.rm = T)))
      print(paste0("Media: ", mean(explanatory_Table[, i], na.rm = T)))
      print(paste0("Mediana: ", median(explanatory_Table[, i], na.rm = T)))
      print(paste0("Desviación típica: ", sd(explanatory_Table[, i], na.rm = T)))
      print(paste0("Asimetría: ", skewness(explanatory_Table[, i], na.rm = T)))
      print(paste0("Curtosis: ", kurtosis(explanatory_Table[, i], na.rm = T)))
      
      # Calcula y muestra el porcentaje de valores faltantes (NA)
      print(paste0("% de missings: ", 100*(sum(is.na(explanatory_Table[, i]))/length(explanatory_Table[, i])), "%"))
    }
    
    # Imprime una línea divisoria para separar la salida de cada variable
    print("*********************************************")
  }
}


descriptivoManual(train_table[, c("Age", "Avgexp", "Income", "Inc_per", "Cur_add", "Active")])

###########################
# 1.2 Exploracion grafica #
###########################

class(train_table)
# Histograma para variables continuas

hist(train_table$Avgexp, col="purple", border = F,
     main = "Histograma de Avgexp del cliente",
     xlab = "Valor", ylab = "Frecuencia")

hist(train_table$Age, col="purple", border = F,
     main = "Age",
     xlab = "Valor", ylab = "Frecuencia")

hist(train_table$Income, col="purple", border = F,
     main = "Income",
     xlab = "Valor", ylab = "Frecuencia")

hist(train_table$Inc_per, col="purple", border = F,
     main = "Inc_per",
     xlab = "Valor", ylab = "Frecuencia")

hist(train_table$Cur_add, col="purple", border = F,
     main = "Curr_add",
     xlab = "Valor", ylab = "Frecuencia")

hist(train_table$Active, col="purple", border = F,
     main = "Active",
     xlab = "Valor", ylab = "Frecuencia")

# Box-Plot Graficos

boxplot(train_table$Avgexp, col = "purple", main = "Boxplot de Avgexp")
boxplot(train_table$Inc_per, col = "purple", main = "Boxplot de Inc_per")

# Matriz de correlacion entre las variables continuas
# permite estudiar mejor las multicolinealidades para mejor
# ajuste de los modelos de regresión lineal

correlationMatrix<-cor(train_table[, c("Avgexp", "Age", "Income", "Inc_per")], method = c("pearson"))

graphics.off()

install.packages("corrplot")
library(corrplot)
install.packages("dplyr")
library(dplyr)
install.packages("ggcorrplot")
library(ggcorrplot)


correlationMatrix<-cor(train_table[c("Avgexp", "Age", "Income", "Inc_per", "Active", "Cur_add", "Ownrent", "Selfempl", "Exp_Inc", "Depndt", "Major")], method = c("pearson"))
corrplot(correlationMatrix, method="number", type="upper", tl.cex=0.5)


#ntcorrplot(matrizCorrelacion, method="number", type="upper",tl.cex=0.5)

# Se debe estudiar la distribución de las variables de forma relacionada al target.
# Se debe de brindar gráficos que se muestros estos



# Convertir en factor el target.

train_table$default <- as.factor(train_table$default)

#Se tiene como factor de tipo ordinal la variable número

train_table$Depndt <- as.factor(train_table$Depndt)

#Mostrar casos en la distribución en base de cada clase del
# target de la variable solo para variables continuas




train_table$Depndt<-as.factor(train_table$Depndt)
train_table$default<-as.factor(train_table$default)


# Usar barplot

tablaFrecuencia<-table(train_table$default, train_table$Active)
barplot(tablaFrecuencia, col = c('springgreen1','purple'), border = F)

tablaFrecuencia<-table(train_table$default, train_table$Depndt)
barplot(tablaFrecuencia, col = c('springgreen1','purple'), border = F)

###############
###############
# 2. Muestreo #
###############
###############

####################
# 2.1 Bajomuestreo #
####################

table(train_table$default)
set.seed(12345)

#Generar un muestro estratificado con respecto a la variable objetivo teniendo
# la proporcion que se desea de 0s y 1s



undersampling <- function(data, objective_var, ones_proportion_wish){
  
  `1s_counts` <- nrow(subset(data, objective_var==1))
  `0s_counts` <- round((`1s_counts`/ones_proportion_wish)-`1s_counts`)
  
  `0sstable` <- subset(data, objective_var==0)
  zeros <- sample(nrow(`0sstable`), `0s_counts`, replace = FALSE)
  
  return(rbind(data[variableObjetivo==1,], `0sstable`[ceros,]))
}

undersampling_0.5 <- undersampling(train_table, train_table$default, 1/2)
table(undersampling_0.5$default)
undersampling_0.33 <- undersampling(train_table, train_table$default, 1/3)
table(undersampling_0.33$default)

#####################
# 2.2 Sobremuestreo #
#####################

ones_table <- subset(train_table, train_table$default==1)
zeros_table <- subset(train_table, train_table$default==0)

# Aplicar remplazamiento para clonar los casos 1

ones_clones <- sample(nrow(ones_table), nrow(zeros_table), replace=TRUE)
ones_table <- ones_table[ones_clones,]
sobremuestreo_0_5 <- rbind(zeros_table, ones_table)
table(sobremuestreo_0_5$default)

############################
############################
# 3. Manipulaci?n de datos #
############################
############################

###############################
# 3.1 Tratamiento de outliers #
###############################

# Aplicar la técnica del logaritmo o aplicar los perceptiles en los extremos






percentile_Avgexp <- quantile(train_table$Avgexp, c(0.90, 0.95, 0.97, 0.99, 1), na.rm = T)
print(percentile_Avgexp)

percentilesInc_per <- quantile(train_table$Inc_per, c(0.90, 0.95,0.97, 0.99, 1), na.rm = T)
print(percentilesInc_per)
without_outliders_trainTrable <- train_table[train_table$Avgexp<=percentile_Avgexp[[3]] & train_table$Inc_per<=percentilesInc_per[[3]],]
dim(train_table)
dim(without_outliders_trainTrable)
# N?mero de registros eliminados:

(nrow(train_table)-nrow(without_outliders_trainTrable))

hist(train_table$Avgexp, col="purple", border = F,
     main = "Histograma de Avgexp del cliente",
     xlab = "Valor", ylab = "Frecuencia")

hist(without_outliders_trainTrable$Avgexp, col="purple", border = F,
     main = "Histograma de Avgexp del cliente",
     xlab = "Valor", ylab = "Frecuencia")

hist(train_table$Inc_per, col="purple", border = F,
     main = "Histograma de Inc_per del cliente",
     xlab = "Valor", ylab = "Frecuencia")

hist(without_outliders_trainTrable$Inc_per, col="purple", border = F,
     main = "Histograma de Inc_per del cliente",
     xlab = "Valor", ylab = "Frecuencia")


###############################
# 3.2 Tratamiento de missings #
###############################

# No se observan missings en los datos

###################
###################
# 4. Modelizaci?n #
###################
###################

###########################
# 4.1 Regresi?n log?stica #
###########################

# Ajustamos con una estrategia de selecci?n por pasos

# Modelo de partida: solo t?rmino intercept

model_logistReg_Interc <- glm(default ~ 1, data=without_outliders_trainTrable, family=binomial(link="logit") )

# Modelo m?s complejo a generar: con todas las variables
# No vamos a tener en cuenta Exp_Inc x su correlacion con Avgexp
model_logist_All <- glm(default ~ Avgexp+Age+Income+Inc_per+Active+Cur_add+Ownrent+Selfempl+Depndt+Major, data=without_outliders_trainTrable, family=binomial(link="logit") )

# Se recorren todos los modelos comprendidos entre ellos
# en ambos sentidos (permitiendo la entrada y salida de variables)
# El objetivo es minimizar el AIC

modelo.regresionLogistica.stepwise <- step(model_logistReg_Interc, scope=list(lower=model_logistReg_Interc,
                                                                              upper=model_logist_All), direction="both")
modelo.regresionLogistica.stepwise

# Observamos los coeficientes del modelo definitivo
# para validar si son significativos y responden a
# la l?gica de negocio esperada

# install.packages("lmtest")
library(lmtest)

# Adem?s de las estimaciones, queremos ver su significatividad (p-valores)

coeftest(modelo.regresionLogistica.stepwise)

##################################################
##################################################
# 5. Valoraci?n: ROC, Cobertura, Precisi?n, LIFT #
##################################################
##################################################

# Aplicamos la predicci?n sobre la tabla de test

predictTest.logisticaStepwise <- predict(modelo.regresionLogistica.stepwise, 
                                         newdata=test_table,
                                         type="response")

# Se comparan los modelos ajustados en t?rminos de curva ROC

install.packages("gplots")
library(gplots)

install.packages("ROCR")
library(ROCR)

# Adjuntamos a la predicci?n el dato real para contruir la curva ROC

predict_TestAux_logisticStepwise <- prediction(predictTest.logisticaStepwise,
                                               test_table$default,
                                               label.ordering = NULL)

roc_CurveTest_logisticStepwise <- performance(predict_TestAux_logisticStepwise, "tpr", "fpr")
plot(roc_CurveTest_logisticStepwise, main="Curva ROC", colorize=TRUE)
# L?nea base
abline(a=0,b=1,col="black")

auc_ROC<-performance(predict_TestAux_logisticStepwise, "auc")
auc_ROC@y.values[[1]]



# Otros gr?ficos

# Gr?fico de precisi?n

precision<-performance(predict_TestAux_logisticStepwise, "prec", "rpp")
plot(precision, main="Gr?fico de precisi?n", colorize=T)
prior=sum(train_table$default==1)/length(train_table$default)
print(prior)
# L?nea base
abline(a=prior,b=0,col="black")

# Gr?fico de cobertura

coverage<-performance(predict_TestAux_logisticStepwise, "rec", "rpp")
plot(coverage, main="Gráfico de cobertura", colorize=T)
# L?nea base
abline(a=0,b=1,col="black")

# Gr?fico de lift

lift<-performance(predict_TestAux_logisticStepwise, "lift", "rpp")
plot(lift, main="Gr?fico lift", colorize=T)
# L?nea base
abline(a=1,b=0,col="black")

#########################
# 4.2 ?rbol de decisi?n #
#########################

##################################
# Basados en test de dependencia #
##################################

install.packages("partykit")
library(partykit)

`dependencies tree.fit` <- ctree(default~Avgexp+Age+Income+Inc_per+Active+Cur_add+Ownrent+Selfempl+Depndt+Major,
                                 data=train_table, control=ctree_control(alpha=0.01, maxdepth=3, minbucket = 30))

# Dibujar el ?rbol creado

graphics.off()
plot(`dependencies tree.fit`)

# Si se quisiera predecir
# test.arbolDependence<-predict(arbolDependence.fit,test)

########################################
# Basados en m?tricas de incertidumbre #
########################################

install.packages("rpart")
library(rpart)

Gini_tree.fit <- rpart(default~Avgexp+Age+Income+Inc_per+Active+Cur_add+Ownrent+Selfempl+Depndt+Major,
                       data=train_table, method="class",
                       parms = list(split = "gini"), control= rpart.control(minbucket = 30, maxdepth = 3, cp=0.05))
Gini_tree.fit

# El ?rbol obtenido no tiene hojas

# plot(arbolGini.fit) Dar? ERROR porque solo tiene el nodo ra?z. 
# No hay ?rbol porque ha sido totalmente podado
# La poda se ha realizado porque no se elimina de un nodo padre
# al nodo hijo un m?nimo de incertidumbre dada por el par?metro cp
# Relajamos el valor de cp a 0.002 (de 0.01 valor por defecto)

Gini_tree.fit <- rpart(default~Avgexp+Age+Income+Inc_per+Active+Cur_add+Ownrent+Selfempl+Depndt+Major,
                       data=train_table, method="class",
                       parms = list(split = "gini"), control= rpart.control(minbucket = 30, maxdepth = 3, cp=0.002))
Gini_tree.fit

# Con cp=-1, saldr?a el ?rbol de mayor profundidad porque no impone restricción de reducci?n de incertidumbre (no poda)

Gini_tree.fit.without_prune <- rpart(default~Avgexp+Age+Income+Inc_per+Active+Cur_add+Ownrent+Selfempl+Depndt+Major,
                                     data=train_table, method="class",
                                     parms = list(split = "gini"), control= rpart.control(minbucket = 30, maxdepth = 3, cp=-1))
Gini_tree.fit.without_prune

install.packages("rattle")
library(rattle)

install.packages("rpart.plot")
library(rpart.plot)

install.packages("RColorBrewer")
install.packages("ggcorrplot")
library(RColorBrewer)
# Dibujamos el ?rbol

fancyRpartPlot(Gini_tree.fit.without_prune, caption=NULL, palettes=c("Purples"))

plotcp(Gini_tree.fit.without_prune)
printcp(Gini_tree.fit.without_prune)

##################################################
##################################################
# 5. Valoraci?n: ROC, Cobertura, Precisi?n, LIFT #
##################################################
##################################################
test_table$Depndt <- as.factor(test_table$Depndt)
print(test_table$Depndt)
predict_TestTable.tree <-predict(Gini_tree.fit.without_prune, type='prob', test_table)

# devuelve la probabilidad asociada a cada clase

# Adjuntamos a la predicci?n el dato real para contruir la curva ROC

predict_TestAuxTable.tree <- prediction(predict_TestTable.tree[, 2], test_table$default)

RocTest.tree <- performance(predict_TestAuxTable.tree, "tpr", "fpr")

plot(RocTest.tree, main="Curva ROC", colorize=TRUE)

# L?nea base
abline(a=0,b=1,col="black")

auc_ROC<-performance(predict_TestAuxTable.tree, "auc")
auc_ROC@y.values[[1]]

# AUC = 0.796999 > 0.653521 (La regresion es la mejor opcion a elegir)

# Otros gr?ficos

# Gr?fico de precisi?n

precision<-performance(predict_TestAuxTable.tree, "prec", "rpp")
plot(precision, main="Gr?fico de precisi?n", colorize=T)
prior=sum(train_table$default==1)/length(train_table$default)
print(prior)
# L?nea base
abline(a=prior,b=0,col="black")

# Gr?fico de cobertura

coverage<-performance(predict_TestAuxTable.tree, "rec", "rpp")
plot(coverage, main="Gr?fico de cobertura", colorize=T)
# L?nea base
abline(a=0,b=1,col="black")

# Gr?fico de lift

lift<-performance(predict_TestAuxTable.tree, "lift", "rpp")
plot(lift, main="Gr?fico lift", colorize=T)
# L?nea base
abline(a=1,b=0,col="black")

#########################################################
# 4.3 ?rbol de decisi?n con matriz de costes/beneficios #
#########################################################

# Inclusi?n de costes por error (matriz de costee beneficios)
# Loss matrix: 0,1 por columnas y -,+ por filas
# Si digo "-" y era 1 (pierdo una baja), mayor coste
# Si digo "+" y era 0 (hago una campa?a inncesaria), menor coste
# NOTA: Loss matrix must have zero on diagonals
# Cargar el paquete rpart
library(rpart)

loss_matrix <- matrix(c(0, 360, 10, 0), byrow = TRUE, nrow = 2)
arbolGini_Cost.fit <- rpart(default~Avgexp+Age+Income+Inc_per+Active+Cur_add+Ownrent+Selfempl+Depndt+Major,
                            data=train_table, method="class",
                            parms = list(prior = c(.025,.975),split = "Gini", loss=loss_matrix),
                            control= rpart.control(minbucket = 30, maxdepth = 3, cp=-1))
graphics.off()
fancyRpartPlot(arbolGini_Cost.fit,caption=NULL,palettes=c("Purples"))

# Asegurarse de que 'Depndt' sea un factor en test_table
test_table$Depndt <- as.factor(test_table$Depndt)

# Los cortes han cambiado

predict_TestTable.tree <-predict(arbolGini_Cost.fit, type='prob', test_table)

# devuelve la probabilidad asociada a cada clase

# Adjuntamos a la predicci?n el dato real para contruir la curva ROC

# Instalar el paquete ROCR si aún no lo has hecho
install.packages("ROCR")

# Cargar el paquete ROCR
library(ROCR)

predict_TestAuxTable.tree <- prediction(predict_TestTable.tree[, 2], test_table$default)

RocTest.tree <- performance(predict_TestAuxTable.tree, "tpr", "fpr")
plot(RocTest.tree, main="Curva ROC", colorize=TRUE)

# L?nea base
abline(a=0,b=1,col="black")

auc_ROC<-performance(predict_TestAuxTable.tree, "auc")
auc_ROC@y.values[[1]]


# Otros gr?ficos

# Gr?fico de precisi?n

precision<-performance(predict_TestAuxTable.tree, "prec", "rpp")
plot(precision, main="Gr?fico de precisi?n", colorize=T)
prior=sum(train_table$default==1)/length(train_table$default)
print(prior)
# L?nea base
abline(a=prior,b=0,col="black")

# Gr?fico de cobertura

coverage<-performance(predict_TestAuxTable.tree, "rec", "rpp")
plot(coverage, main="Gr?fico de cobertura", colorize=T)
# L?nea base
abline(a=0,b=1,col="black")

# Gr?fico de lift

lift<-performance(predict_TestAuxTable.tree, "lift", "rpp")
plot(lift, main="Gr?fico lift", colorize=T)
# L?nea base
abline(a=1,b=0,col="black")

#####################
# 4.5 Random Forest #
#####################

install.packages('randomForest')
library(randomForest)

# Nota: El Random Forest, no funciona con missings.
set.seed(12345)
model.randomForest <- randomForest(formula=default~Avgexp+Age+Income+Inc_per+Active+Cur_add+Ownrent+Selfempl+Depndt+Major,
                                   data=train_table,
                                   ntree=100,
                                   nodesize=30, # observaciones por hoja
                                    maxnodes=8, # 2^3 = 8 (profundidad = 3)
                                   # Para valorar la importancia de cada uno de los 500 ?rboles #
                                    importance=TRUE,
                                   keep.forest=TRUE)

graphics.off()
varImpPlot(model.randomForest)

predict_TestTable.randomForest <- predict(model.randomForest, type='prob', test_table)

predict_AuxTable.randomForest <- prediction(predict_TestTable.randomForest[, 2], test_table$default)

auc_ROC<-performance(predict_AuxTable.randomForest, "auc")
auc_ROC@y.values[[1]]

# 0.796999 > 0.625733

#########################
# 4.6 Gradient Boosting #
#########################

# En este caso, vamos a contemplar una tabla de validaci?n
install.packages("lubridate")
install.packages("caret")
library(caret)
library(lattice)
trainProportion <- 0.7
trainIndexes <- createDataPartition(1:nrow(train_table), 1, trainProportion, list = FALSE)

training <- train_table[trainIndexes,]
validation <- train_table[-trainIndexes,]

# Para utilizar la funci?n del gradient boosting, es
# preciso hacer un One-Hot-Encoding de las variables
# de clase (factores), dado que la funci?n de R no las trata
# La funci?n para hacer el One-Hot-Encoding solo funciona
# sobre objetos de tipo data.table

install.packages('data.table')
library(data.table)

training.dt<-data.table(training)

install.packages('mltools')
library(mltools)

one_hot_encoding.training<-one_hot(training.dt[,c("Avgexp","Age","Income","Inc_per","Active","Cur_add","Ownrent","Selfempl","Exp_Inc","Depndt","Major")],dropCols=TRUE)

validation.dt<-data.table(validation)
one_hot_encoding.validation<-one_hot(validation.dt[,c("Avgexp","Age","Income","Inc_per","Active","Cur_add","Ownrent","Selfempl","Exp_Inc","Depndt","Major")],dropCols=TRUE)

test.dt<-data.table(test_table)
one_hot_encoding.test<-one_hot(test.dt[,c("Avgexp","Age","Income","Inc_per","Active","Cur_add","Ownrent","Selfempl","Exp_Inc","Depndt","Major")],dropCols=TRUE)

# Adem?s, la funci?n xgb funciona sobre objetos de tipo xgb.DMatrix

install.packages('xgboost')
library(xgboost)

one_hot_encoding.training.DMatrix<-xgb.DMatrix(
  data.matrix(one_hot_encoding.training),
  label=training$default
  )

one_hot_encoding.validation.DMatrix<-xgb.DMatrix(
  data.matrix(one_hot_encoding.validation),
  label=validation$default
)

one_hot_encoding.test.DMatrix<-xgb.DMatrix(
  data.matrix(one_hot_encoding.test),
  label=test_table$default
)

# es = early stopping

set.seed(12345)

watchlist <- list(train=one_hot_encoding.training.DMatrix,
                  valid=one_hot_encoding.validation.DMatrix)

hyperparameters<-list(subsample = 0.75, # Porcentaje de observaciones de cada muestra
                      max_depth = 10, # Profundidad de los ?rboles
                      eta = 0.05) # Tasa de aprendizaje)

xgb.model <- xgb.train(one_hot_encoding.training.DMatrix,
                       params=hyperparameters,
                       nrounds = 100,
                       watchlist=watchlist,
                       early_stopping_rounds = 10)

predictTest.xgb <- as.data.frame(predict(xgb.model,one_hot_encoding.test.DMatrix))

predictTestAux.xgb <- prediction(predictTest.xgb[,1], test_table$default)

auc_ROC<-performance(predictTestAux.xgb, "auc")
auc_ROC@y.values[[1]]

# 0.796999 > 0.6481545 (Regresion continua arrojando mejores resultados)
# Aun probando con varios valores con cada uno de los hiperparametros uno de los
# mejores resultados fue de  0.6481545

