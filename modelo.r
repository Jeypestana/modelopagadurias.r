library(car)
library(dplyr)
library(readxl)
library(caret)

# importamos la base para el modelo
data <- read_excel("Policiauniformado_modelo.xlsx")
head(data)

# eliminamos las varibales sin relevancia para el modelo
colnames(data)
mode1 <- data[, c(-1,-16,-17,-19)]

# revisamos datos faltantes
colSums(is.na(mode1))

# imputacion de datos en dado caso por valor mediano
mode1$ingresos_reportados[is.na(mode1$ingresos_reportados)] <- 
  median(mode1$ingresos_reportados,na.rm = T) 

#revisamos los clientes que han entrado y los que no en mora
table(mode1$mora) # Good = No / Bad = si
# valores en proporcion
prop.table(table(mode1$mora)) *100

#realizamos un Cross-Validation "variacion de parametros
set.seed(1234)
control <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                        summaryFunction = twoClassSummary )

#estimacion del modelo con los datos originales
fit <- train(mora ~ . , data = mode1 , method = "glm", family = binomial(),
             metric = "ROC" , trControl = control  )
fit
summary(fit)

# separamos la base en 70% entrenamiento y 30% prueba
set.seed(1234) # select a random sample with
train <- createDataPartition(mode1$mora, p= 0.7 , list = FALSE)


mode1.test <- mode1[-train,]
mode1.train <- mode1[train,]

# Revisamos las proporciones entre el inicial el test y el train
prop.table(table(mode1$mora)) *100 # proporcion base inicial
prop.table(table(mode1.train$mora)) *100 # proporcion entreno
prop.table(table(mode1.test$mora)) *100 # proporcion test

#renombramos los malos como "1" y buenos como "0"
mode1.train$mora <- ifelse(mode1.train$mora== "Si" ,1,0)

#recodificamos la variable dependiente en factor de dos niveles con etiquetas
#para el sub-muistreo
#mode1.train$mora <- recode(mode1.train$mora, "1=1 ;0=0")
table(mode1.train$mora) # al dia = 0 / mora = 1
mode1.train$mora <- as.factor(mode1.train$mora) # convertimos como factor
mode1.train$mora = factor(mode1.train$mora, 
                          levels = levels(mode1.train$mora),
                          labels = c("No","Si"),
                          ordered = F) # agregamos etiquetas
str(mode1.train$mora) # revisamos los cambios factos dos niveles con labels


# teniendo en cuenta la desproporcion de los datos realizamos muestreos
# Down sub-muestreo
set.seed(1234)
dow_train <- downSample(x = mode1.train[,-16], y = mode1.train$mora)
table(dow_train$Class) # proporcion test igual

# Up sobre-muestreo
set.seed(1234)
up_train <- upSample(x = mode1.train[,-16], y = mode1.train$mora)
table(up_train$Class) # proporcion test igual)

#revisamos varibales que apliquen cambio a factor para el muestreo de ROSE
str(mode1.train)

mode1.train <- mode1.train %>% mutate(genero = factor(genero), actividad_cliente = factor(actividad_cliente),
                estado_civil = factor(estado_civil), tipo_vivienda = factor(tipo_vivienda),
                regional = factor(regional) , linea_credito = factor(linea_credito))


# muestreo ROSE
library(ROSE)
set.seed(1234)
rose_train <- ROSE(mora ~ ., data = mode1.train)$data
table(rose_train$mora)

# muestreo SMOTE
#revisamos las varibales que apliquen cambio a numeric
str(mode1.train)

mode1.train <- mode1.train %>% mutate(genero = as.numeric(genero), actividad_cliente = as.numeric(actividad_cliente),
                                      estado_civil = as.numeric(estado_civil), tipo_vivienda = as.numeric(tipo_vivienda),
                                      regional = as.numeric(regional) , linea_credito = as.numeric(linea_credito))


# Estimacion de metricas de modelos

#DOWN
#generamos vectores para especifidad , sensitividad y ROC
sen_d <- rep(0,10)
esp_d <- rep(0,10)
roc_d <- rep(0,10)
#valores de control del modelos "library caret
control <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                        summaryFunction = twoClassSummary)
# bucle 10 para almacenar los resultados 
for (i in 1:10) {
  fit_dow <- train(Class ~ ., data = dow_train, method = "glm", metric = "ROC", trControl = control)
  ypred <- predict(fit_dow,mode1.test)
  ypred=as.factor(ypred)
  resul=confusionMatrix(ypred,mode1.test$mora,positive= "Si")
  roc_d[i]=as.numeric(fit_dow$results[2])
  sen_d[i]=as.numeric(fit_dow$results[3])
  esp_d[i]=as.numeric(fit_dow$results[4])
}

# clasificacion de los que tienen mora "exito"
sen_d
esp_d
roc_d

#UP
#generamos vectores para especifidad , sensitividad y ROC
sen_u <- rep(0,10)
esp_u <- rep(0,10)
roc_u <- rep(0,10)
#valores de control del modelos "library caret
control <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                        summaryFunction = twoClassSummary)
# bucle 10 para almacenar los resultados 
for (i in 1:10) {
  fit_up <- train(Class ~ ., data = up_train, method = "glm", metric = "ROC", trControl = control)
  ypred <- predict(fit_up,mode1.test)
  ypred=as.factor(ypred)
  resul=confusionMatrix(ypred,mode1.test$mora,positive= "Si")
  roc_u[i]=as.numeric(fit_up$results[2])
  sen_u[i]=as.numeric(fit_up$results[3])
  esp_u[i]=as.numeric(fit_up$results[4])
}

# clasificacion de los que tienen mora "exito"
sen_u
esp_u
roc_u



#ROSE
#generamos vectores para especifidad , sensitividad y ROC
sen_r <- rep(0,10)
esp_r <- rep(0,10)
roc_r <- rep(0,10)
#valores de control del modelos "library caret
control <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                        summaryFunction = twoClassSummary)
# bucle 10 para almacenar los resultados 
for (i in 1:10) {
  fit_rose <- train(mora ~ ., data = rose_train, method = "glm", metric = "ROC", trControl = control)
  #ypred <- predict(fit_rose,mode1.test)
  #ypred=as.factor(ypred)
  #resul=confusionMatrix(ypred,mode1.test$mora,positive= "Si")
  roc_r[i]=as.numeric(fit_rose$results[2])
  sen_r[i]=as.numeric(fit_rose$results[3])
  esp_r[i]=as.numeric(fit_rose$results[4])
}

# clasificacion de los que tienen mora "exito"
sen_r
esp_r
roc_r

#resultados 3 modelos de sub-muestreo

#calsificar bien a los clientes en mora sensitividad
boxplot(sen_d,sen_r,sen_u)
#Especificidad modelo 2 clasifica mejor a los que si tiene mora
boxplot(esp_d,esp_r,esp_u)
#vr curva roc metodo 2 mayor presencia
boxplot(roc_d,roc_r,roc_u)
