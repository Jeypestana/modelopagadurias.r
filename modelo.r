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


#renombramos los malos como "0" y buenos como "1"
#mode1$mora <- ifelse(mode1$mora=='Si',0,1)
#table(mode1$mora) # Good = 1 / Bad = 0
#prop.table(table(mode1$mora)) *100


#convertimos variables de tipo categoricas a factor
#factor(mode1$genero)
#factor(mode1$actividad_cliente)
#factor(mode1$estado_civil)
#factor(mode1$tipo_vivienda)
#factor(mode1$regional)
#factor(mode1$oficina)
#factor(mode1$departamento_cliente)
#factor(mode1$linea_credito)


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

#ytrain <- mode1$mora[train]
#ytest <- mode1$mora[-train]


# Revisamos las proporciones entre el inicial el test y el train
prop.table(table(mode1$mora)) *100 # proporcion base inicial
prop.table(table(mode1.train$mora)) *100 # proporcion entreno
prop.table(table(mode1.test$mora)) *100 # proporcion test

#renombramos los malos como "1" y buenos como "0"
mode1.train$mora <- ifelse(mode1.train$mora== "Si" ,1,0)

#recodificamos la variable dependiente en factor de dos niveles con etiquetas
#para el sub-muistreo
mode1.train$mora <- recode(mode1.train$mora, "1=1 ;0=0")
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

#revisamos varibales que apliquen cambio a factor
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

str(mode1.train)

mode1.train <- mode1.train %>% mutate(genero = as.numeric(genero), actividad_cliente = as.numeric(actividad_cliente),
                                      estado_civil = as.numeric(estado_civil), tipo_vivienda = as.numeric(tipo_vivienda),
                                      regional = as.numeric(regional) , linea_credito = as.numeric(linea_credito))


library(smotefamily)
set.seed(1234)
smote_train <- SMOTE(mode1.train[,-16],as.numeric(mode1.train$mora))
smote_train <- smote_train$data
table(smote_train$class)







#convertimos variables de tipo categoricas a factor
#factor(mode1$genero)
#factor(mode1$actividad_cliente)
#factor(mode1$estado_civil)
#factor(mode1$tipo_vivienda)\
#factor(mode1$regional)
#factor(mode1$oficina)
#factor(mode1$departamento_cliente)
#factor(mode1$linea_credito)
#factor(mode1$mora)



# generamos modelo con las variables sin contar numero radicacion
# ya que esta es el identificador para cada credito
m1 <- glm(mora ~ . -numero_radicacion -oficina, family = binomial , mode1 )
summary(m1)

#varibales significaticas al 10%
sig.var<- summary(m1)$coeff[-1,4] <0.01
names(sig.var)[sig.var == TRUE]


# revisamos el entrenamiento del modelo con el 30% separado
pred1<- predict.glm(m1,newdata = mode1.test, type="response")
# revisamos la matriz de confusion
result1<- table(ytest, floor(pred1+0.45))
result1

# debido a la falta de prediccion , evaluamos seleccion de variables

mode2 <- glm(mora ~ . -numero_radicacion -oficina, family = binomial , mode1 )
AIC(mode2)


# Eliminar la variable con el valor p más alto (menos significativa) en el modelo y repetir esto hasta que todas sean significativas
modelo_res <- step(mode2, direction = "backward")

# Construir un nuevo modelo sin las variables eliminadas y verificar sus metricas de ajuste
summary(modelo_res)

# revisamos las predicciones con este nuevo modelo despues de la seleccion de variables
pred1<- predict.glm(modelo_res,newdata = mode1.test, type="response")
# revisamos la matriz de confusion
result1<- table(ytest, floor(pred1+0.3))
result1



