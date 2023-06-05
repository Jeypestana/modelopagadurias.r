library(readxl)

#importamos la base para el modelo
mode1 <- read_excel("Policiauniformado_modelo.xlsx")
View(mode1)

#revisamos los clientes que han entrado y los que no en mora
table(mode1$mora) # Good = No / Bad = si

#renombramos los malos como "0" y buenos como "1"
mode1$mora <- ifelse(mode1$mora=='Si',0,1)
table(mode1$mora) # Good = 1 / Bad = 0

#convertimos variables de tipo categoricas a factor
factor(mode1$genero)
factor(mode1$actividad_cliente)
factor(mode1$estado_civil)
factor(mode1$tipo_vivienda)\
factor(mode1$regional)
factor(mode1$oficina)
factor(mode1$departamento_cliente)
factor(mode1$linea_credito)


# separamos la base en 70% entrenamiento y 30% prueba
n<- dim(mode1)[1]

set.seed(1234) # select a random sample with
train <- sample(1:n , 0.7*n)

mode1.test <- mode1[-train,]
mode1.train <- mode1[train,]

ytrain <- mode1$mora[train]
ytest <- mode1$mora[-train]

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



