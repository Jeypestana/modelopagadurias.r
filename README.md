# modelopagadurias.r
Ejemplo de modelo logit para probabilidad de mora de un credito con datos 
desbalanceados se ajusta 3 metodos para sub-muestreo y se comparan especificidad , sensitividad y roc

Esta es una base de cartera libranza de creditos de la policia por libranza el cual con una 
serie de variables se quiere pronosticar posibles default

Como se puede evidenciar el modelo logit directo nos da unas metricas al modelo muy bajas
no ajustables para tomar el modelo a produccion 

> sen_r
 [1] 0.5363636 0.5303030 0.5436364 0.5412121 0.5412121 0.5418182
 [7] 0.5393939 0.5339394 0.5490909 0.5412121
> esp_r
 [1] 0.5943558 0.5901317 0.5907576 0.6033883 0.5961449 0.5919552
 [7] 0.5853396 0.5901353 0.6033829 0.6003491
> roc_r
 [1] 0.5830157 0.5820365 0.5863664 0.5855619 0.5814168 0.5842362
 [7] 0.5813298 0.5848694 0.5901341 0.5884690
 
 Para las 3 metricas de sensibilidad especificidad y curva roc
 haciendo test no parametricos para la comparacion de medias de Mann-Whitney para los 3 metodos haciendo una clasificacion para los datos desbalanceados dan promedio de 55% en las metricas del modelo