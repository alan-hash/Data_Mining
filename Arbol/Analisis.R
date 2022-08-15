  #install.packages("psych") 
  #install.packages("rpart") 
  #install.packages("rattle")
  #install.packages("dplyr")
  #install.packages("shiny")
  #install.packages("DT")

  #Importamos el dataset y lo alamacenamos en una variable
 df <- read.csv('C:\\Users\\wiris\\Desktop\\dataset.csv')
 
 
 #Función que devuelve las primeras n filas del conjunto(head)/ ultimas n filas del conjunto(tail)
  head(df, n=10)
  tail(df, n=10)
  
  #devuelve información relevante sobre de la estructura de datos y los datos
  str(df)
  summary(df)
  
  
  # boxplot función diseñada para crear diagramas de caja
  boxplot(df$Cantidad,col=c("green"))
  boxplot(df$Total,col=c("yellow"))
  
  boxplot(IDComida~Cantidad ,data = df,col=c("orange"))
  
  
  #PREPROCESAMIENTO
  colSums(is.na(df))
  
 #SELECCION DE VARIABLES
  library(magrittr)
  library(dplyr)
  df%>%
    select(Valor,Cantidad,IDComida)->dfsel
  df%>%
    select(DIA,Comida,IDComida,Cantidad,Total,Valor)->rdata
  
  #Entrenamiento
  library(rpart)
  model<-rpart(formula = Valor~Cantidad+IDComida,data = dfsel)


  
  ##PREDICCION
  pred<-predict(object = model,newdata = dfsel)
  
  dfsel$predicciones<-ifelse(pred >0.1,1,0)
  dfsel %>%
    select(Valor,predicciones,everything())
  
  #Predicciones vs Realidad
  sum(dfsel$predicciones==dfsel$Valor)
  mean(dfsel$predicciones==dfsel$Valor)
  
  
  
  #Graficamos el arbol
  library(rattle)
  fancyRpartPlot(model=model)
  
  #Guardar el nombre de los alimentos que están en alto, bajo y normal consumo
  
  altoConsumo <- rdata[rdata$Valor == 1,]
  bajoConsumo <- rdata[rdata$Valor == 0,]
  normalConsumo <- rdata[rdata$Valor == -1,]
  
  #Imprimir alto consumo
  print(altoConsumo)
  
  #Imprimir bajo consumo
  print(bajoConsumo)
  
  #Imprimir normal consumo
  print(normalConsumo)
  
  boxplot(rdata$Valor ~ rdata$Comida, col = "yellow", main = "Comida respecto a su valor continuo", xlab = "Comida", ylab = "Valor")
  
  
  library(tidyverse)
  analisis <- c (rdata)
  duplicated(analisis$Comida)
  !duplicated(analisis$Comida)
  comida <- analisis[!duplicated(analisis$Comida)]
  comida
  