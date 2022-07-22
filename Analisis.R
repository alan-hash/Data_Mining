  #install.packages("psych") 
  #install.packages("rpart") 
  #install.packages("rattle") 




  #Importamos el dataset y lo alamacenamos en una variable
 df <- read.csv('C:\\Users\\109141\\Desktop\\dataset.csv')
 
 
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
  df%>%
    select(Valor,Cantidad,Comida)->dfsel
  
  #Entrenamiento
  library(rpart)
  model<-rpart(formula = Valor~Cantidad+Comida,data = dfsel)


  
  ##PREDICCION
  pred<-predict(object = model,newdata = dfsel)
  
  dfsel$predicciones<-ifelse(pred >0.3,1,0)
  dfsel %>%
    select(Valor,predicciones,everything())
  
  #Predicciones vs Realidad
  sum(dfsel$predicciones==dfsel$Valor)
  mean(dfsel$predicciones==dfsel$Valor)
  
  
  
  #Graficamos el arbol
  library(rattle)
  fancyRpartPlot(model=model)
 
  
  


  