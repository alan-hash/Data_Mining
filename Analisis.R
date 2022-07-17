  #install.packages("psych") 
  #install.packages("corrplot") 

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
  
  
  
  
  #permite crear una amplia variedad de correlogramas con una sola función
  # Posdata no se puedo aplicar este metodo investigar por que 
  library(corrplot)
  M=cor(df)
  corrplot(M,method = 'number')
  corrplot(M,method = 'pie')
  corrplot(M,method = 'shade')
  

  
  
  
  
  