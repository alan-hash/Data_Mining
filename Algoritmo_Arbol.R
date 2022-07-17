#install.packages('titanic')
#install.packages('tidyverse')

#cargarmos la libreria para el analisisis de datos
library(tidyverse)

#cargamos la libreria de datos
library(titanic)
data("titanic_train")
head(titanic_train)

#cargamos librerias para la clasificacion
#install.packages('rpart')
library(rpart)
#install.packages('rattle')
library(rattle)
#install.packages('rpart.plot')
library(rpart.plot)


#modelado de los arboles de decision
arbol <-rpart(
  formula = Survived ~ Sex + Age,
  data = titanic_train,
  method = 'class'
)

#Graficamos el arbol
fancyRpartPlot(arbol)


#prediciendo con el arbol
pred_arbol <- predict(arbol, type= 'class')
titanic_pred <- cbind(titanic_train, pred_arbol)




#pasajero masculino de 4 años de edad
predict(object = arbol,
        newdata = data.frame(Age= 4,
                             Sex='male'),
        type='class')



library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Arboles regresion con Shiny"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)





 

