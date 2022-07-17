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
        newdata = data.frame(Sex='male', Age= 4),
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
      selectInput(inputId = "Sex",
                  label = "Genero del Pasajero",
                  choices = c("male", "female")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "Age",
                   label = "Edad",
                   value = 1),
      
      actionButton("predict", "Prediccion")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      
    )
  )
)



server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "male" = male,
           "female" = female)
  })
  
  observeEvent(input$predict, {
    nuevo <- predict(object = arbol,newdata = data.frame(datasetInput(),Age=input$age),
            type='class')
      output$summary <- renderPrint({
      predict(nuevo)
    })
    
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
 

