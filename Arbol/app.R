#Importamos librería
library (shiny)

#Iniciamos el proceso principal
ui<- fluidPage(
  titlePanel(title ="Análisis de los datos"), #Titulo de la página
  sidebarLayout( #Apartamos espacio en un costado de la ventana
    sidebarPanel( #Creamos el espacio a un costado de la ventana
      radioButtons("dist", "Diagramas", #Asignamos ID y nombre del panel
                         c("Caja de cantidad" = "uno", #Nombre e ID de las opciones del radiobutton
                           "Caja del total" = "dos", 
                           "Caja de cantidad~comida" = "tres",
                           "Árbol propuesto" = "cuatro",
                           "Comida~valor continuo" = "seis",
                           "Ocultar diagramas"= "siete")),
      
      selectInput(inputId = "dataset", #creamos un nuevo objeto y asignamos ID
                  label = "Visualizar tablas",
                  choices = c("NONE",
                              "Alimentos + consumidos",
                              "Alimentos - consumidos",
                              "Alimentos prom. Consumidos",
                              "Hamburguesa sencilla",
                              "Hamburguesa Hawaiana",
                              "Hamburguesa Pollo",
                              "Hamburguesa doble carne",
                              "Chalupas",
                              "Burrito de longaniza",
                              "Burrito de coriqueso",
                              "Papas fritas",
                              "Pambazos",
                              "HotDog",
                              "Pastel 3 leches",
                              "Pay de limón",
                              "Helado de limon",
                              "Helado de fresa",
                              "Coca-Cola 600ml",
                              "Boing Mango 600ml",
                              "Boing Guayaba 500ml",
                              "Agua de jamaica 1L",
                              "Agua de limón 1L",
                              "Agua de horchata")),
                              
      
      numericInput(inputId = "obs", #Objeto que controla el número de filas a visualizar
                   label = "Número de filas para ver: ",
                   value = 10),
      
      verbatimTextOutput("summary"), #Imprime un summary
    ),
    mainPanel( #Ventana principal
      plotOutput(outputId = "distPlot"), #Objetos a imprimir por ID en el server
      tableOutput("view") #Imprime la tabla
    )
  )
)

server<-function(input,output){ #Backend del shiny
    output$distPlot<-renderPlot({ #Indica que se imprimirán renderPlot con el ID distplot
      dist<-switch(input$dist, #indicamos que al seleccionar una opción la ventana debe cambiar
                   #Acciones de cada opción por ID dado en las opciones del radioButton
                 uno = boxplot(df$Cantidad,col=c("green")),
                 dos = boxplot(df$Total,col=c("yellow")),
                 tres = boxplot(IDComida~Cantidad ,data = df,col=c("orange")),
                 cuatro = fancyRpartPlot(model=model),
                 seis = boxplot(rdata$Valor ~ rdata$Comida, col = "yellow", main = "Comida respecto a su valor continuo", xlab = "Comida", ylab = "Valor", Vertical = TRUE),
                 siete = dev.off())
    })
    datasetInput <- reactive({ #Control del selectInput
      switch(input$dataset,
             "Alimentos + consumidos" = altoConsumo,
             "Alimentos - consumidos" = bajoConsumo,
             "Alimentos prom. consumidos" = normalConsumo,
             "Hamburguesa sencilla" = rdata[rdata$IDComida==1,],
             "Hamburguesa Hawaiana" = rdata[rdata$IDComida == 2,],
             "Hamburguesa Pollo" = rdata[rdata$IDComida == 3,],
             "Hamburguesa doble carne" = rdata[rdata$IDComida == 4,],
             "Chalupas" = rdata[rdata$IDComida == 5,],
             "Burrito de longaniza" = rdata[rdata$IDComida == 6,],
             "Burrito de coriqueso" = rdata[rdata$IDComida == 7,],
             "Papas fritas" = rdata[rdata$IDComida == 8,],
             "Pambazos" = rdata[rdata$IDComida == 9,],
             "HotDog" = rdata[rdata$IDComida == 10,],
             "Pastel 3 leches" = rdata[rdata$IDComida == 11,],
             "Pay de limón" = rdata[rdata$IDComida == 12,],
             "Helado de limon" = rdata[rdata$IDComida == 13,],
             "Helado de fresa" = rdata[rdata$IDComida == 14,],
             "Coca-Cola 600ml" = rdata[rdata$IDComida == 15,],
             "Boing Mango 600ml" = rdata[rdata$IDComida == 16,],
             "Boing Guayaba 500ml" = rdata[rdata$IDComida == 17,],
             "Agua de jamaica 1L" = rdata[rdata$IDComida == 18,],
             "Agua de limón 1L" = rdata[rdata$IDComida == 19,],
             "Agua de horchata" = rdata[rdata$IDComida == 20,])
    })
    
    output$summary <-renderPrint({
      dataset <- datasetInput()
      summary(dataset)
    })
    
    output$view <- renderTable({
      head(datasetInput(), n = input$obs)
    })
}

#Comando para ejecutar la aplicación
shinyApp(ui=ui,server=server)