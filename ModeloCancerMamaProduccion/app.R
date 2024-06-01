library(shiny)
library(nnet)
library(shinythemes)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .prediccion-positivo {
        font-size: 24px;
        color: rgba(54, 162, 235, 0.6);
        font-weight: bold;
      }
      .prediccion-negativo {
        font-size: 24px;
        color: rgba(255, 99, 132, 0.6);
        font-weight: bold;
      }
      table th {
        font-weight: bold;
      }
      .title-panel {
        background-color: #f5f5f5;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
      }
      .subtitle {
        font-size: 16px;
        color: #333;
      }
    "))
  ),
  wellPanel(
    class = "title-panel",
    h1("Modelo de Red Neuronal Cáncer de Mama"),
    div(class = "subtitle", "Mario Pascual González; Bioinformática 23-24")
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("edad", "Edad:", value = 30, min = 0, max = 100),
      textOutput("edadFeedback"),
      selectInput("estadio", "Estadio:", choices = c("T0-T1", "T2", "T3", "T4")),
      selectInput("fenotipo", "Fenotipo:", choices = c("Basal", "Her2", "LumA", "LumB", "Normal")),
      selectInput("grado", "Grado:", choices = c("I", "II", "III")),
      selectInput("rest", "REst:", choices = c("N", "P")),
      sliderInput("threshold", "Threshold:", min = 0.01, max = 0.99, value = 0.5, step = 0.01),
      textOutput("selectedThreshold"),
      actionButton("predict", "Predecir")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predicción",
                 fluidRow(
                   column(8, plotlyOutput("predictionPlot")),
                   column(4,
                          h3("Resumen de la consulta y predicción"),
                          tableOutput("inputValuesTable"))
                 )
        ),
        tabPanel("Información",
                 h3("Información del Proyecto"),
                 p("Esta aplicación fue relizada para la asignatura \"Minería de Datos\" para el grado en Ingeniería de la Salud, con mención en Bioinformática en el curso 23-24 bajo la supervisión del profesor José Manuel Jérez Aragonés. Mario Pascual González fue el alumno que se encargó se las tareas de exploración de datos, selección de modelo, entrenamiento, y construcción de la interfaz gráfica de esta aplicación con motivo de la tercera entrega de la asignatura. El código está abierto bajo la licencia del MIT en GitHub en el siguiente enlace:"),
                 a("https://github.com/MarioPasc/Algoritmos-de-Aprendizaje-Computacional-R", href = "https://github.com/MarioPasc/Algoritmos-de-Aprendizaje-Computacional-R")
        )
      )
    )
  )
)




categorizar_edad <- function(edad) {
  if (edad >= 24 && edad < 29.1) {
    return("Grupo1")
  } else if (edad >= 29.1 && edad < 34.2) {
    return("Grupo2")
  } else if (edad >= 34.2 && edad < 39.3) {
    return("Grupo3")
  } else if (edad >= 39.3 && edad < 44.4) {
    return("Grupo4")
  } else if (edad >= 44.4 && edad < 49.5) {
    return("Grupo5")
  } else if (edad >= 49.5 && edad < 54.6) {
    return("Grupo6")
  } else if (edad >= 54.6 && edad < 59.7) {
    return("Grupo7")
  } else if (edad >= 59.7 && edad < 64.8) {
    return("Grupo8")
  } else if (edad >= 64.8 && edad < 69.9) {
    return("Grupo9")
  } else if (edad >= 69.9 && edad < 75) {
    return("Grupo10")
  } else {
    return("Fuera de rango")
  }
}

# Cargar los datos
data <- read.csv("data_factor.csv")
# Recodificar la variable PCR para que los valores sean 0 y 1
data$PCR <- data$PCR - 1


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Convertir las columnas de data a factores
  data <- read.csv("./data_factor.csv")
  data$PCR <- data$PCR - 1
  data$Edad <- factor(data$Edad)
  data$Estadio <- factor(data$Estadio)
  data$Fenotipo <- factor(data$Fenotipo)
  data$Grado <- factor(data$Grado)
  data$REst <- factor(data$REst)
  
  # Feedback para la edad
  observeEvent(input$edad, {
    if (input$edad < 29.09 || input$edad > 76) {
      output$edadFeedback <- renderText({
        "Por favor, introduce una edad entre 29 y 75 años."
      })
    } else {
      output$edadFeedback <- renderText({
        ""
      })
    }
  })
  
  output$selectedThreshold <- renderText({
    paste("Threshold seleccionado:", input$threshold)
  })
  
  observeEvent(input$predict, {
    if (input$edad < 29.09 || input$edad > 76) {
      output$edadFeedback <- renderText({
        "Por favor, introduce una edad entre 29 y 75 años."
      })
      return(NULL)
    } else {
      output$edadFeedback <- renderText({
        ""
      })
    }
    
    grupo_edad <- categorizar_edad(input$edad)
    
    # Guardar las entradas del usuario en un vector asegurando que cada factor tenga al menos dos niveles
    new_data <- data.frame(
      Edad = factor(grupo_edad, levels = levels(data$Edad)),
      Estadio = factor(input$estadio, levels = levels(data$Estadio)),
      Fenotipo = factor(input$fenotipo, levels = levels(data$Fenotipo)),
      Grado = factor(input$grado, levels = levels(data$Grado)),
      REst = factor(input$rest, levels = levels(data$REst)),
      stringsAsFactors = TRUE
    )

    
    # Convertir los datos a one-hot encoding, incluyendo PCR en data_matrix
    data_matrix <- model.matrix(PCR ~ Edad + Estadio + Fenotipo + Grado + REst, data = data)
    data_matrix <- as.data.frame(data_matrix)
    data_matrix$PCR <- data$PCR
    data_matrix$`(Intercept)` <- NULL
    
    
    # Crear las mismas columnas en new_data_matrix que en data_matrix
    new_data_matrix <- model.matrix(~ Edad + Estadio + Fenotipo + Grado + REst, data = new_data)
    new_data_matrix <- as.data.frame(new_data_matrix)
    
    # Asegurarse de que new_data_matrix tenga las mismas columnas que data_matrix
    missing_cols <- setdiff(names(data_matrix), names(new_data_matrix))
    for (col in missing_cols) {
      new_data_matrix[[col]] <- 0
    }
    
    # Reordenar las columnas para que coincidan
    new_data_matrix <- new_data_matrix[names(data_matrix)]
    
    # Entrenar el modelo con todos los datos
    model <- nnet(PCR ~ ., data = data_matrix, size = 5, decay = 0.4)
    
    # Hacer la predicción raw
    prediccion_raw <- predict(model, newdata = new_data_matrix, type = "raw")
    
    # Aplicar el threshold para determinar la predicción final
    prediccion_final <- ifelse(prediccion_raw > input$threshold, "PCR-Positivo", "PCR-Negativo")
    
    # Crear el gráfico de barras
    output$predictionPlot <- renderPlotly({
      plot_ly(
        x = c("PCR0", "PCR1"),
        y = c((1 - prediccion_raw) * 100, prediccion_raw * 100),
        type = 'bar',
        name = 'Predicción',
        marker = list(color = c('rgba(255, 99, 132, 0.6)', 'rgba(54, 162, 235, 0.6)'))
      ) %>% layout(
        title = "Porcentaje de Predicción",
        xaxis = list(title = ""),
        yaxis = list(title = "Porcentaje (%)")
      )
    })
    
    # Crear la tabla de resumen incluyendo la predicción
    output$inputValuesTable <- renderTable({
      data.frame(
        Variable = c("Edad", "Categoría de Edad", "Estadio", "Fenotipo", "Grado", "REst", "Predicción"),
        Valor = c(input$edad, grupo_edad, input$estadio, input$fenotipo, input$grado, input$rest, prediccion_final)
      )
    })
  })
}

shinyApp(ui = ui, server = server)
