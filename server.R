#dependencias
library(shiny)
library(shinydashboard)
library(tidyverse)

historico_dolar <- readRDS("historico_dolar.RDS")
grafico_casos <- readRDS("grafico_casos.RDS")
grafico_hospitalizados <- readRDS("grafico_hospitalizados.RDS")
grafico_provincia <- readRDS("grafico_provincia.RDS")
datos_economicos <- data.frame(x = seq(50),
                               y = rnorm(50, 10, 3),
                               z = rnorm(50, 11, 2),
                               w = rnorm(50, 9, 2))
datos_covid <- data.frame(x = seq(50),
                          y = rnorm(50, 10, 3),
                          z = rnorm(50, 11, 2),
                          w = rnorm(50, 9, 2))

#server
shinyServer(function(input, output, session){
  # Tab Resumen: columna Variables economicas --------------------------------------------------------------------------
  
  # Value boxes Indicadores Venta dolar --------------------------------------------------------------------------
  
  output$precio_venta <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Precio en colones", 
             icon = icon("dollar-sign"),
             color = "teal")
  })
  
  output$cambio_venta <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Value boxes Indicadores Compra dolar --------------------------------------------------------------------------
  
  output$precio_compra <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Precio en colones", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  output$cambio_compra <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Value boxes Indicadores Inflacion --------------------------------------------------------------------------
  
  output$valor_inflacion <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Inflacion % anual", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  output$cambio_inflacion <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Value boxes Indicadores Importaciones --------------------------------------------------------------------------
  
  output$valor_importaciones <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Importaciones en millones de $", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  output$cambio_importaciones <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Value boxes Indicadores Exportaciones --------------------------------------------------------------------------
  
  output$valor_exportaciones <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Exportaciones en millones de $", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  output$cambio_exportaciones <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Tab Resumen: columna datos COVID --------------------------------------------------------------------------
  
  # Value boxes Casos nuevos --------------------------------------------------------------------------
  
  output$casos_nuevos <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Casos nuevos", 
             icon = icon("arrows-alt-v"),
             color = "maroon")
  })
  
  output$cambio_nuevos <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Tasa de cambio % casos nuevos", 
             icon = icon("arrows-alt-v"),
             color = "maroon")
  })
  
  # Value box Hospitalizados --------------------------------------------------------------------------
  
  output$valor_hospitalizados <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Pacientes hospitalizados", 
             icon = icon("arrows-alt-v"),
             color = "maroon")
  })
  
  # Value box UCI --------------------------------------------------------------------------
  
  output$valor_uci <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Pacientes en UCI", 
             icon = icon("arrows-alt-v"),
             color = "maroon")
  })
  
  # Value box Recuperados --------------------------------------------------------------------------
  
  output$valor_recuperados <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Personas recuperadas", 
             icon = icon("arrows-alt-v"),
             color = "maroon")
  })
  
  # Tab Resumen: graficos series de tiempo --------------------------------------------------------------------------
  
  # Grafico variable economica --------------------------------------------------------------------------
  
  output$graf_var <- renderEcharts4r({
    
    datos_economicos %>% 
      e_charts(x) %>% 
      e_line(z) %>% 
      e_area(w) %>% 
      e_theme("infographic") %>% 
      e_datazoom()
    
  })
  
  
  
  
  # App anterior --------------------------------------------------------------------------
  output$compra_dolar <- renderValueBox({
    valueBox(datos[nrow(datos),2],
             tags$p("Tipo de cambio compra",
                    style = "font-size: 100%"),
             color = "olive",
             icon = icon("clock")
    )
  })
  output$venta_dolar <- renderValueBox({
    valueBox(datos[nrow(datos),3],
             tags$p("Tipo de cambio venta",
                    style = "font-size: 100%"),
             color = "navy",
             icon = icon("clock")
    )
  })
  output$historico_dolar <- renderEcharts4r({
    historico_dolar
  })
##graficos del segundo tab
  output$grafico_casos <- renderPlot({
    grafico_casos
  })
  output$grafico_hospitalizados <- renderPlot({
    grafico_hospitalizados
  })
  output$grafico_provincia <- renderPlot({
    grafico_provincia
  })
})