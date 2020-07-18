#dependencias
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(echarts4r)

historico_dolar <- readRDS("historico_dolar.RDS")
#grafico_casos <- readRDS("grafico_casos.RDS")
#grafico_hospitalizados <- readRDS("grafico_hospitalizados.RDS")
#grafico_provincia <- readRDS("grafico_provincia.RDS")
grafico_prediccion_red <- readRDS("grafico_prediccion_red.RDS")
tabla_red_compra <- readRDS("tabla_red_compra.RDS")
tabla_red_venta <- readRDS("tabla_red_venta.RDS")
grafico_prediccion_arima <- readRDS("grafico_prediccion_arima.RDS")
tabla_arima_venta <- readRDS("tabla_arima_venta.RDS")
tabla_arima_compra <- readRDS("tabla_arima_compra.RDS")
grafico_prediccion_naive <- readRDS("grafico_prediccion_naive.RDS")
tabla_naive_compra <- readRDS("tabla_naive_compra.RDS")
tabla_naive_venta <- readRDS("tabla_naive_venta.RDS")

datos_economicos <- data.frame(x = seq(50),
                               y = rnorm(50, 10, 3),
                               z = rnorm(50, 11, 2),
                               w = rnorm(50, 9, 2),
                               u = rnorm(50, 9, 1),
                               v = rnorm(50, 12, 4))
graf_var_ventadolar <- datos_economicos %>% 
  e_charts(x) %>% 
  e_line(y) %>% 
  e_theme("infographic") %>% 
  e_datazoom() %>% 
  e_legend(right = "50")

graf_var_compradolar <- datos_economicos %>% 
  e_charts(x) %>% 
  e_line(z) %>% 
  e_theme("infographic") %>% 
  e_datazoom() %>% 
  e_legend(right = "50")

graf_var_inflacion <- datos_economicos %>% 
  e_charts(x) %>% 
  e_line(w) %>% 
  e_theme("infographic") %>% 
  e_datazoom() %>% 
  e_legend(right = "50")

graf_var_import <- datos_economicos %>% 
  e_charts(x) %>% 
  e_line(u) %>% 
  e_theme("infographic") %>% 
  e_datazoom() %>% 
  e_legend(right = "50")

graf_var_export <- datos_economicos %>% 
  e_charts(x) %>% 
  e_line(v) %>% 
  e_theme("infographic") %>% 
  e_datazoom() %>% 
  e_legend(right = "50")

graf_covid_diario <- readRDS("graf_covid_diario.RDS")

graf_covid_acum <- readRDS("graf_covid_acum.RDS")


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
  
  output$titulo_graf_var <- renderText({
    
    if (input$variables == "Tipo de cambio: Venta $") {
      "Tipo de cambio: Venta del $"
    } else if (input$variables == "Tipo de cambio: Compra $") {
      "Tipo de cambio: Compra $"
    } else if (input$variables == "Inflación") {
      "Inflación comercial"
    } else if (input$variables == "Importaciones") {
      "Importaciones (CIF)"
    } else if (input$variables == "Exportaciones") {
      "Exportaciones (FOB)"
    } 
    
  })
  
  output$graf_var <- renderEcharts4r({
    
    if (input$variables == "Tipo de cambio: Venta $") {
      graf_var_ventadolar
    } else if (input$variables == "Tipo de cambio: Compra $") {
      graf_var_compradolar
    } else if (input$variables == "Inflación"){
      graf_var_inflacion
    } else if (input$variables == "Importaciones"){
      graf_var_import
    } else if (input$variables == "Exportaciones"){
      graf_var_export
    }
    
  })
  
  # Grafico graficos covid --------------------------------------------------------------------------
  
  output$titulo_graf_covid <- renderText({
    if (input$opciones_graf_covid == "Datos acumulados") {
      "Acumulado de infectados, recuperados y fallecidos"
    } else if (input$opciones_graf_covid == "Datos diarios") {
      "Nuevos infectados, recuperados y fallecidos por día"
    } 
  })
  
  output$graf_covid <- renderEcharts4r({
    
    if (input$opciones_graf_covid == "Datos acumulados") {
      graf_covid_acum
    } else if (input$opciones_graf_covid == "Datos diarios") {
      graf_covid_diario
    } 
    
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
  # tab modelos --------------------------------------------------------------------------
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
  output$grafico_prediccion_red <- renderEcharts4r({
    grafico_prediccion_red
  })
  output$grafico_prediccion_arima <- renderEcharts4r({
    grafico_prediccion_arima
  })
  output$grafico_prediccion_naive  <- renderEcharts4r({
    grafico_prediccion_naive 
  })
  output$tabla_red_compra <- renderText({
    tabla_red_compra
  })
  output$tabla_red_venta <- renderText({
    tabla_red_venta
  })
  output$tabla_arima_venta <- renderText({
    tabla_arima_venta
  })
  output$tabla_arima_compra <- renderText({
    tabla_arima_venta
  })
  output$tabla_naive_compra <- renderText({
    tabla_naive_compra
  })
  output$tabla_naive_venta <- renderText({
    tabla_naive_venta
  })
})