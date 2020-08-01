#dependencias
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(echarts4r)

# Modelos
grafico_prediccion_red <- readRDS("inputs_app/grafico_prediccion_red.RDS")
tabla_red_compra <- readRDS("inputs_app/tabla_red_compra.RDS")
tabla_red_venta <- readRDS("inputs_app/tabla_red_venta.RDS")
grafico_prediccion_arima <- readRDS("inputs_app/grafico_prediccion_arima.RDS")
tabla_arima_venta <- readRDS("inputs_app/tabla_arima_venta.RDS")
tabla_arima_compra <- readRDS("inputs_app/tabla_arima_compra.RDS")
grafico_prediccion_naive <- readRDS("inputs_app/grafico_prediccion_naive.RDS")
tabla_naive_compra <- readRDS("inputs_app/tabla_naive_compra.RDS")
tabla_naive_venta <- readRDS("inputs_app/tabla_naive_venta.RDS")

# Datos COVID
graf_covid_diario <- readRDS("inputs_app/graf_covid_diario.RDS")
graf_covid_acum <- readRDS("inputs_app/graf_covid_acum.RDS")
datos_covid_hoy <- readRDS("inputs_app/datos_covid_hoy.RDS")

# Datos tipo de cambio
graf_var_ventadolar <- readRDS("inputs_app/graf_var_ventadolar.RDS")
graf_var_compradolar <- readRDS("inputs_app/graf_var_compradolar.RDS")
dato_venta_y_compra <- readRDS("inputs_app/dato_venta_y_compra.RDS")

# Datos exportaciones
datos_exp <- readRDS("inputs_app/exporta.RDS")

# Datos importaciones
datos_imp <- readRDS("inputs_app/importa.RDS")

# Datos inflacion
graf_var_inflacion <- readRDS("inputs_app/graf_inflacion.RDS")
datos_inflacion <- readRDS("inputs_app/datos_inflacion.RDS")



#server
shinyServer(function(input, output, session){
  # Tab Resumen: columna Variables economicas --------------------------------------------------------------------------
  
  # Value boxes Indicadores Venta dolar --------------------------------------------------------------------------
  
  output$precio_venta <- renderValueBox({
    valueBox(value = dato_venta_y_compra[2, 2], 
             subtitle = "Precio en colones", 
             icon = icon("dollar-sign"),
             color = "teal")
  })
  
  output$cambio_venta <- renderValueBox({
    hoy <- dato_venta_y_compra[2, 2]
    ayer <- dato_venta_y_compra[1, 2]
    
    valueBox(value = ((hoy - ayer)/ayer)*100, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Value boxes Indicadores Compra dolar --------------------------------------------------------------------------
  
  output$precio_compra <- renderValueBox({
    valueBox(value = dato_venta_y_compra[2, 1], 
             subtitle = "Precio en colones", 
             icon = icon("dollar-sign"),
             color = "teal")
  })
  
  output$cambio_compra <- renderValueBox({
    hoy <- dato_venta_y_compra[2, 1]
    ayer <- dato_venta_y_compra[1, 1]
    
    valueBox(value = ((hoy - ayer)/ayer)*100, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Value boxes Indicadores Inflacion --------------------------------------------------------------------------
  
  output$valor_inflacion <- renderValueBox({
    
    valueBox(value = round(as.numeric(datos_inflacion[2, 2]), 4), 
             subtitle = "Inflacion % anual", 
             icon = icon("dollar-sign"),
             color = "teal")
  })
  
  output$cambio_inflacion <- renderValueBox({
    
    pasado <- datos_inflacion[1, 2]
    presente <- datos_inflacion[2, 2]
    
    valueBox(value = round((presente - pasado)/pasado, 3)*100, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  # Value boxes Indicadores Importaciones --------------------------------------------------------------------------
  
  output$valor_importaciones <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Importaciones en millones de $", 
             icon = icon("dollar-sign"),
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
             icon = icon("dollar-sign"),
             color = "teal")
  })
  
  output$cambio_exportaciones <- renderValueBox({
    valueBox(value = 10, 
             subtitle = "Tasa de cambio %", 
             icon = icon("arrows-alt-v"),
             color = "teal")
  })
  
  output$nota_datos <- renderText({
    
    as.character(input$prod)
    
  })
  
  # Tab Resumen: columna datos COVID --------------------------------------------------------------------------
  
  # Value boxes Casos nuevos --------------------------------------------------------------------------
  
  output$casos_nuevos <- renderValueBox({
    valueBox(value = datos_covid_hoy[1,1], 
             subtitle = "Casos nuevos", 
             icon = icon("user-plus"),
             color = "maroon")
  })
  
  output$cambio_nuevos <- renderValueBox({
    valueBox(value = datos_covid_hoy[1,2], 
             subtitle = "Tasa de cambio % casos nuevos", 
             icon = icon("arrows-alt-v"),
             color = "maroon")
  })
  
  # Value box Hospitalizados --------------------------------------------------------------------------
  
  output$valor_hospitalizados <- renderValueBox({
    valueBox(value = datos_covid_hoy[1,3], 
             subtitle = "Pacientes hospitalizados", 
             icon = icon("procedures"),
             color = "maroon")
  })
  
  # Value box UCI --------------------------------------------------------------------------
  
  output$valor_uci <- renderValueBox({
    valueBox(value = datos_covid_hoy[1,4], 
             subtitle = "Pacientes en UCI", 
             icon = icon("exclamation"),
             color = "maroon")
  })
  
  # Value box Recuperados --------------------------------------------------------------------------
  
  output$valor_recuperados <- renderValueBox({
    valueBox(value = datos_covid_hoy[1,5], 
             subtitle = "Personas recuperadas", 
             icon = icon("user-check"),
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
      
      # Generacion directa de grafico de exportaciones
      datos_imp %>% filter(Producto == input$prod) %>%
        e_charts(Fecha) %>%
        e_line(Monto) %>%
        e_title(text = NULL, subtext = "Millones USD") %>% 
        e_legend(right = "50") %>% 
        e_tooltip(trigger = "axis")  %>% 
        e_mark_point("Monto", data = list(type = "max")) %>% 
        e_datazoom()
      
    } else if (input$variables == "Exportaciones"){
      
      # Generacion directa de grafico de exportaciones
      datos_exp %>% filter(Producto == input$prod) %>%
        e_charts(Fecha) %>%
        e_line(Monto) %>%
        e_title(text = NULL, subtext = "Millones USD") %>% 
        e_legend(right = "50") %>% 
        e_tooltip(trigger = "axis")  %>% 
        e_mark_point("Monto", data = list(type = "max")) %>% 
        e_datazoom()
      
    }
    
  })
  
  output$nota_graficos <- renderText({
    
    as.character(input$prod)
    
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
