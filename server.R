#dependencias
library(shiny)
library(shinydashboard)
library(tidyverse)

historico_dolar <- readRDS("historico_dolar.RDS")
grafico_casos <- readRDS("grafico_casos.RDS")
grafico_hospitalizados <- readRDS("grafico_hospitalizados.RDS")
grafico_provincia <- readRDS("grafico_provincia.RDS")

#server
shinyServer(function(input, output, session){
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