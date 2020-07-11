#Dependencias

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(echarts4r)

#Interfaz de usuario

shinyUI(
  dashboardPage(skin = "blue",
    dashboardHeader(title = "Indicadores económicos y COVID-19 de CR",
                    titleWidth = 450),
    dashboardSidebar(
      sidebarMenu(
        id = "menu",
        menuItem("Resumen", tabName = "resumen", icon = icon("sort-amount-up")),
        menuItem("Modelos", tabName = "modelos", icon = icon("chart-line")),
        menuItem("General", tabName = "general"),
        menuItem("Situacion Nacional COVID", tabName = "covid"),
        menuItem("Exportaciones e importaciones", tabName = "expo_impo")
      )
    ),
    dashboardBody(
      tabItems(
        # Tab Resumen --------------------------------------------------------------------------
        tabItem("resumen",
                h3("Indicadores"),
                selectInput(inputId = "variables", 
                            label = "Elija la variable macroeconómica a visualizar:", 
                            choices = c("Tipo de cambio: Venta $",
                                        "Tipo de cambio: Compra $",
                                        "Inflación",
                                        "Importaciones",
                                        "Exportaciones"
                                        ),
                            width = 1000),
                fluidRow(
                  # columna Variables economicas --------------------------------------------------------------------------
                    
                  column(
                    width = 6,
                    box(title = "Datos de variable macroeconomica", 
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = NULL,
                        status = "primary",
                        
                        # Indicadores Venta dolar --------------------------------------------------------------------------
                        
                        conditionalPanel(
                          condition = "input.variables == 'Tipo de cambio: Venta $'",
                          h4("Tipo de cambio: Venta del $"),
                          valueBoxOutput("precio_venta", width = NULL),
                          valueBoxOutput("cambio_venta", width = NULL)
                        ),
                        
                        # Indicadores Compra dolar --------------------------------------------------------------------------
                        
                        conditionalPanel(
                          condition = "input.variables == 'Tipo de cambio: Compra $'",
                          h4("Tipo de cambio: Compra $"),
                          valueBoxOutput("precio_compra", width = NULL),
                          valueBoxOutput("cambio_compra", width = NULL)
                        ),
                        
                        # Indicadores Inflacion --------------------------------------------------------------------------
                        
                        conditionalPanel(
                          condition = "input.variables == 'Inflación'",
                          h4("Inflación comercial"), 
                          valueBoxOutput("valor_inflacion", width = NULL),
                          valueBoxOutput("cambio_inflacion", width = NULL)
                        ),
                        
                        # Indicadores Importaciones --------------------------------------------------------------------------
                        
                        conditionalPanel(
                          condition = "input.variables == 'Importaciones'",
                          h4("Importaciones (CIF)"),
                          valueBoxOutput("valor_importaciones", width = NULL),
                          valueBoxOutput("cambio_importaciones", width = NULL)
                        ),
                        
                        # Indicadores Exportaciones --------------------------------------------------------------------------
                        
                        conditionalPanel(
                          condition = "input.variables == 'Exportaciones'",
                          h4("Exportaciones (FOB)"), 
                          valueBoxOutput("valor_exportaciones", width = NULL),
                          valueBoxOutput("cambio_exportaciones", width = NULL)
                        )
                    ),
                  ),
                  # columna Datos COVID --------------------------------------------------------------------------
                  
                  column(
                    width = 6,
                    box(title = "Datos del COVID-19", 
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = NULL,
                        status = "primary",
                        h4("Información al dia de hoy"), 
                        h5(""),
                        valueBoxOutput("casos_nuevos", width = NULL),
                        valueBoxOutput("cambio_nuevos", width = NULL),
                        valueBoxOutput("valor_hospitalizados", width = NULL),
                        valueBoxOutput("valor_uci", width = NULL),
                        valueBoxOutput("valor_recuperados", width = NULL)
                    )
                  )
                ),
                h3("Series de tiempo"),
                fluidRow(
                  column(
                    width = 12,
                    box(title = "Grafico de variable economica",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = NULL,
                        status = "primary",
                        echarts4rOutput("graf_var")
                    )
                  )
                )
        ),
        
        tabItem("general",
          h3("Tipo de cambio al dia de hoy"),
          fluidRow(
            valueBoxOutput("compra_dolar"),
            valueBoxOutput("venta_dolar")
          ),
          h3("Historico del cambio de compra y venta dolar"),
          fluidRow(
            echarts4rOutput("historico_dolar", height = "50vh")
          )
        ),
        tabItem("covid",
          h2("Datos del COVID-19 en Costa Rica"),
          h4("Positivos, recuperados y activos"),
          fluidRow(
            plotOutput("grafico_casos", height = "50vh")
          ),
          h4("Cantidad de personas hospitalizadas y en UCI"),
          fluidRow(
            plotOutput("grafico_hospitalizados")
          ),
          h4("Cantidad de casos activos por provincia"),
          fluidRow(
            plotOutput("grafico_provincia")
          )
        )
      )
    )
  )
)