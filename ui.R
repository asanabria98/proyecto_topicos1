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
                    menuItem("Modelos", tabName = "modelos", icon = icon("chart-line"))
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
                              
                                conditionalPanel(
                                  condition = "(input.variables == 'Importaciones' || input.variables == 'Exportaciones')",
                                  column(
                                    width = 6,
                                    selectInput(inputId = "prod", 
                                                label = "Elija el producto de importación a visualizar:", 
                                                choices = paste0("NP", 
                                                                 c(paste0("00",
                                                                          seq(1,9)),
                                                                   paste0("0",
                                                                          seq(10,99)), 
                                                                   paste0("",seq(100,118))) 
                                                ) , 
                                                selected = "NP005",
                                                width = NULL)),
                                  column(
                                    width = 6,
                                    downloadButton("Clasificacion", "Descargar clasificación de productos")
                                  ) 
                              )
                            ),
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
                                      valueBoxOutput("cambio_venta", width = NULL),
                                      h5("* Datos actualizados al día: ", textOutput(outputId = "fechaVenta"))
                                    ),
                                    
                                    # Indicadores Compra dolar --------------------------------------------------------------------------
                                    
                                    conditionalPanel(
                                      condition = "input.variables == 'Tipo de cambio: Compra $'",
                                      h4("Tipo de cambio: Compra $"),
                                      valueBoxOutput("precio_compra", width = NULL),
                                      valueBoxOutput("cambio_compra", width = NULL),
                                      h5("* Datos actualizados al día: ", textOutput(outputId = "fechaCompra"))
                                    ),
                                    
                                    # Indicadores Inflacion --------------------------------------------------------------------------
                                    
                                    conditionalPanel(
                                      condition = "input.variables == 'Inflación'",
                                      h4("Inflación comercial"), 
                                      valueBoxOutput("valor_inflacion", width = NULL),
                                      valueBoxOutput("cambio_inflacion", width = NULL),
                                      h5("* Datos actualizados al día: ", textOutput(outputId = "fechaInflacion"))
                                    ),
                                    
                                    # Indicadores Importaciones --------------------------------------------------------------------------
                                    
                                    conditionalPanel(
                                      condition = "input.variables == 'Importaciones'",
                                      h4("Importaciones (CIF)"),
                                      valueBoxOutput("valor_importaciones", width = NULL),
                                      valueBoxOutput("cambio_importaciones", width = NULL),
                                      h5("* Datos actualizados al día: ", textOutput(outputId = "fechaImp"))
                                    ),
                                    
                                    # Indicadores Exportaciones --------------------------------------------------------------------------
                                    
                                    conditionalPanel(
                                      condition = "input.variables == 'Exportaciones'",
                                      h4("Exportaciones (FOB)"),
                                      valueBoxOutput("valor_exportaciones", width = NULL),
                                      valueBoxOutput("cambio_exportaciones", width = NULL),
                                      h5("* Datos actualizados al día: ", textOutput(outputId = "fechaExp"))
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "(input.variables == 'Importaciones' || input.variables == 'Exportaciones')",
                                      h5("* Datos del producto con codigo ", textOutput(outputId = "nota_product"))
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
                                    h4("Principales indicadores"), 
                                    h5(""),
                                    valueBoxOutput("casos_nuevos", width = NULL),
                                    valueBoxOutput("cambio_nuevos", width = NULL),
                                    valueBoxOutput("valor_hospitalizados", width = NULL),
                                    valueBoxOutput("valor_uci", width = NULL),
                                    valueBoxOutput("valor_recuperados", width = NULL),
                                    h5("* Datos actualizados al día: ", textOutput(outputId = "fechaCOVID"))
                                )
                              )
                            ),
                            # Fila y columna de series de tiempo --------------------------------------------------------------------------
                            
                            h3("Series de tiempo"),
                            fluidRow(
                              column(
                                width = 12,
                                box(title = "Grafico de variable economica",
                                    collapsible = TRUE,
                                    solidHeader = TRUE,
                                    width = NULL,
                                    status = "primary",
                                    h4(textOutput(outputId = "titulo_graf_var")),
                                    echarts4rOutput("graf_var"),
                                    conditionalPanel(
                                      condition = "(input.variables == 'Importaciones' || input.variables == 'Exportaciones')",
                                      h5("Datos del producto con codigo ", textOutput(outputId = "nota_graficos"))
                                    )
                                ),
                                box(title = "Grafico de variables asociadas al COVID-19",
                                    collapsible = TRUE,
                                    solidHeader = TRUE,
                                    width = NULL,
                                    status = "primary",
                                    selectInput(inputId = "opciones_graf_covid",
                                                label = "Elija el tipo de datos a visualizar:",
                                                choices = c("Datos acumulados",
                                                            "Datos diarios"
                                                )
                                    ),
                                    h4(textOutput(outputId = "titulo_graf_covid")),
                                    echarts4rOutput("graf_covid")
                                )
                              )
                            )
                    ),
                    tabItem("modelos",
                            h3("Pronostico del tipo cambio de compra y de venta del dólar de USA"),
                            selectInput(inputId = "op_modelos",
                                        label = "Seleccione un modelo",
                                        choices = c("Red Neuronal Autorregresiva",
                                                    "ARIMA",
                                                    "Naive"),
                                        width = 1000),
                            fluidRow(
                              conditionalPanel(
                                condition = "input.op_modelos == 'Red Neuronal Autorregresiva'",
                                echarts4rOutput("grafico_prediccion_red"),
                                h4(strong("Metricas de desempeño para la variable Compra")),
                                htmlOutput("tabla_red_compra"),
                                h4(strong("Metricas de desempeño para la variable Venta")),
                                htmlOutput("tabla_red_venta"),
                                h4(strong("Prediccion para los proximos 6 meses")),
                                htmlOutput("tabla_red_prediccion_final")
                              ),
                              conditionalPanel(
                                condition = "input.op_modelos == 'ARIMA'",
                                echarts4rOutput("grafico_prediccion_arima"),
                                h4(strong("Metricas de desempeño para la variable Compra")),
                                htmlOutput("tabla_arima_compra"),
                                h4(strong("Metricas de desempeño para la variable Venta")),
                                htmlOutput("tabla_arima_venta"),
                                h4(strong("Prediccion para los proximos 6 meses")),
                                htmlOutput("tabla_arima_prediccion_final")
                              ),
                              conditionalPanel(
                                condition = "input.op_modelos == 'Naive'",
                                echarts4rOutput("grafico_prediccion_naive"),
                                h4(strong("Metricas de desempeño para la variable Compra")),
                                htmlOutput("tabla_naive_compra"),
                                h4(strong("Metricas de desempeño para la variable Venta")),
                                htmlOutput("tabla_naive_venta")
                              )
                            )
                    )
                  )
                )
  )
)