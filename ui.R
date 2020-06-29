#Dependencias

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(echarts4r)

#Interfaz de usuario

shinyUI(
  dashboardPage(skin = "red",
    dashboardHeader(title = "Diagnostico economico CR",
                    titleWidth = 450),
    dashboardSidebar(
      sidebarMenu(
        id = "menu",
        menuItem("General", tabName = "general", icon = icon("chart-line")),
        menuItem("Situacion Nacional COVID", tabName = "covid"),
        menuItem("Exportaciones e importaciones", tabName = "expo_impo")
      )
    ),
    dashboardBody(
      tabItems(
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