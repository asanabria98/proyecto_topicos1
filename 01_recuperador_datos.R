# Dependencias -------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(echarts4r)
library(gapminder)
library(tidyr)
library(rlang)
library(stringr)
library(xts)
library(kableExtra)
library(forecast)
library(devtools)
library(magrittr)
library(dplyr)


# Datos COVID -------------------------------------------------------------------------------------------------------------------------------------

datosCOVID <- data.frame(read.csv("datos/datos_COVID.csv",header=TRUE,sep=","))

# Datos para Value Boxes
datosgenerales <- datosCOVID[, c(1, 3, 28, 32, 38)]

names(datosgenerales) <- c("Fecha","Positivos","Hospitalizados", "UCI", "Recuperados")

#datosgenerales <- datosgenerales %>%
#  mutate(Fecha=as.Date(as.character(Fecha), format = "%d/%m/%Y"))

fin <- nrow(datosgenerales)

cambio_positivos <- data.frame(cambio_positivos = round((datosgenerales$Positivos[fin] - datosgenerales$Positivos[fin - 1])*100 / datosgenerales$Positivos[fin - 1], 2))

datos_covid_hoy <- tail(datosgenerales, 1)

datos_covid_hoy <- cbind(datos_covid_hoy, cambio_positivos)

saveRDS(datos_covid_hoy, file = "inputs_app/datos_covid_hoy.RDS")



# Series de tiempo
datosSerie <- datosCOVID%>%
  select(FECHA,positivos,nue_posi,RECUPERADOS,NUE_RECUP,fallecidos,nue_falleci)

datosSerie$FECHA = as.Date(datosSerie$FECHA,format="%d/%m/%Y")

nombres <- c("Fecha","Positivos","Nuevos_Positivos","Recuperados","Nuevos_Recuperados",
             "Fallecidos","Nuevos_Fallecidos")
names(datosSerie) <- nombres

graf_covid_acum <- datosSerie %>% 
  e_charts(x = Fecha) %>% # initialise and set x
  e_line(serie = Positivos, smooth=TRUE) %>%  # add a line
  e_line(serie = Recuperados, smooth=TRUE) %>% 
  e_line(serie = Fallecidos, smooth=TRUE) %>% 
  e_title(text = NULL, subtext = "Cantidad pacientes") %>% 
  e_x_axis(min = ymd("2020/03/01")) %>% 
  e_legend(right = "50") %>% 
  #e_mark_point(data = max) %>% marcar el dia con mas casos
  e_datazoom() %>% #barra para seleccionar el segmento de tiempo que se desea ver
  e_tooltip(trigger = "axis") %>% 
  e_mark_point("Positivos", data = list(type = "max")) %>% #marcar el dia con mas casos
  e_mark_point("Recuperados", data = list(type = "max")) %>% 
  e_mark_point("Fallecidos", data = list(type = "max"))

saveRDS(graf_covid_acum, file = "inputs_app/graf_covid_acum.RDS")

graf_covid_diario <- datosSerie %>% 
  e_charts(x = Fecha) %>% # initialise and set x
  e_line(serie = Nuevos_Positivos, smooth = TRUE, name = "Positivos") %>%  # add a line
  e_line(serie = Nuevos_Recuperados, smooth = TRUE, name = "Recuperados") %>% 
  e_line(serie = Nuevos_Fallecidos, smooth = TRUE, name = "Fallecidos") %>% 
  e_title(text = NULL, subtext = "Cantidad pacientes") %>% 
  #e_theme("chalk") %>% 
  e_x_axis(min = ymd("2020/03/01")) %>% 
  e_legend(right = "50") %>% 
  e_datazoom() %>% #barra para seleccionar el segmento de tiempo que se desea ver
  e_tooltip(trigger = "axis") %>% 
  e_mark_point("Positivos", data = list(type = "max")) %>% #marcar el dia con mas casos
  e_mark_point("Recuperados", data = list(type = "max")) %>% 
  e_mark_point("Fallecidos", data = list(type = "max"))

saveRDS(graf_covid_diario, file = "inputs_app/graf_covid_diario.RDS")

fechamin <- min(datosSerie$Fecha)
fechamax <- max(datosSerie$Fecha)

# Tipo de cambio -----------------------------------------------------------------------------

#Descarga automatica de los datos
#src <- "https://gee.bccr.fi.cr/indicadoreseconomicos/Cuadros/frmVerCatCuadro.aspx?CodCuadro=400&Idioma=1&FecInicial=2020/03/06&FecFinal=2020/07/20&Filtro=0"
#lcl <- basename(src)
#download.file(url = src, destfile = lcl)

datos_tipocambio <- read_excel(path = "datos/datos_tipocambio.xlsx", sheet = 1, 
                    col_names = TRUE, 
                    skip = 4)

#limpieza de datos
names(datos_tipocambio) <- c("fecha","compra","venta")

datos_tipocambio$fecha <- str_replace(datos_tipocambio$fecha, "Ago", "Aug")
datos_tipocambio$fecha <- str_replace(datos_tipocambio$fecha, "Set", "Sep")
datos_tipocambio$fecha <- str_replace(datos_tipocambio$fecha,"Dic","Dec")
datos_tipocambio$fecha <- str_replace(datos_tipocambio$fecha,"Ene","Jan")
datos_tipocambio$fecha <- str_replace(datos_tipocambio$fecha,"Abr","Apr")
datos_tipocambio$fecha <- dmy(datos_tipocambio$fecha)
datos_tipocambio <- na.omit(datos_tipocambio)

# Indicador Compra dolar del dia actual
dato_venta_y_compra <- tail(datos_tipocambio, 2) %>% 
  mutate(cambio_compra = (compra - lag(compra))*100/lag(compra),
         cambio_venta = (venta - lag(venta))*100/lag(venta))

saveRDS(dato_venta_y_compra, file = "inputs_app/dato_venta_y_compra.RDS") 

##historico dolar  

graf_var_ventadolar <- datos_tipocambio %>%
  e_charts(fecha) %>%
  e_line(venta, name = "Venta en colones", smooth = TRUE) %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = "50") %>% 
  e_x_axis(max = fechamax) %>%
  e_y_axis(min = 540) %>%
  e_title(text = NULL, subtext = "Precio en colones") %>% 
  e_datazoom() #%>% 
  #e_theme("infographic")

saveRDS(graf_var_ventadolar, file = "inputs_app/graf_var_ventadolar.RDS")  

graf_var_compradolar <- datos_tipocambio %>%
  e_charts(fecha) %>%
  e_line(compra, name = "Compra en colones", smooth = TRUE) %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = "50") %>% 
  e_x_axis(max = fechamax) %>%
  e_y_axis(min = 540) %>%
  e_axis_labels(x = "Dia",y = "Precio en colones") %>%
  e_datazoom() #%>% 
#e_theme("infographic")

saveRDS(graf_var_compradolar, file = "inputs_app/graf_var_compradolar.RDS") 


# modelos de estimacion tipo de cambio --------------------------------------------------------------------------

#Obtener fechas mensuales
fecha_final_mes <-  datos_tipocambio %>%
  mutate(mes = month(fecha), ano = year(fecha)) %>%
  group_by(mes, ano) %>%
  summarise(fecha = max(fecha))%>%
  arrange(fecha) %>%
  ungroup() %>%
  select(fecha)

datos_mensuales <- datos_tipocambio %>%
  filter(fecha %in% as.Date(fecha_final_mes$fecha))

#convertir a time series compra y venta
datos_compra <- xts(datos_mensuales$compra, order.by = datos_mensuales$fecha)
datos_venta <- xts(datos_mensuales$venta, order.by = datos_mensuales$fecha)

# creacion set de entrenamiento y de prueba
train_compra <- head(datos_compra, n = 24)
test_compra <- tail(datos_compra, n = 6)

train_venta <- head(datos_venta, n = 24)
test_venta <- tail(datos_venta, n = 6)

#ajuste red neuronal autoregresiva

fit_red_compra <- nnetar(train_compra)
prediccion_red_compra <- forecast(fit_red_compra, h = 6)

fit_red_compra_final <- nnetar(datos_compra)
prediccion_red_compra_final <- forecast(fit_red_compra, h = 6)

fit_red_venta <- nnetar(train_venta)
prediccion_red_venta <- forecast(fit_red_venta, h = 6)

fit_red_venta_final <- nnetar(datos_venta)
prediccion_red_venta_final <- forecast(fit_red_venta_final, h = 6)

# almacenar info en data frame

df_prediccion_red <- data.frame(compra = prediccion_red_compra$mean, venta = prediccion_red_venta$mean, 
                                fecha = c( "marzo", "abril", "mayo", "junio","julio","agosto"))

grafico_prediccion_red<- df_prediccion_red%>%
  e_chart(fecha)%>%
  e_line(compra)%>%
  e_line(venta)%>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(name = "Prediccion Precio de compra y venta", min = 550, nameGap = 40, nameLocation = "center")%>%
  e_x_axis(name = "Mes de estudio", nameLocation = "center", nameGap = 40, margin = 0)%>%
  e_text_style(color = "black", fontsize = 12) %>%
  e_grid(right = 60, top = 90, width = "85%")%>%
  e_theme("roma")
saveRDS(grafico_prediccion_red, file = "inputs_app/grafico_prediccion_red.RDS")

#tablas para medir el ajuste
tabla_red_compra <- kable(accuracy(prediccion_red_compra,test_compra)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

saveRDS(tabla_red_compra, file = "inputs_app/tabla_red_compra.RDS")

tabla_red_venta <- kable(accuracy(prediccion_red_venta,test_venta)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_red_venta, file = "inputs_app/tabla_red_venta.RDS")

#tabla prediciones red
tabla_red_pred <- data.frame(mes = c("septiembre","octubre","noviembre","diciembre","enero","febrero") ,
                             Compra = prediccion_red_compra_final, 
                             Venta =  prediccion_red_venta_final )
colnames(tabla_red_pred) <- c("Mes","Compra","Venta")
tabla_red_pred <- kable(tabla_red_pred) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_red_pred, file = "inputs_app/tabla_red_prediccion_final.RDS")

#ajuste arima

fit_arima_compra <- auto.arima(train_compra, stepwise =FALSE)
prediccion_arima_compra <- forecast(fit_arima_compra, h = 6)

fit_arima_venta <- auto.arima(train_venta, stepwise =FALSE)
prediccion_arima_venta <- forecast(fit_arima_venta, h = 6) 

fit_arima_compra_final <- auto.arima(datos_compra, stepwise = TRUE)
prediccion_arima_compra_final <- forecast(fit_arima_compra, h = 6)

fit_arima_venta_final <- auto.arima(datos_venta, stepwise =FALSE)
prediccion_arima_venta_final <- forecast(fit_arima_venta_final, h = 6) 

#tabla prediciones arima
tabla_arima_pred <- data.frame(mes = c("septiembre","octubre","noviembre","diciembre","enero","febrero") ,
                             Compra = prediccion_arima_compra_final$mean, 
                             Venta =  prediccion_arima_venta_final$mean )

tabla_arima_pred <- kable(tabla_arima_pred) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

saveRDS(tabla_arima_pred, file = "inputs_app/tabla_arima_prediccion_final.RDS")

# almacenar info en data frame
df_prediccion_arima <- data.frame(compra = prediccion_arima_compra$mean, venta = prediccion_arima_venta$mean, 
                                  fecha = c( "marzo", "abril", "mayo", "junio","julio","agosto"))
# graficar
grafico_prediccion_arima <- df_prediccion_arima%>%
  e_chart(fecha)%>%
  e_line(compra)%>%
  e_line(venta)%>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(name = "Predicción Precio de compra y venta", min = 550, nameGap = 40, nameLocation = "center")%>%
  e_x_axis(name = "Mes de estudio", nameLocation = "center", nameGap = 40, margin = 0)%>%
  e_text_style(color = "black", fontsize = 12) %>%
  e_grid(right = 60, top = 90, width = "85%")%>%
  e_theme("roma")
saveRDS(grafico_prediccion_arima, file = "inputs_app/grafico_prediccion_arima.RDS")
#tablas para medir el ajuste

tabla_arima_venta <- kable(accuracy(prediccion_arima_compra,test_compra)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_arima_venta, file = "inputs_app/tabla_arima_venta.RDS")

tabla_arima_compra <- kable(accuracy(prediccion_arima_venta,test_venta)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_arima_compra, file = "inputs_app/tabla_arima_compra.RDS")

#naive

prediccion_naive_compra <- naive(train_compra, h = 6)
prediccion_naive_venta <- naive(train_venta, h = 6)

# almacenar info en data frame
df_prediccion_naive <- data.frame(compra= prediccion_naive_compra$mean, venta = prediccion_naive_venta$mean, 
                                  fecha = c( "marzo", "abril", "mayo", "junio","julio","agosto"))

# graficar
grafico_prediccion_naive <- df_prediccion_naive%>%
  e_chart(fecha)%>%
  e_line(compra)%>%
  e_line(venta)%>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(name = "Prediccion Precio de compra y venta", min = 550, nameGap = 40, nameLocation = "center")%>%
  e_x_axis(name = "Mes de estudio", nameLocation = "center", nameGap = 40, margin = 0)%>%
  e_text_style(color = "black", fontsize = 12) %>%
  e_grid(right = 60, top = 90, width = "85%")%>%
  e_theme("roma")

saveRDS(grafico_prediccion_naive, file = "inputs_app/grafico_prediccion_naive.RDS")

#tablas para medir el ajuste
tabla_naive_compra <- kable(accuracy(prediccion_naive_compra,test_compra)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_naive_compra, file = "inputs_app/tabla_naive_compra.RDS")

tabla_naive_venta <- kable(accuracy(prediccion_naive_venta, test_venta)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_naive_venta, file = "inputs_app/tabla_naive_venta.RDS")



# Inflacion Comercial Porcentual ------------------------------------------

inflacion_raw <- read_excel("datos/IndicadorInflacionSociosComerciales/frmVerCatCuadro.xlsx", range = "Hoja1!A5:R17")
names(inflacion_raw)[1] <- c("Mes")
inflacion_raw$Mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
inflacion <- gather(inflacion_raw, Año, Porcentaje, -Mes)
inflacion <- unite(inflacion, Fecha, Año, Mes, sep = "-")

inflacion$Fecha <- seq(from = as.Date("2004-01-01"), to = as.Date("2020-12-01"), by = "month")

# Limpiamos ambiente
rm(inflacion_raw)

#Ultimos datos inflacion

inflacion_filtrado <- inflacion %>%
  filter(Fecha > ymd("2020-02-01"),
         !is.na(Porcentaje))

datos_inflacion <- tail(inflacion_filtrado, 2) %>% 
  mutate(cambio_inflacion = (Porcentaje - lag(Porcentaje))*100/lag(Porcentaje))

saveRDS(datos_inflacion, file = "inputs_app/datos_inflacion.RDS")

#Grafico de Inflacion

max_y_axis <- round(max(inflacion_filtrado$Porcentaje) * 1.5, digits = 1)

graf_inflacion <- inflacion_filtrado  %>% 
  e_charts(Fecha) %>%
  e_line(Porcentaje) %>%
  e_title(text = NULL, subtext = "Porcentaje Inflacional") %>% 
  #e_title("Indicador de Inflación de Socios Comerciales") %>%
  e_y_axis(max = max_y_axis) %>% 
  e_x_axis(max = fechamax) %>% 
  e_legend(right = "50") %>% 
  e_tooltip(trigger = "axis")  %>% 
  #e_animation(duration = 5000) %>%
  e_mark_point("Porcentaje", data = list(type = "max")) %>% 
  e_datazoom() #%>% 
#e_toolbox()
#e_toolbox_feature(feature = "magicType", type = list("bar", "line"))


saveRDS(graf_inflacion, file = "inputs_app/graf_inflacion.RDS")

 
# Exportaciones ------------------------------------------

exporta_raw <- read_excel("datos/ExportacionesFOB/frmVerCatCuadro.xlsx", range = "Hoja1!A5:CX124")
names(exporta_raw)[1] <- c("Producto")

exporta <- gather(exporta_raw, Fecha, Monto, -Producto)
exporta <- spread(exporta, Producto, Monto)
exporta <- separate(exporta, Fecha, c("Mes", "Año"))


# Orden de los datos
exporta$Mes[exporta$Mes == "Enero"] <- "a"
exporta$Mes[exporta$Mes == "Febrero"] <- "b"
exporta$Mes[exporta$Mes == "Marzo"] <- "c"
exporta$Mes[exporta$Mes == "Abril"] <- "d"
exporta$Mes[exporta$Mes == "Mayo"] <- "e"
exporta$Mes[exporta$Mes == "Junio"] <- "f"
exporta$Mes[exporta$Mes == "Julio"] <- "g"
exporta$Mes[exporta$Mes == "Agosto"] <- "h"
exporta$Mes[exporta$Mes == "Septiembre"] <- "i"
exporta$Mes[exporta$Mes == "Octubre"] <- "j"
exporta$Mes[exporta$Mes == "Noviembre"] <- "k"
exporta$Mes[exporta$Mes == "Diciembre"] <- "l"

exporta <- exporta %>% arrange(Año, Mes)


# Limpiamos ambiente
rm(exporta_raw)

# Ajustamos fechas

exporta <- unite(exporta, Fecha, Mes, Año, sep = " ")
exporta$Fecha <- seq(from = as.Date("2012-01-01"), to = as.Date("2020-05-01"), by = "month")

#Reacomodar por producto
exporta <- gather(exporta, Producto, Monto, -Fecha)

#Filtrado fechas pertinentes
exporta_filtrada <- exporta %>%
  filter(Fecha > ymd("2020-02-01"))

#Se guarda datos para ValueBoxes
saveRDS(exporta_filtrada, file = "inputs_app/exporta.RDS")


# Importaciones ------------------------------------------

importa_raw <- read_excel("datos/ImportacionesCIF/frmVerCatCuadro.xlsx", range = "Hoja1!A5:CM124")

names(importa_raw)[1] <- c("Producto")

importa <- gather(importa_raw, Fecha, Monto, -Producto)
importa <- spread(importa, Producto, Monto)
importa <- separate(importa, Fecha, c("Mes", "Año"))


# Orden de los datos
importa$Mes[importa$Mes == "Enero"] <- "a"
importa$Mes[importa$Mes == "Febrero"] <- "b"
importa$Mes[importa$Mes == "Marzo"] <- "c"
importa$Mes[importa$Mes == "Abril"] <- "d"
importa$Mes[importa$Mes == "Mayo"] <- "e"
importa$Mes[importa$Mes == "Junio"] <- "f"
importa$Mes[importa$Mes == "Julio"] <- "g"
importa$Mes[importa$Mes == "Agosto"] <- "h"
importa$Mes[importa$Mes == "Septiembre"] <- "i"
importa$Mes[importa$Mes == "Octubre"] <- "j"
importa$Mes[importa$Mes == "Noviembre"] <- "k"
importa$Mes[importa$Mes == "Diciembre"] <- "l"

importa <- importa %>% arrange(Año, Mes)

# Limpiamos ambiente
rm(importa_raw)

# Ajustamos fechas

importa <- unite(importa, Fecha, Mes, Año, sep = " ")
importa$Fecha <- seq(from = as.Date("2012-12-01"), to = as.Date("2020-05-01"), by = "month")

#Reacomodar por producto
importa <- gather(importa, Producto, Monto, -Fecha)

#Filtrado fechas pertinentes
importa_filtrada <- importa %>%
  filter(Fecha > ymd("2020-02-01"))

#Se guarda en RDS
saveRDS(importa_filtrada, file = "inputs_app/importa.RDS")

