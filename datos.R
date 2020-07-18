#dependencias
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


#importar datos
datos <- read_excel("datos/datos.xlsx")

#limpieza de datos
names(datos) <- c("fecha","compra","venta")

datos$fecha <- str_replace(datos$fecha, "Ago", "Aug")
datos$fecha <- str_replace(datos$fecha, "Set", "Sep")
datos$fecha <- str_replace(datos$fecha,"Dic","Dec")
datos$fecha <- str_replace(datos$fecha,"Ene","Jan")
datos$fecha <- str_replace(datos$fecha,"Abr","Apr")
datos$fecha <- dmy(datos$fecha)

# Indicador Compra dolar del dia actual

compra_dolar <- datos[nrow(datos),2]
venta_dolar <- datos[nrow(datos),3]

#hacer un save RDS por aca

##historico dolar  

historico_dolar <- datos %>%
  e_charts(fecha) %>%
  e_line(compra, name = "Tipo cambio compra") %>%
  e_line(venta, name = "Tipo cambio venta") %>%
  e_tooltip(trigger = "axis") %>%
  e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
  e_y_axis(min = 500) %>%
  e_text_style(fontSize = 12)


saveRDS(historico_dolar, file = "historico_dolar.RDS")  


##COVID

# Cragar y transformar datos -------------------------------------------------------------------------------------------------------------------------------------

datosxcanton <- data.frame(read.csv("datos/covid19_cantones_cr.csv",header=TRUE,sep=","))
datosgenerales <- data.frame(read.csv("datos/covid19_general_cr.csv",header=TRUE,sep=","))

datosgenerales <- datosgenerales %>%
  select(Fecha,Confirmados,Recuperados,Hospitalizados,CI)%>%
  mutate(Fecha=as.Date(as.character(Fecha), format = "%d/%m/%Y"),
         Activos=Confirmados-Recuperados)

datos_1 <- datosxcanton %>% 
  pivot_longer(-c(provincia, canton),
               names_to = "fecha", values_to = "total") %>% 
  filter(canton != "DESCONOCIDO") %>% 
  filter(!is.na(total)) %>% 
  mutate(fecha = str_replace_all(fecha,"\\.", "-"),
         fecha = str_remove(fecha,"X"),
         fecha = as.Date(fecha, format = "%d-%m-%Y"))

datos_1 <- datos_1 %>% 
  group_by(provincia, fecha) %>% 
  summarize(total = sum(total))

datos_totales <- datos_1 %>% 
  filter(fecha==as.Date("2020-06-08", format = "%Y-%m-%d")) %>% 
  ungroup(provincia) %>% 
  mutate(provincia = as.character(provincia)) %>% 
  arrange(desc(total))

grafico_casos <-  ggplot(datosgenerales, aes(x=Fecha))+
  geom_line(aes(y= Confirmados),color="red")+
  geom_line(aes(y=Recuperados),color="blue")+
  geom_point(aes(y= Confirmados),color="red")+
  geom_point(aes(y=Recuperados),color="blue") +
  geom_line(aes(y=Activos),color="yellow")+
  geom_point(aes(y= Activos),color="yellow")+
  labs(title="Casos de COVID-19 en Costa Rica")+
  theme_light()


saveRDS(grafico_casos, file = "grafico_casos.RDS")


grafico_hospitalizados <- ggplot(datosgenerales, aes(x=Fecha, y=Hospitalizados))+
  geom_line(color="red")+
  geom_line(aes(y=CI),color="blue")+
  geom_point(color="red")+
  geom_point(aes(y=CI),color="blue") + 
  labs(title="Personas hospitalizadas por COVID-19 en Costa Rica")+
  theme_light()

saveRDS(grafico_hospitalizados, file = "grafico_hospitalizados.RDS")

grafico_provincia <- ggplot(datos_totales, aes(x=total, y=provincia, label = total))+
  geom_col(aes(fill=provincia))+
  labs(title="Personas hospitalizadas por COVID-19 en Costa Rica")+
  theme_classic()+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))


saveRDS(grafico_provincia, file = "grafico_provincia.RDS")

# modelos de estimacion tasa de cambio --------------------------------------------------------------------------

#Obtener fechas mensuales
fecha_final_mes <-  datos %>%
  mutate(mes = month(fecha), ano = year(fecha)) %>%
  group_by(mes, ano) %>%
  summarise(fecha = max(fecha))%>%
  arrange(fecha) %>%
  ungroup() %>%
  select(fecha)

datos_mensuales <- datos %>%
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

fit_red_venta <- nnetar(train_venta)
prediccion_red_venta <- forecast(fit_red_venta, h = 6)

# almacenar info en data frame

df_prediccion_red <- data.frame(compra = prediccion_red_compra$mean, venta = prediccion_red_venta$mean, 
                                fecha = c("enero", "febrero", "marzo", "abril", "mayo", "junio"))

grafico_prediccion_red<- df_prediccion_red%>%
  e_chart(fecha)%>%
  e_line(compra)%>%
  e_line(venta)%>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(name = "Predicci?n Precio de compra y venta", min = 510, nameGap = 40, nameLocation = "center")%>%
  e_x_axis(name = "Mes de estudio", nameLocation = "center", nameGap = 40, margin = 0)%>%
  e_text_style(color = "black", fontsize = 12) %>%
  e_grid(right = 60, top = 90, width = "85%")%>%
  e_theme("roma")
saveRDS(grafico_prediccion_red, file = "grafico_prediccion_red.RDS")

#tablas para medir el ajuste
tabla_red_compra <- kable(accuracy(prediccion_red_compra,test_compra)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

saveRDS(tabla_red_compra, file = "tabla_red_compra.RDS")

tabla_red_venta <- kable(accuracy(prediccion_red_venta,test_venta)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_red_venta, file = "tabla_red_venta.RDS")

#ajuste arima

fit_arima_compra <- auto.arima(train_compra, stepwise =FALSE)
prediccion_arima_compra <- forecast(fit_arima_compra, h = 6)

fit_arima_venta <- auto.arima(train_venta, stepwise =FALSE)
prediccion_arima_venta <- forecast(fit_arima_venta, h = 6) 

# almacenar info en data frame
df_prediccion_arima <- data.frame(compra = prediccion_arima_compra$mean, venta = prediccion_arima_venta$mean, 
                                  fecha = c("enero", "febrero", "marzo", "abril", "mayo", "junio"))
# graficar
grafico_prediccion_arima <- df_prediccion_arima%>%
  e_chart(fecha)%>%
  e_line(compra)%>%
  e_line(venta)%>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(name = "Predicci?n Precio de compra y venta", min = 510, nameGap = 40, nameLocation = "center")%>%
  e_x_axis(name = "Mes de estudio", nameLocation = "center", nameGap = 40, margin = 0)%>%
  e_text_style(color = "black", fontsize = 12) %>%
  e_grid(right = 60, top = 90, width = "85%")%>%
  e_theme("roma")
saveRDS(grafico_prediccion_arima, file = "grafico_prediccion_arima.RDS")
#tablas para medir el ajuste

tabla_arima_venta <- kable(accuracy(prediccion_arima_compra,test_compra)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_arima_venta, file = "tabla_arima_venta.RDS")

tabla_arima_compra <- kable(accuracy(prediccion_arima_venta,test_venta)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_arima_compra, file = "tabla_arima_compra.RDS")

#naive

prediccion_naive_compra <- naive(train_compra, h = 6)
prediccion_naive_venta <- naive(train_venta, h = 6)

# almacenar info en data frame
df_prediccion_naive <- data.frame(compra= prediccion_naive_compra$mean, venta = prediccion_naive_venta$mean, 
                                  fecha = c("enero", "febrero", "marzo", "abril", "mayo", "junio"))

# graficar
grafico_prediccion_naive <- df_prediccion_naive%>%
  e_chart(fecha)%>%
  e_line(compra)%>%
  e_line(venta)%>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(name = "Predicci?n Precio de compra y venta", min = 510, nameGap = 40, nameLocation = "center")%>%
  e_x_axis(name = "Mes de estudio", nameLocation = "center", nameGap = 40, margin = 0)%>%
  e_text_style(color = "black", fontsize = 12) %>%
  e_grid(right = 60, top = 90, width = "85%")%>%
  e_theme("roma")

saveRDS(grafico_prediccion_naive, file = "grafico_prediccion_naive.RDS")

#tablas para medir el ajuste
tabla_naive_compra <- kable(accuracy(prediccion_naive_compra,test_compra)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_naive_compra, file = "tabla_naive_compra.RDS")

tabla_naive_venta <- kable(accuracy(prediccion_naive_venta, test_venta)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
saveRDS(tabla_naive_venta, file = "tabla_naive_venta.RDS")
