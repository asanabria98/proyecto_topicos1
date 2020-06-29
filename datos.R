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
