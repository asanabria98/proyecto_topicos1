library(dplyr)
library(echarts4r)

datos <- as.data.frame(read.csv("07_05_CSV_GENERAL.csv",header=TRUE,sep=";"))

datos <- datos%>%
  select(FECHA,positivos,nue_posi,RECUPERADOS,NUE_RECUP,fallecidos,nue_falleci)
         
datos$FECHA = as.Date(datos$FECHA,format="%d/%m/%Y")

nombres <- c("Fecha","Positivos","Nuevos_Positivos","Recuperados","Nuevos_Recuperados",
             "Fallecidos","Nuevos_Fallecidos")
names(datos) <- nombres

graf_covid_acum <- datos %>% 
  e_charts(x = Fecha) %>% # initialise and set x
  e_line(serie = Positivos, smooth=TRUE) %>%  # add a line
  e_line(serie = Recuperados, smooth=TRUE) %>% 
  e_line(serie = Fallecidos, smooth=TRUE) %>% 
  e_axis_labels(x="Día",y="Cantidad pacientes") %>% 
  e_theme("infographic") %>% 
  e_legend(right = "50") %>% 
  #e_mark_point(data = max) %>% marcar el d?a con m?s casos
  e_datazoom() %>% #barra para seleccionar el segmento de tiempo que se desea ver
  e_tooltip(trigger = "axis") %>% 
  e_mark_point("Positivos", data = list(type = "max")) %>% #marcar el d?a con m?s casos
  e_mark_point("Recuperados", data = list(type = "max")) %>% 
  e_mark_point("Fallecidos", data = list(type = "max"))
   
saveRDS(graf_covid_acum, file = "graf_covid_acum.RDS")

graf_covid_diario <- datos %>% 
  e_charts(x = Fecha) %>% # initialise and set x
  e_bar(serie = Nuevos_Positivos, smooth = TRUE, name = "Positivos") %>%  # add a line
  e_bar(serie = Nuevos_Recuperados, smooth = TRUE, name = "Recuperados") %>% 
  e_bar(serie = Nuevos_Fallecidos, smooth = TRUE, name = "Fallecidos") %>% 
  e_axis_labels(x = "Día",y = "Cantidad pacientes") %>% 
  #e_theme("chalk") %>% 
  e_theme("infographic") %>%
  e_legend(right = "50") %>% 
  e_datazoom() %>% #barra para seleccionar el segmento de tiempo que se desea ver
  e_tooltip(trigger = "axis") %>% 
  e_mark_point("Positivos", data = list(type = "max")) %>% #marcar el d?a con m?s casos
  e_mark_point("Recuperados", data = list(type = "max")) %>% 
  e_mark_point("Fallecidos", data = list(type = "max"))

saveRDS(graf_covid_diario, file = "graf_covid_diario.RDS")
  