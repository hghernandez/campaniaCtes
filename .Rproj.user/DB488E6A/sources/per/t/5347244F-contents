library(stringr)
library(osmdata)
library(leaflet)
library(dplyr)

#Cargo el data

data <- read.csv2("form-1__rastrillaje-de-viviendas-para-fa.csv",sep = ",", header = TRUE)


#Arreglo los nombres de las variables

names(data) <- str_remove_all(substring(names(data),1,str_length(names(data))),"X")


#Recorto filas con valores extraños
data <- data[1:1242,]

#Agrego punto a longitudes erroneas
data$long_5_ubicacin_de_la_res <- ifelse(str_detect(data$long_5_ubicacin_de_la_res,fixed("."))== "FALSE",paste(substring(data$long_5_ubicacin_de_la_res,1,3),".",substring(data$long_5_ubicacin_de_la_res,4,str_length(data$long_5_ubicacin_de_la_res)),sep=""),as.numeric(data$long_5_ubicacin_de_la_res))

#Convierto en numeros a la latitud y longitud

data$lat_5_ubicacin_de_la_res <- as.numeric(as.character(data$lat_5_ubicacin_de_la_res))
data$long_5_ubicacin_de_la_res <- as.numeric(as.character(data$long_5_ubicacin_de_la_res))

#Selecciono las latitudes distintas a 0 o na
data <- subset(data,is.na(data$lat_5_ubicacin_de_la_res) != "TRUE" | data$lat_5_ubicacin_de_la_res != 0)

#Selecciono las longitudes distintas a 0 o na

data <- subset(data,is.na(data$long_5_ubicacin_de_la_res) != "TRUE" | data$long_5_ubicacin_de_la_res != 0)

#Cargo el mapa de Corrientes de Open Street Map
mapCtes <- getbb("CTES",  format_out = "sf_polygon") #Busque las abrevituaras de las provincia en openstreetmap en google


#Armo el mapa

#Siemrpe longitud primero

mapCorrientes <- leaflet(mapCtes) %>%
  addTiles() %>% 
  addPolygons() %>%
  addCircles(data[data$`8_Ingreso_a_la_resid`== 'Si',c("long_5_ubicacin_de_la_res")],
             data[data$`8_Ingreso_a_la_resid`== 'Si',c("lat_5_ubicacin_de_la_res")],
             color="red") %>%
  
  addCircles(data[data$`8_Ingreso_a_la_resid`== 'No',c("long_5_ubicacin_de_la_res")],
             data[data$`8_Ingreso_a_la_resid`== 'No',c("lat_5_ubicacin_de_la_res")],
             color="blue")


library(htmlwidgets)

saveWidget(mapCorrientes, "campaniaCtes.html", selfcontained = TRUE)

saveWidget(mapCorrientes, "C:/Users/Gabriela/Desktop/campaniaCtes.html", selfcontained = TRUE)

