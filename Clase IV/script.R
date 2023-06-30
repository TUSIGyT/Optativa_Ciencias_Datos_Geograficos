#rm(list = ls())
# packages
library(sf) # vector
library(tmap) # thematic maps
library(geobr)
library(tidyverse)

# importando datos de Airbnb 
# datos <- read_csv("./Datos/AirbnbRJRentals.csv")
# datos %>% filter(price < 5000) %>% write_csv("./Datos/AirbnbRJRentals_modificado.csv")
datos <- read_csv("./Datos/AirbnbRJRentals_modificado.csv")

# transformando en geografico
airbnb <- st_as_sf(datos, coords = c("longitude", "latitude"))

tm_shape(airbnb) +
  tm_dots() + 
  tm_grid()

tmap_mode("view")
tm_shape(airbnb) +
  tm_dots() + 
  tm_grid()

# importando datos del Municipio de RJ
tmap_mode("plot")
rj <- read_municipality(code_mun='RJ')
tm_shape(rj, bbox = airbnb)+
  tm_polygons() +
  tm_shape(airbnb) +
  tm_dots() + 
  tm_grid(lines = FALSE) +
  tm_style("natural")


# pero necesito solamente el municipio de Rio.
# Vamos a filtrar:
# explicar el pipe, explicar la diferencia entre '=' y '=='
rj <- rj %>% filter(name_muni == 'Rio De Janeiro')
tm_shape(rj)+
  tm_polygons() +
  tm_shape(airbnb) + 
  tm_dots()

# ya se puede identificar un patrón en la distribución de los alquileres, no ?

# empecemos el viaje:
tmap_mode("view")
tm_shape(rj)+
  tm_polygons() +
  tm_shape(airbnb) + 
  tm_dots()
  
# Analizando los datos 
# análsis exploratorios

# que datos existen en la planilla (data frame)?
colnames(airbnb)
# resumo de los cinco numeros
summary(airbnb)

# que info existen en la columns "room_type"?
unique(airbnb$room_type)

# Como se distribuyen los datos de precio?
library(ggplot2)
ggplot(airbnb) + geom_histogram(aes(x=price))
# Cual es el valor medio?
mean(airbnb$price)

# Cual es el valor medio por barrio?
airbnb %>% group_by(neighbourhood) %>% summarise(media = mean(price)) %>% arrange(desc(media))

# Donde podríamos quedarnos con el valor promedio
airbnb_814 <- airbnb %>% filter(price <= 814)
tm_shape(rj)+
  tm_polygons() +
  tm_shape(airbnb_814) + 
  tm_dots()

# como tenemos la info del barrio, podríamos investigar los barrios que más nos interesa:
airbnb_814 <- airbnb_814 %>% filter(neighbourhood %in% c("Copacabana", "Barra da Tijuca"))
mean(airbnb_814$price)
ggplot(airbnb_814) + geom_histogram(aes(x=price))

# variación de valores con boxplot
ggplot(airbnb_814) + geom_boxplot(aes(x=price))

# como el valor medio cambia en función del barrio y tipo de alquiler?
ggplot(airbnb_814) + geom_boxplot(aes(x=price, fill=neighbourhood)) + facet_grid(.~room_type)

# bien, pero los valores deben cambiar acorde a la localización
tm_shape(airbnb_814) + 
  tm_dots(size='price')

# bien, pero los valores deben cambiar también acorde al tipo de alquiler
tm_shape(airbnb_814) + 
  tm_dots(size='price', col = 'room_type')


# en qué barrio hay mas ofertas de alquiler?
airbnb %>% group_by(neighbourhood) %>% summarise(n=n()) %>% arrange(desc(n))


# En que barrio hay más oferta de habitaciones compartidas?
airbnb %>% filter(room_type == 'Shared room') %>% group_by(neighbourhood) %>% summarise(n=n()) %>% arrange(desc(n))

# En qué barrios hay más ofertas de habitaciones de hotel?
airbnb %>% filter(room_type == 'Hotel room') %>% group_by(neighbourhood) %>% summarise(n=n()) %>% arrange(desc(n))

# En qué barrios hay más ofertas de habitaciones privada?
airbnb %>% filter(room_type == 'Private room') %>% group_by(neighbourhood) %>% summarise(n=n()) %>% arrange(desc(n))

# qué tipo de habitación tiene más revisiones por mes?
airbnb %>% group_by(room_type) %>% summarise(media=mean(reviews_per_month, na.rm = TRUE)) %>% arrange(desc(media))
