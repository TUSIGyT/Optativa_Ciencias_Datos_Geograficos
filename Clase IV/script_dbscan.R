# install.packages("dbscan")
library(sf)
library(tmap)
library(tidyverse)
library(dbscan) #nuevo


datos <- read_csv("./Datos/AirbnbRJRentals_modificado.csv")

# transformando en geografico
airbnb <- st_as_sf(
  datos, 
  coords = c("longitude", "latitude"),
  crs=4326)

airbnb <- st_transform(airbnb, 32723)

copacabana <- airbnb %>% filter(
  neighbourhood %in% c("Copacabana", "Leme"),
  room_type == "Entire home/apt")

summary(copacabana$price)

copacabana <- copacabana %>% filter(price >= 1071)

cluters_espacial_100_10 <- dbscan(
  st_coordinates(copacabana),
  eps=100,
  minPts = 10)

cluters_espacial_100_10

copacabana <- copacabana %>% mutate(
  cluters_espacial_100_10 = # nueva columna
    cluters_espacial_100_10$cluster # valor asignado a la columnas nueva
)
copacabana$cluters_espacial_100_10
tmap_mode("view")
tm_shape(copacabana) +
  tm_dots(col="cluters_espacial_100_10")
