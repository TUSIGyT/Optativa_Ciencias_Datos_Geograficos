library(tidyverse)
library(sf)
library(tmap)
t <- read_sf('./Datos/fogo_cruzado2020.csv')
t <- t %>% mutate(
  ID=as.numeric(id_ocorrencia),
  latitude = latitude_ocorrencia,
  longitude=longitude_ocorrencia)

tiroteos <- read_csv("./Datos/Tiroteos_Rio.csv")
# tiroteos <- st_as_sf(tiroteos, coords='')
t <- left_join(t, tiroteos, by ='ID')
colnames(t)
t <- t %>% select(68:83)
write_csv(t, "./Datos/Tiroteos_Rio.csv")
