# Instalar pacotes ----

if(require(c("tidyverse",
             "rnaturalearth",
             "adehabitatHR")) == FALSE){ 
  
  install.packages(c("tidyverse",
                     "rnaturalearth",
                     "adehabitatHR"))
  
}

# Carregar pacotes ----

library(tidyverse)

library(rnaturalearth)

library(adehabitatR)

# Dados ----

## Registros de cocos ----

### Importar ----

registros <- readr::read_csv("cocos_nucifera_occurrences_clean_americas_only.csv")

### Visualizar ----

registros

registros |> dplyr::glimpse()

## Shapefile da América do Sul ----

### Baixar ----

america_sul <- rnaturalearth::ne_countries()

### Visualizar ----

america_sul

ggplot() +
  geom_sf(data = america_sul, color = "black")

## Shapefile dos estados do Brasil ----

### Baixar ----

estados_brasil <- rnaturalearth::ne_states(country = "Brazil")

### Visualizar ----

estados_brasil

ggplot() +
  geom_sf(data = estados_brasil, color = "black")

# Tratando coordenadas ----

## Criar o shapefile das coordenadas ----

registros_sf <- registros |> 
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = estados_brasil |> sf::st_crs())

registros_sf

ggplot() +
  geom_sf(data = america_sul, color = "black") +
  geom_sf(data = registros_sf, color = "orangered", fill = "orangered", 
          shape = 21, alpha = 0.1)

## Removendo pontos fora das Américas ----

registros_sf_americas <- registros_sf |> 
  sf::st_intersection(america_sul |> 
                        dplyr::filter(subregion |> stringr::str_detect("America")))

registros_sf_americas

ggplot() +
  geom_sf(data = america_sul, color = "black") +
  geom_sf(data = registros_sf_americas, color = "orangered", fill = "orangered", 
          shape = 21, alpha = 0.1)

# Mapa de registros de cocos ----

ggplot() +
  geom_sf(data = america_sul, color = "black", fill = "white") +
  geom_sf(data = registros_sf_americas, color = "orangered", fill = "orangered", 
          shape = 21, alpha = 0.1) +
  geom_sf(data = america_sul, color = "black", fill = "transparent") +
  geom_sf(data = estados_brasil, color = "black", fill = "transparent") +
  coord_sf(xlim = c(-140, -30), ylim = c(-50, 60)) +
  theme_minimal()
