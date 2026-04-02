# Instalar pacotes ----

if(require(c("tidyverse",
             "rnaturalearth",
             "sf",
             "ggview",
             "terra",
             "tidyterra",
             "adehabitatHR")) == FALSE){ 
  
  install.packages(c("tidyverse",
                     "rnaturalearth",
                     "sf",
                     "ggview",
                     "terra",
                     "tidyterra",
                     "adehabitatHR"))
  
}

# Carregar pacotes ----

library(tidyverse)

library(rnaturalearth)

library(sf)

library(ggview)

library(terra)

library(tidyterra)

library(adehabitatR)

# Dados ----

## Registros de cocos ----

### Importar ----

registros <- readr::read_csv("cocos_nucifera_occurrences_clean_americas_only.csv")

### Visualizar ----

registros

registros |> dplyr::glimpse()

### Tratar ----

registros <- registros |> 
  dplyr::mutate(scientific_name_std = "Cocos nucifera")

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
  geom_sf(data = america_sul, color = "black", 
          aes(fill = "Américas"),
          linewidth = 0.75) +
  geom_sf(data = estados_brasil, color = "black", 
          aes(fill = "Brasil"),
          linewidth = 0.75) +
  geom_sf(data = registros_sf_americas, color = "orangered", fill = "orangered", 
          shape = 21, alpha = 0.5) +
  geom_sf(data = america_sul, color = "black", fill = "transparent",
          linewidth = 0.75) +
  geom_sf(data = estados_brasil, color = "black", fill = "transparent",
          linewidth = 0.75) +
  coord_sf(xlim = c(-140, -30), ylim = c(-50, 60)) +
  scale_fill_manual(values = c("Américas" = "gray90", 
                               "Brasil" = "lightgoldenrod")) +
  labs(fill = NULL) +
  theme_minimal() +
  theme(text = element_text(color = "black", size = 25),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_registros_cocos.png", height = 10, width = 12)

# Mapa de densidade de kernel -----

## Densidade de Kernel ----

### Calculando -----

registros_kde <- registros_sf_americas |> 
  sf::st_transform(crs = 32725) |> 
  dplyr::select(scientific_name_std) |> 
  sf::as_Spatial() |>  
  adehabitatHR::kernelUD(h = "href", grid = 1000)

### Conversão em raster ----

#### Conversão ----

registros_kde_raster <- registros_kde |> 
  adehabitatHR::estUDm2spixdf() |> 
  terra::rast() |> 
  terra::project("EPSG:4326") |> 
  terra::crop(america_sul |> 
                dplyr::filter(subregion |> stringr::str_detect("America"))) |> 
  terra::mask(america_sul |> 
                dplyr::filter(subregion |> stringr::str_detect("America")))

#### Visualização ----

registros_kde_raster

ggplot() +
  tidyterra::geom_spatraster(data = registros_kde_raster) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent") +
  theme_minimal() 

### Conversão em shapefile ----

#### Conversão ----

converter_kd_sf <- function(porcentagem){
  
  registros_kde_contorno <- registros_kde |> 
    adehabitatHR::getverticeshr(percent = porcentagem) |> 
    sf::st_as_sf() |> 
    sf::st_transform(crs = 4326) |> 
    dplyr::mutate(Porcentagem = paste0(porcentagem, "%"))
  
  assign(paste0("sf_kde_", porcentagem), 
         registros_kde_contorno,
         envir = globalenv())
  
}

porcentagem <- seq(50, 90, by = 10)

porcentagem

purrr::map(porcentagem, converter_kd_sf)

#### Unindo os shapefiles ----

sf_kde <- ls(pattern = "sf_kde_") |> 
  mget(envir = globalenv()) |> 
  dplyr::bind_rows() |> 
  dplyr::arrange(Porcentagem |> dplyr::desc()) |> 
  sf::st_intersection(america_sul |> 
                        dplyr::filter(subregion |> stringr::str_detect("America")) |> 
                        sf::st_union() |> 
                        sf::st_boundary() |> 
                        sf::st_cast("POLYGON"))

#### Visualização ----

sf_kde

ggplot() +
  geom_sf(data = sf_kde, aes(fill = Porcentagem), color = "transparent") +
  scale_fill_viridis_d(option = "turbo") +
  geom_sf(data = america_sul, color = "black", fill = "transparent",
          linewidth = 0.75) +
  coord_sf(xlim = c(-140, -30), ylim = c(-50, 60)) +
  theme_minimal()
