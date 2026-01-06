# Pacotes ----

library(tidyverse)

library(sf) 

library(geobr)

library(scales)

library(cowplot)

# Dados ----

## Coordenadas ----

### Importando ----

coords <- readr::read_csv("coordenadas_qgis - coordenadas_qgis.csv")

### Visualizando ----

coords 

### Tratando ----

coords_shp <- coords |> 
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674)

coords_shp

ggplot() +
  geom_sf(data = coords_shp, shape = 21, fill = "gold")

## Batimetria ----

### Importando ----

unzip("batimetria_brasil.zip")

bat <- list.files(path = "batimetria_brasil/Batimetria",
           pattern = ".shp")[c(3)]

bat <- sf::st_read(paste0("batimetria_brasil/Batimetria/", bat))

### Visualizando ----

bat

ggplot() +
  geom_sf(data = coords_shp, shape = 21, fill = "gold") +
  geom_sf(dat = bat)

## Mapa do Brasil ----

### Importando ----

estados <- geobr::read_state(year = 2019)

### Visualizando -----

estados

ggplot() +
  geom_sf(data = coords_shp, , shape = 21, fill = "gold") +
  geom_sf(data = estados) +
  geom_sf(dat = bat)

# Área da batimentria ----

## mìnimo polígono convexo ----

convex <- tibble::tibble(lon = c(-41, -37, -41, -37),
                         lat = c(-19, -15, -15, -19)) |> 
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = bat |> sf::st_crs()) |> 
  sf::st_union() |> 
  sf::st_convex_hull()

convex

ggplot() +
  geom_sf(data = convex)

## Recortando ----

bat_trat <- bat |> 
  sf::st_make_valid() |>
  sf::st_intersection(convex) |> 
  dplyr::arrange(PROFUNDIDA)

bat_trat$PROFUNDIDA

vetor <- c(
  "0 a -20 m",      
  "-20 a -50 m",    
  "-50 a -75 m",    
  "-75 a -100 m",   
  "-100 a -200 m",  
  "-200 a -300 m",  
  "-300 a -400 m",  
  "-400 a -500 m",  
  "-500 a -600 m",  
  "-600 a -700 m",  
  "-700 a -800 m",  
  "-800 a -900 m",  
  "-900 a -1000 m", 
  "-1000 a -1100 m",
  "-1100 a -1200 m",
  "-1200 a -1300 m",
  "-1300 a -1400 m",
  "-1400 a -1500 m",
  "-1500 a -1600 m",
  "-1600 a -1700 m",
  "-1700 a -1800 m",
  "-1800 a -1900 m",
  "-1900 a -2000 m",
  "-2000 a -2250 m",
  "-2250 a -2500 m",
  "-2500 a -2750 m",
  "-2750 a -3000 m",
  "-3000 a -3250 m",
  "-3250 a -3500 m",
  "-3500 a -3750 m",
  "-3750 a -4000 m",
  "-4000 a -4250 m"
)

bat_trat <- bat_trat |> 
  dplyr::mutate(PROFUNDIDA = PROFUNDIDA |> forcats::fct_relevel(vetor))

bat_trat

ggplot() +
  geom_sf(data = bat_trat)

# Mapa ----

## Inset map ----

inset_map <- ggplot() +
  geom_sf(data = estados, color = "black") +
  geom_rect(aes(xmin = -40.1, 
                xmax = -38.28,
                ymin = -18.58,
                ymax = -16.97),
            color = "darkred",
            fill = "red",
            alpha = 0.3) +
  theme_void()

inset_map



## Mapa principal ----

paleta <- scales::seq_gradient_pal(low = "#56B1F7", 
                         high = "#132B43")(seq(0, 
                                               1, 
                                               length.out = length(vetor)))

paleta

mapa_principal <- ggplot() +
  geom_sf(dat = bat_trat, 
          linewidth = 0.5, 
          color = "black", 
          aes(fill = PROFUNDIDA)) +
  geom_sf(data = coords_shp, size = 3, shape = 21, fill = "gold") +
  geom_sf(data = estados, color = "black") +
  coord_sf(label_graticule = "NSWE",
           xlim = c(-40.1, -38.28),
           ylim = c(-18.58, -16.97)) +
  scale_fill_manual(values = paleta) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.ontop = TRUE,
        panel.grid = element_line(color = "black", linetype = "dashed"),
        legend.position = "none")

mapa_principal

## Unindo os mapas ----

mapa_principal |> 
  cowplot::ggdraw() +
  cowplot::draw_plot(inset_map, 
                     x = 0.1,
                     y = 0.5,
                     width = 0.35,
                     height = 0.35)

ggsave(filename = "mapa_thiago.png", height = 10, width = 12)
