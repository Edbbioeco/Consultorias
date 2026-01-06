# Pacotes ----

library(readxl) 

library(tidyverse)

library(parzer)

library(sf)

library(geobr)

library(cowplot)

library()

# Dados ----

## Registro de coordenadas ----

### Importando ----

coords <- readxl::read_xlsx("Pontos de Coleta - T. chapeuensis.xlsx")

### Visualizando ----

coords

coords |> dplyr::glimpse()

### Tratando ----

coords_sf <- coords |> 
  dplyr::mutate(Longitude = Longitude |> 
                  parzer::parse_lon(),
                Latitude = Latitude |> 
                  parzer::parse_lat()) |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) 

coords_sf

ggplot() +
  geom_sf(data = coords_sf)

## Planalto da Borborema ----

### Importando ----

planalto <- sf::st_read("suborborema.shp")

### Visualizando ----

planalto

ggplot() +
  geom_sf(data = planalto, color = "orangered", fill = "orangered") +
  geom_sf(data = coords_sf)

## Shapefile do Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black", fill = "white") +
  geom_sf(data = planalto, color = "orangered", fill = "orangered") +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = coords_sf)

## Shapefile do Nordeste ----

### Importando ----

ne <- br |> 
  dplyr::filter(name_region == "Nordeste")

### Visualizando ----

ne

ggplot() +
  geom_sf(data = br, color = "black", fill = "white") +
  geom_sf(data = ne, color = "black", fill = "gold") +
  geom_sf(data = planalto, color = "orangered", fill = "orangered") +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = coords_sf)

# Mapa ----

## Setando tema ----

theme_set(theme_bw() +
            theme(axis.text = element_text(color = "black", size = 15),
                  axis.title = element_text(color = "black", size = 15),
                  legend.text = element_text(color = "black", size = 15),
                  legend.title = element_text(color = "black", size = 15),
                  legend.position = "bottom",
                  legend.background = element_blank(),
                  panel.border = element_rect(color = "black", linewidth = 1),
                  panel.grid = element_line(linewidth = 0.75, color = "gray60",
                                            linetype = "dashed")))

## Mapa principal ----

mapa_principal <- ggplot() +
  geom_sf(data = br, color = "black",
          aes(fill = "Brasil"),
          linewidth = 1) +
  geom_sf(data = ne, color = "black", 
          aes(fill = "Nordeste"),
          linewidth = 1) +
  geom_sf(data = planalto, 
          aes(fill = "Planalto da Borborema"), 
          color = "orange4",
          linewidth = 1) +
  geom_sf(data = br, color = "black", fill = "transparent",
          linewidth = 1) +
  geom_sf(data = coords_sf, aes(fill = `Tipo de ponto`),
          color = "black",
          shape = 21,
          size = 5,
          stroke = 1) +
  coord_sf(label_graticule = "NSWE",
           xlim = c(-38.3, -35.1),
           ylim = c(-9.6, -5.8)) +
  labs(fill = NULL,
       color = NULL) +
  scale_fill_manual(breaks = c("Brasil",
                               "Nordeste",
                               "Planalto da Borborema",
                               "Com amostragem",
                               "Sem amostragem"),
                    values = c("white",
                               "lightgoldenrod",
                               "orange4",
                               "orange",
                               "royalblue"))

mapa_principal

## Inset map ----

inset_map <- ggplot() +
  geom_sf(data = br, color = "black",
          aes(fill = "Brasil"),
          linewidth = 0.5) +
  geom_sf(data = ne, color = "black", 
          aes(fill = "Nordeste"),
          linewidth = 0.5) +
  geom_sf(data = planalto, 
          aes(fill = "Planalto da Borborema"), 
          color = "orange4",
          linewidth = 0.51) +
  geom_sf(data = br, color = "black", fill = "transparent",
          linewidth = 0.5) +
  labs(fill = NULL,
       color = NULL) +
  scale_fill_manual(breaks = c("Brasil",
                               "Nordeste",
                               "Planalto da Borborema",
                               "Com amostragem",
                               "Sem amostragem"),
                    values = c("white",
                               "lightgoldenrod",
                               "orange4",
                               "orange",
                               "royalblue")) +
  geom_rect(aes(xmin = -38.3, 
                xmax = -35.1,
                ymin = -9.6, 
                ymax = -5.8),
            color = "darkred",
            fill = "red",
            linewidth = 1,
            alpha = 0.5) +
  theme_void() +
  theme(legend.position = "none")

inset_map

## Mapa completo ----

cowplot::ggdraw(mapa_principal) +
  cowplot::draw_plot(inset_map,
                     x = 0.175,
                     y = 0.625,
                     height = 0.35,
                     width = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_cecilia.png", height = 10, width = 12)
