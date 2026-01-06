# Pacotes ----

library(sf)

library(tidyverse)

library(rnaturalearth)

library(ggtext)

library(ggview)

# Dados ----

## Distribuição ----

### Descrompimir ----

unzip("redlist_species_data_4739f452-703f-499f-80ff-61b215080507.zip")

### Importando ----

dist <- sf::read_sf("data_0.shp")

### Visualizando -----

dist

ggplot() +
  geom_sf(data = dist)

### Tratando ----

dist_trat <- dist |> 
  sf::st_union()

dist_trat

ggplot() +
  geom_sf(data = dist_trat)

## Continentes ----

### Importando ----

continentes <- rnaturalearth::ne_countries()

### Visualizando ----

continentes

ggplot() +
  geom_sf(data = continentes)

### Tratando ----

continentes_trat <- continentes |> 
  dplyr::filter(continent |> stringr::str_detect("America"))

continentes_trat

ggplot() +
  geom_sf(data = continentes_trat) 

# Mapa ----

ggplot() +
  geom_sf(data = continentes_trat, color = "black", linewidth = 1) +
  geom_sf(data = dist_trat, 
          aes(colour = "Distribuição do gênero *Scinax* sp.", 
              fill = "Distribuição do gênero *Scinax* sp."), 
          alpha = 0.3, 
          linewidth = 1) +
  labs(fill = NULL,
       colour = NULL,
       caption = "Fonte: iucnredlist.org") +
  scale_colour_manual(values = c("Distribuição do gênero *Scinax* sp." = "darkgreen")) +
  scale_fill_manual(values = c("Distribuição do gênero *Scinax* sp." = "darkgreen")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.grid = element_line(linetype = "dashed", linewidth = 1, 
                                  color = "gray40"),
        legend.text = ggtext::element_markdown(color = "black", size = 25),
        legend.position = "bottom",
        plot.caption = element_text(color = "black", size = 25, hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_scinax.png", height = 10, width = 12)
