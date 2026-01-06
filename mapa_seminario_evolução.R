# Pacotes ----

library(sf)

library(tidyverse)

library(rnaturalearth)

# Dados ----

## Importando ----

continentes <- rnaturalearth::ne_countries()

### Visualizando ----

continentes

ggplot() +
  geom_sf(data = continentes) +
  coord_sf(expand = FALSE)

## Tratando ----

continentes_trat <- continentes |> 
  dplyr::filter(name %in% c("South Korea", "Australia"))

continentes_trat

ggplot() +
  geom_sf(data = continentes_trat) 

# Mapa ----

ggplot() +
  geom_sf(data = continentes, color = "black", linewidth = 1) +
  geom_sf(data = continentes_trat, color = "black", fill = "gold", 
          linewidth = 1) +
  coord_sf(expand = FALSE,
           label_graticule = "NSEW") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.grid = element_line(linetype = "dashed", linewidth = 1, 
                                  color = "gray40"),
        legend.text = ggtext::element_markdown(color = "black", size = 12),
        legend.position = "bottom",
        plot.caption = element_text(color = "black", size = 12, hjust = 0.5))

ggsave(filename = "mapa_especies_bd.png", height = 10, width = 12)
