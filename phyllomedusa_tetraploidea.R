# Pacotes ----

library(tuneR)

library(seewave)

library(tidyverse)

library(patchwork)

# Dados ----

## Importando ----

filo <- tuneR::readWave("phyllomedusa_tetraploidea.wav")

## Visualizando ----

filo

# Analisando ----

## Espectrotrograma ----

### Espectrotro ----

spectro <- filo |> 
  seewave::spectro(wl = 512,
                   ovl = 99,
                   flim = c(0, 13), 
                   tlim = c(0, 13.5))

### ggplot ----

tibl_spectro <- tibble::tibble(`Time (s)` = rep(spectro$time, 
                                                each = length(spectro$freq)),
                               `Frequency (KHz)` = rep(spectro$freq, 
                                                       length(spectro$time)),
                               Amplitude = spectro$amp |> 
                                 as.vector()) |> 
  dplyr::filter(Amplitude > -50) |> 
  dplyr::mutate(Amplitude = dplyr::case_when(`Time (s)` |> 
                                               dplyr::between(0, 1) ~ 0,
                                             `Time (s)` |> 
                                               dplyr::between(10, 12) ~ 0,
                                             .default = Amplitude))

tibl_spectro 

gg_spectro <- tibl_spectro |> 
  ggplot(aes(`Time (s)`, `Frequency (KHz)`, z = Amplitude)) +
  stat_contour(geom = "polygon", aes(fill = ..level..),
               bins = 150) +
  scale_fill_viridis_c(option = "inferno") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 13.5)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 13)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "black", size = 12, 
                                  face = "bold.italic"),
        legend.position = "none",
        plot.margin = margin(0.5, -0.5, 0, 0.5, "cm"),
        panel.grid = element_line(linetype = "dashed"),
        panel.background = element_rect(fill = "black"))

gg_spectro

## Densidade de frequência ----

### Analisando ----

spec <- filo |> 
  seewave::meanspec(from = 0,
                    to = 13.5,
                    dB = "C")

spec

### ggplot ----

spec_df <- spec |> 
  tibble::as_tibble() |> 
  dplyr::rename("Frequency (KHz)" = x,
                "Amplitude density (dB/KHz)" = y) |> 
  dplyr::filter(`Frequency (KHz)` <= 13 &
                  `Amplitude density (dB/KHz)` >= -45)

spec_df

gg_m_spec <- spec_df |> 
  ggplot(aes(`Frequency (KHz)`, `Amplitude density (dB/KHz)`)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 13)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-45, 1),
                     position = "right") +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.y = element_blank(),
        axis.title = element_text(color = "black", size = 12),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(color = "black", size = 12, 
                                  face = "bold.italic"),
        legend.position = "none",
        panel.grid = element_line(linetype = "dashed"),
        plot.margin = margin(0.5, 0.5, 0, -0.5, "cm"))

gg_m_spec

## Oscilograma ----

### Calculando ----

filo |> 
  seewave::oscillo()

tbl_osc <- tibble::tibble(`Time (s)` = seq(0, seewave::duration(filo), 
                                           length.out = length(filo@left)),
                          `Amplitude (KU)` = filo@left) |> 
  dplyr::filter(`Time (s)` |> dplyr::between(0, 13.5)) |> 
  dplyr::mutate(`Amplitude (KU)` = dplyr::case_when(`Time (s)` |> 
                                                      dplyr::between(0, 1) ~ 0,
                                                    `Time (s)` |> 
                                                      dplyr::between(10, 12) ~ 0,
                                                    .default = `Amplitude (KU)`))

tbl_osc

### ggplot ----

gg_osc <- tbl_osc %>%
  ggplot(aes(`Time (s)`, `Amplitude (KU)`)) +
  geom_line()  +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        plot.margin = margin(0, -0.5, 0.5, 0.5, "cm"),
        legend.position = "none",
        panel.grid = element_line(linetype = "dashed"))

gg_osc

# Gráfico final ----

gg_spectro + gg_m_spec + gg_osc + 
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(filename = "vocalizacoes_phyllomedusa.png", height = 10, width = 12)
