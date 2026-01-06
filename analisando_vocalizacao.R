# Pacotes ----

library(tuneR)

library(seewave)

library(tidyverse)

library(patchwork)

# Dados ----

## Importando ----

voc <- tuneR::readWave("vocalizações.wav")

## Visualizando ----

voc

voc |> 
  seewave::ggspectro(f = 512) +
  stat_contour(geom = "polygon", 
               aes(fill = ..level..,
                   alpha = ..level..), 
               bins = 150) +
  scale_fill_viridis_c(option = "inferno",
                       direction = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# Analisando ----

## Espectrotrograma ----

### Espectrotro ----

spectro <- voc |> 
  seewave::spectro(wl = 512,
                   ovl = 99,
                   flim = c(0, 4.75))

### ggplot ----

tibl_spectro <- tibble::tibble(`Time (s)` = rep(spectro$time, 
                                             each = length(spectro$freq)),
                            `Frequency (KHz)` = rep(spectro$freq, 
                                                    length(spectro$time)),
                            Amplitude = spectro$amp |> 
                              as.vector()) |> 
  dplyr::filter(Amplitude > -45) |> 
  dplyr::filter(!(Amplitude < -30 & `Time (s)` < 0.19)) |> 
  dplyr::filter(!(Amplitude < -30 & `Time (s)` > 0.55))

tibl_spectro

sps_df <- tibble::tibble(sps = c("Scinax tropicalia",
                                 "Scinax nebulosus",
                                 "Scinax x-signatus",
                                 "Scinax fuscovarius"),
                         `Frequency (KHz)` = 5,
                         `Time (s)` = c(0.09,
                                        0.25,
                                        0.45,
                                        0.7),
                         Amplitude = 0)

sps_df

gg_spectro <- tibl_spectro |> 
  ggplot(aes(`Time (s)`, `Frequency (KHz)`, z = Amplitude)) +
  stat_contour(geom = "polygon", aes(fill = ..level..),
               bins = 150) +
  scale_fill_viridis_c(option = "inferno",
                       direction = -1) +
  geom_text(data = sps_df,
            aes(`Time (s)`, `Frequency (KHz)`, label = sps),
            fontface = "bold.italic",
            color = "black",
            size = 3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 5.25)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "black", size = 12, 
                                  face = "bold.italic"),
        legend.position = "none",
        plot.margin = margin(0.5, -0.5, 0, 0.5, "cm"))

gg_spectro

## Densidade de frequência ----

### Calculando por espécie ----

guia <- list(c(0, 0.175),
             c(0.175, 0.307),
             c(0.307, 0.529),
             c(0.529, 0.827))

guia

calcular_spec_sps <- function(x, y){
  
  inicial <- guia[[x]][1]
  
  final <- guia[[x]][2]
  
  spec <- voc |> 
    seewave::meanspec(from = inicial,
                      to = final,
                      dB = "C")
  
  spec_tbl <- spec |> 
    tibble::as_tibble() |> 
    dplyr::rename("Frequency (KHz)" = x,
                  "Amplitude density (dB/KHz)" = y) |> 
    dplyr::filter(`Frequency (KHz)` <= 5 &
                    `Amplitude density (dB/KHz)` >= -45) |> 
    dplyr::mutate(Species = paste0("Scinax ", y)) |> 
    tidyr::drop_na()
  
  assign(paste0("spec_tbl_", y),
         spec_tbl,
         envir = globalenv())
  
}

purrr::walk2(1:length(guia),
             c("tropicalia",
               "nebulosus",
               "x-signatus",
               "fuscovarius"),
             calcular_spec_sps)

tbl_m_spec <- ls(pattern = "spec_tbl") |> 
  mget(envir = globalenv()) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(Species = Species |> forcats::fct_relevel(c("Scinax tropicalia",
                                                            "Scinax nebulosus",
                                                            "Scinax x-signatus",
                                                            "Scinax fuscovarius")))

tbl_m_spec

### ggplot ----

gg_m_spec <- tbl_m_spec |> 
  ggplot(aes(`Frequency (KHz)`, `Amplitude density (dB/KHz)`)) +
  geom_line(aes(color = Species),
            linewidth = 1) +
  scale_color_manual(values = c("green4",
                                "orangered",
                                "purple4",
                                "cyan4")) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 5.25)) +
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
        plot.margin = margin(0.5, 0.5, 0, -0.5, "cm"))
  
gg_m_spec

## Oscilograma ----

### Calculando ----

voc |> 
  seewave::oscillo()

tbl_osc <- tibble::tibble(`Time (s)` = seq(0, seewave::duration(voc), 
                                           length.out = length(voc@left)),
                          `Amplitude (KU)` = voc@left) |> 
  dplyr::mutate(Species = dplyr::case_when(
    `Time (s)` >= 0.000 & `Time (s)` < 0.175 ~ "Scinax tropicalia",
    `Time (s)` >= 0.175 & `Time (s)` < 0.307 ~ "Scinax nebulosus",
    `Time (s)` >= 0.307 & `Time (s)` < 0.529 ~ "Scinax x-signatus",
    `Time (s)` >= 0.529 ~ "Scinax fuscovarius"),
  Species = Species |> forcats::fct_relevel(c("Scinax tropicalia",
                                              "Scinax nebulosus",
                                              "Scinax x-signatus",
                                              "Scinax fuscovarius")))

tbl_osc

### ggplot ----

gg_osc <- tbl_osc %>%
  ggplot(aes(`Time (s)`, `Amplitude (KU)`, color = Species)) +
  geom_line()  +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("green4",
                                "orangered",
                                "purple4",
                                "cyan4")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        plot.margin = margin(0, -0.5, 0.5, 0.5, "cm"),
        legend.position = "none")

gg_osc

## legenda ----

l1 <- tbl_m_spec |> 
  ggplot(aes(`Frequency (KHz)`, `Amplitude density (dB/KHz)`),
         ) +
  geom_line(aes(color = Species),
            linewidth = 1) +
  scale_color_manual(values = c("green4",
                                "orangered",
                                "purple4",
                                "cyan4")) +
  scale_x_continuous(limits = c(0, 1)) + 
  scale_y_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(legend.position = c(0.5, 0.5),
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.margin = margin(0, 0, 0, -0.5, "cm"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 12, face = "bold.italic"))

l1

l2 <- tibl_spectro |> 
  ggplot(aes(`Time (s)`, `Frequency (KHz)`, z = Amplitude)) +
  stat_contour(geom = "polygon", aes(fill = ..level..),
               bins = 150) +
  scale_fill_viridis_c(option = "inferno",
                       direction = -1) +
  labs(fill = "Power (dB)") +
  guides(fill = guide_colourbar(title.hjust = 0.5,
                                barheight = 17.5,
                                frame.colour = "black",
                                ticks.colour = "black",
                                frame.linewidth = 1,
                                ticks.linewidth = 1)) +
  theme_bw() +
  theme(panel.ontop = TRUE,
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.5, 0.5),
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.margin = margin(0, 0, 0, -0.5, "cm"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 12, face = "bold"))

l2

legenda <- l2 + l1 + theme(plot.margin = margin(0, 0, 0, -0.5, "cm"))

legenda

# Gráfico final ----

gg_spectro + gg_m_spec + gg_osc + legenda + 
  patchwork::plot_layout(ncol = 2)

ggsave(filename = "vocalizacoes_scinax.png", height = 10, width = 12)
