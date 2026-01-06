# Pacotes ----

library(readxl)

library(tidyverse)

library(sjPlot)

# Dados ----

## Importando ----

i_s <- readxl::read_xlsx("./idle_slayer.xlsx")

## Visualizando ----

i_s

# Análise ----

## Criando o modelo ----

modelo <- lm(`Altura relativa` ~ Soma, data = i_s)

## Pressupostos do modelo ----

modelo |> sjPlot::plot_model(type = "diag") |> 
  sjPlot::plot_grid()

## Avaliando o modelo ----

modelo |> 
  summary()

## Gráfico ----

i_s |> 
  ggplot(aes(`Altura relativa`)) +
  geom_histogram(color = "black", binwidth = 25) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.grid = element_line(linetype = "dashed", linewidth = 1))

i_s |> 
  ggplot(aes(Soma, `Altura relativa`)) +
  geom_point(size = 2.5, 
             shape  = 21, 
             color = "black", 
             fill = "gold") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        panel.border = element_rect(color = "black", linewidth = 1),
        panel.grid = element_line(linetype = "dashed", linewidth = 1))
