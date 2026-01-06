# Pacotes ----

library(tidyverse)

library(performance)

library(ggview)

# Dados ----

## Criando o dataframe ----

set.seed(123); dados <- tibble::tibble(area = runif(50, 1, 30),
                                       numero = 5 + 0.3 * area + rnorm(50, 
                                                                       mean = 20, 
                                                                       sd = 10))

## Visualizando ----

dados

# Modelos lineares ----

## Criando o modelo ----

modelo <- lm(numero ~ area, data = dados)

## Avaliando o modelo ----

modelo |> performance::check_model(check = c("vif",
                                             "qq",
                                             "homogeneity",
                                             "normality"))

## Vizualizando as estatísticas do modelo ----

modelo |> summary()

# Gráfico ----

dados |> 
  ggplot(aes(area, numero)) +
  geom_point(size = 5, color = "#1b630b") +
  labs(x = "Área (km²)",
       y = "Número de Espécies") +
  geom_smooth(method = "lm", color = "#1b630b", fill = "#c1ff72") +
  ggtext::geom_richtext(aes(x = 20, y = 50,
                            label = "<b>R<sup>2</sup> = 0.14, p < 0.01</b>"),
                        color = "#1b630b",
                        fill = "transparent",
                        label.color = NA,
                        size = 15) +
  theme_classic() +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 30, color = "#1b630b", face = "bold"),
        axis.title = element_text(size = 30, color = "#1b630b", face = "bold"),
        axis.line = element_line(linewidth = 1, color = "#1b630b")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_post_valor_p.png", height = 10, width = 12)

view