# Pacotes ----

library(readxl)

library(tidyverse)

library(performance)

library(rsq)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados_culicideos.xlsx")

## Visualizando ----

dados

## Tratando ----

dados_trat <- dados |> 
  dplyr::mutate(Bromélia = Bromélia |> as.character()) |> 
  dplyr::summarise(Abundancia = Abundancia |> sum(),
                   `Cobertura dossel` = `Cobertura dossel` |> mean(),
                   .by = Bromélia)

dados_trat

# GLM -----

## Criando o modelo ----

modelo <- glm(Abundancia ~ `Cobertura dossel`,
              data = dados_trat,
              family = poisson(link = "log"))

## Avaliando o modelo ----

modelo |> 
  performance::check_model()

modelo |> 
  summary()

modelo |> 
  rsq::rsq(adj = FALSE)

## Gráfico ----

dados_trat |> 
  ggplot(aes(`Cobertura dossel`, Abundancia)) +
  geom_point(size = 5, shape = 21, color = "black", fill = "gold") +
  geom_smooth(method = "lm",
              color = "blue",
              se = FALSE) +
  geom_text(aes(x = 27, 
                y = 20,
                label = "β = 0.016 ± 0.006, z = 2.88, p = 0.004, R² = 0.035"),
            color = "black",
            size = 7.5) +
  labs(x = "% de cobertura de dossel") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15))

ggsave(filename = "grafico_rick.png", height = 10, width = 12)

