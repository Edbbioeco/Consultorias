# Pacotes ----

library(tidyverse)

library(performance)

library(ggbeeswarm)

library(reshape2)

library(viridis)

library(factoextra) 

library(FactoMineR)

# Dados ----

## Carregando ----

data("iris")

## Visualizando ----

iris

iris |> dplyr::glimpse()

# Modelo linear ----

## Criando o modelo ----

modelo_iris <- lm(Petal.Length ~ Sepal.Length, 
                  data = iris)

## Avaliando os pressupostos do modelo ----

### pressuposto 1: independência de unidades amostrais ----

### Pressupostos 2: não ocorrência de outliers ----

modelo_iris |> performance::check_outliers()

### Pressuposto 3: distribuição gaussiana (normal) dos resíduos ----

modelo_iris |> performance::check_normality()

### Pressuposto 4: variância homogênea dos resíduos aos valores preditos ----

modelo_iris |> performance::check_heteroscedasticity()

### Visualizando todos os pressupostos ----

modelo_iris |> performance::check_model(check = c("normality", 
                                                  "homogeneity", 
                                                  "outliers",
                                                  "vif",
                                                  "qq"))

## Estatísticas do modelo ----

modelo_iris |> summary()

## Checando o T-Crítico ----

qt(p = 0.05, df = 148, lower.tail = FALSE)

## Checando o F-Crítico ----

qf(p = 0.05, df1 = 1, df2 = 148, lower.tail = FALSE)

# Setando temas -----

theme_set(theme_bw() +
            theme(axis.text = element_text(color = "black", size = 15),
                  axis.title = element_text(color = "black", size = 15),
                  legend.text = element_text(color = "black", size = 15),
                  legend.title = element_text(color = "black", size = 15),
                  panel.border = element_rect(color = "black", linewidth = 1),
                  panel.grid = element_line(linewidth = 0.75, color = "gray60",
                                            linetype = "dashed")))

# Gráfico de dispersão -----

iris |> 
  ggplot(aes(Sepal.Length, Petal.Length, fill = Species, color = Species)) +
  geom_point(shape = 21,
             color = "black",
             size = 5,
             stroke = 1) + 
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("springgreen", "orange", "mediumpurple")) +
  scale_color_manual(values = c("darkgreen", "darkorange4", "purple4"))

ggsave(filename = "dispersão_angela.png", height = 10, width = 12)

# Gráfico de barras ----

## Calculando os valores ----

iris_estatísticas <- iris |> 
  dplyr::slice_max(Sepal.Length, n = 1,
                   by = Species)

iris_estatísticas

## Gráfico ----

iris_estatísticas |> 
  ggplot(aes(Species, Sepal.Length, fill = Species)) +
  geom_col(color = "black", width = 0.5) +
  scale_fill_manual(values = c("springgreen", "orange", "mediumpurple"))

ggsave(filename = "barras_angela.png", height = 10, width = 12)

# Boxplot ----

iris |> 
  ggplot(aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot(color = "black",
               outlier.shape = 21,
               outlier.size = 5) +
  scale_fill_manual(values = c("springgreen", "orange", "mediumpurple"))

ggsave(filename = "boxplot_angela.png", height = 10, width = 12)

# Gráfico de dispersão por grupos ----

## jitter ----

iris |> 
  ggplot(aes(Species, Sepal.Length, fill = Species)) +
  geom_jitter(color = "black",
              stroke = 1,
              shape = 21,
              size = 5,
              width = 0.25) +
  scale_fill_manual(values = c("springgreen", "orange", "mediumpurple"))

ggsave(filename = "dispersao_grupos_jitter_angela.png", height = 10, width = 12)

## pacote ggbeeswarm ----

iris |> 
  ggplot(aes(Species, Sepal.Length, fill = Species)) +
  ggbeeswarm::geom_quasirandom(color = "black",
                               stroke = 1,
                               shape = 21,
                               size = 5) +
  scale_fill_manual(values = c("springgreen", "orange", "mediumpurple"))

ggsave(filename = "dispersao_grupos_ggbeeswarm_angela.png", height = 10, width = 12)

# Gráfico de calor ----

## Calculando a coorelação entre as variáveis ----

cor_iris <- iris |> 
  dplyr::select(dplyr::where(is.numeric)) |> 
  cor(method = "spearman")

cor_iris

## Criando um dataframe para o ggplot ----

cor_iris[upper.tri(cor_iris)] <- NA

cor_iris

cor_iris_df <- cor_iris |> 
  reshape2::melt() |> 
  tidyr::drop_na() |> 
  dplyr::mutate(value = value |> round(2)) |> 
  dplyr::rename("Spearman Correlation Index" = value)

cor_iris_df

## Gráfico ----

### Opção 1 ----

cor_iris_df |> 
  ggplot(aes(Var1, Var2, fill = `Spearman Correlation Index`, 
             label = `Spearman Correlation Index`)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(color = "black", size = 5) +
  coord_equal() +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Opção 2 ----

cor_iris_df |> 
  ggplot(aes(Var1, Var2, fill = `Spearman Correlation Index`, 
             label = `Spearman Correlation Index`)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(color = "black", size = 5) +
  coord_equal() +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       limits = c(-1, 1)) +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_colorbar(title.hjust = 0.5,
                               barheight = 15,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "grafico_calor_angela.png", height = 10, width = 12)

# Biplot de PCA ----

## Calculando PCA ----

### Calculando ---

pca_iris <- iris |> 
  dplyr::select(dplyr::where(is.numeric)) |> 
  vegan::decostand(method = "standardize") |> 
  FactoMineR::PCA()

### Autovalores ----

pca_iris$eig

### Autovetores ----

pca_iris$var

### Scores ----

pca_iris$ind$coord

## Opção 1 ----

pca_iris |> 
  factoextra::fviz_pca_biplot(habillage = iris$Species,
                              col.var = "black",
                              addEllipses = TRUE) +
  scale_fill_manual(values = c("springgreen", "orange", "mediumpurple")) +
  scale_color_manual(values = c("springgreen", "orange", "mediumpurple")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1))


ggsave(filename = "pca_angela.png", height = 10, width = 12)

## Opção 2 ----

### Criando um dataframe ----

pca_iris_df <- pca_iris$ind$coord |> 
  as.data.frame() |> 
  dplyr::mutate(Species = iris$Species) |> 
  dplyr::rename("PC1 (73%)" = Dim.1,
                "PC2 (22.9%)" = Dim.2)

pca_iris_df

### Criando os dados de centróides ----

pca_iris_centroides <- pca_iris_df |> 
  dplyr::summarise(c_x = `PC1 (73%)` |> mean(),
                   c_y = `PC2 (22.9%)` |> mean(),
                   .by = Species)

pca_iris_centroides

pca_iris_df <- pca_iris_df |> 
  dplyr::left_join(pca_iris_centroides,
                   by = "Species")

pca_iris_df

### Gráfico ----

pca_iris_df |> 
  ggplot(aes(`PC1 (73%)`, `PC2 (22.9%)`, fill = Species, color = Species)) +
  geom_segment(aes(x = `PC1 (73%)`, xend = c_x,
                   y = `PC2 (22.9%)`, yend = c_y),
               linewidth = 1,
               linetype = "dashed") +
  geom_point(shape = 21,
             stroke = 1,
             color = "black",
             size = 2.5)  +
  scale_fill_manual(values = c("springgreen", "orange", "mediumpurple")) +
  scale_color_manual(values = c("darkgreen", "darkorange4", "purple4"))

ggsave(filename = "pca_manual_angela.png", height = 10, width = 12)
