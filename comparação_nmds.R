# Pacotes ----

library(ecodados)

library(tidyverse)

library(vegan)

# Dados ----

## Importando ----

com <- ecodados::composicao_aves_filogenetica

## Visualizando ----

com

# Análise ----

## nMDS sem matriz inicial ----

### nMDS ----

nmds <- com |> 
  vegan::metaMDS()

nmds

### Scores ----

nmds_scores <- nmds |> 
  vegan::scores(display = "sites") |> 
  tibble::as.tibble() |> 
  tibble::rownames_to_column() |> 
  dplyr::rename("comunidade" = rowname) |> 
  dplyr::mutate(comunidade = paste0("comunidade ", comunidade), 
                stress = nmds_mat$stress,
                Matriz = "Sem Matriz")

nmds_scores

## nMDS com matriz inicial ----

### nMDS ----

nmds_mat <- com |> 
  vegan::vegdist() |> 
  vegan::metaMDS()

nmds_mat

### Scores ----

nmds_mat_scores <- nmds_mat |> 
  vegan::scores() |> 
  tibble::as.tibble() |> 
  tibble::rownames_to_column() |> 
  dplyr::rename("comunidade" = rowname) |> 
  dplyr::mutate(comunidade = paste0("comunidade ", comunidade), 
                stress = nmds_mat$stress,
                Matriz = "Com Matriz")

nmds_mat_scores

## Plotando ----

### Unindo os dados ----

nmds_df <- dplyr::bind_rows(nmds_scores,
                            nmds_mat_scores)

nmds_df

### Gráfico ----

nmds_df |> 
  ggplot(aes(NMDS1, NMDS2, label = comunidade)) +
  geom_label() +
  facet_wrap(~ Matriz, ncol = 1) +
  scale_x_continuous(expand = c(0.05, 0.05)) +
  scale_y_continuous(expand = c(0.05, 0.05))

cor(com, 
    nmds_mat |> 
      vegan::scores()) |> 
  as.data.frame() |> 
  dplyr::arrange(NMDS1 |> desc())
