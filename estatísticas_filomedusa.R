# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Notas ----

### Importando ----

notas <- readxl::read_xlsx("bioacustica_filomedusa.xlsx") |>  
  tidyr::fill(dplyr::contains("Amp"), 
              .direction = "updown") |>  
  tidyr::fill(dplyr::contains(c("Freq", "Power")), 
              .direction = "downup") |> 
  dplyr::distinct(Selection, 
                  .keep_all = TRUE)

### Visualizando ----

notas

notas |> dplyr::glimpse()

## Intervalos ----

### Importando ----

intervalos <- readxl::read_xlsx("bioacustica_filomedusa.xlsx",
                                sheet = 2)

### Visualizando ----

intervalos

intervalos |> dplyr::glimpse()

# Análises ----

## Notas -----

notas |> 
  dplyr::summarise(dplyr::across(c(High_Freq,
                                   dplyr::contains("Delta"),
                                   dplyr::contains("Peak")),
                                 mean)) |>
  dplyr::mutate(dplyr::across(dplyr::contains("Freq"),
                              ~./1000)) |> 
  tidyr::pivot_longer(cols = dplyr::everything(),
                      values_to = "Média",
                      names_to = "Variáveis") |> 
  dplyr::bind_cols(notas |> 
                     dplyr::summarise(dplyr::across(c(High_Freq,
                                                      dplyr::contains("Delta"),
                                                      dplyr::contains("Peak")),
                                                    sd)) |>
                     dplyr::mutate(dplyr::across(dplyr::contains("Freq"),
                                                 ~./1000)) |> 
                     tidyr::pivot_longer(cols = dplyr::everything(),
                                         values_to = "Desvio Padrão",
                                         names_to = "vars")) |> 
  dplyr::select(-3) |> 
  as.data.frame()

## Intervalos ----

intervalos |> 
  dplyr::summarise(media = Delta_Time |> mean(),
                   sd = Delta_Time |> sd())
