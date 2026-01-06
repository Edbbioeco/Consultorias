# Pacotes ----

library(RefManageR)

library(tidyverse)

# Dados ----

## Importando ----

bib <- RefManageR::ReadBib("library.bib")

## Visualizando ----

bib

## Tratando ----

bib_df <- bib |> 
  tibble::as.tibble()

bib_df

bib_df |> dplyr::glimpse()

# Setando temas -----

theme_set(theme_bw() +
            theme(axis.text = element_text(color = "black", size = 15),
                  axis.title = element_text(color = "black", size = 15),
                  legend.text = element_text(color = "black", size = 15),
                  legend.title = element_text(color = "black", size = 15),
                  panel.border = element_rect(color = "black", linewidth = 1)))

# Análises ----

## Tipo de referência por ano ----

bib_df |> 
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = c(bibtype, year)) |> 
  dplyr::mutate(year = year |> as.numeric()) |> 
  ggplot(aes(year, quantidade, color = bibtype)) +
  geom_line(linewidth = 1) +
  labs(x = "Ano",
       y = "Quantidade de trabalhos",
       color = "Tipo de bibliografia") +
  scale_color_manual(values = c("tomato", 
                                "gold", 
                                "limegreen", 
                                "royalblue", 
                                "purple"))


## Tipo de jornal por ano ----

bib_df |> 
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = c(journal)) |> 
  dplyr::arrange(quantidade|> dplyr::desc()) |> 
  tidyr::drop_na()

bib_df |> 
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = c(journal, year)) |> 
  dplyr::mutate(year = year |> as.numeric()) |> 
  tidyr::drop_na() |> 
  dplyr::filter(journal %in% c("Biotropica",
                               "Biodiversity and Conservation",
                               "Biological Conservation",
                               "Ecology",
                               "PLoS ONE"))  |> 
  ggplot(aes(year, quantidade, color = journal)) +
  geom_line(linewidth = 1) +
  labs(x = "Ano",
       y = "Quantidade de trabalhos",
       color = "Revista científica") +
  scale_color_manual(values = c("tomato", 
                                "gold",
                                "limegreen",
                                "royalblue",
                                "purple"))

## Tamanho do título por ano ----

bib_df |> 
  dplyr::summarise(quantidade = title |> stringr::str_count(stringr::boundary("word")),
                   .by = year) |> 
  dplyr::slice_max(quantidade,
                   by = year,
                   n = 1) |> 
  dplyr::mutate(year = year |> as.numeric()) |> 
  ggplot(aes(year, quantidade)) +
  geom_line(linewidth = 1) +
  labs(x = "Ano",
       y = "Quantidade de palavras") +
  scale_x_continuous(breaks = seq(1905, 2025, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 2))

## Histogrma da quantidade de palavras ----

bib_df |> 
  dplyr::mutate(observado = title |> 
                  stringr::str_count(stringr::boundary("word"))) |> 
  ggplot(aes(observado)) +
  geom_histogram(color = "black", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  labs(x = "Quantidade de palavras",
       y = "Contagem")
