# Instalando os pacotes -----

# ⚠️⚠️⚠️⚠️⚠️APENAS RODE ISSO SE VOCÊ NÃO TIVER ESSES PACOTES INSTALADOS⚠️⚠️⚠️⚠️⚠️

install.packages(c("readxl",
                   "tidyverse",
                   "geobr",
                   "sf",
                   "patchwork"))

# Carregando os pacotes ----

library(readxl)

library(tidyverse)

library(geobr)

library(sf)

library(patchwork)

# Dados ----

## Coordenada ----

### Importando ----

livia <- readxl::read_xlsx("dados_livia.xlsx")

### Visualizando ----

livia

livia |> dplyr::glimpse()
