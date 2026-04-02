# Instalar pacotes ----

if(require(c("tidyverse",
             "rnaturalearth",
             "geobr",
             "adehabitatHR")) == FALSE){ 
  
  install.packages(c("tidyverse",
                     "rnaturalearth",
                     "geobr",
                     "adehabitatHR"))
  
}

# Carregar pacotes ----

library(tidyverse)

library(rnaturalearth)

library(geobr)

library(adehabitatR)

# Dados ----