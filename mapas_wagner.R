# Instalar pacotes ----

if(require(c("tidyverse",
             "rnaturalearth",
             "geobr",
             "adehabitatR")) == FALSE){ 
  
  install.packages(c("tidyverse",
                     "rnaturalearth",
                     "geobr",
                     "adehabitatR"))
  
}

# Carregar pacotes ----

library(tidyverse)

library(rnaturalearth)

library(geobr)

library(adehabitatR)

# Dados ----