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