# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## LOAD Packages =================================================

# Description:
#     This script contains all of the packages to be loaded for
#     the long and short lag interaction repository for Brazil.

# Script authors:
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Dr Giovenale Moirano  (giovenale.moirano@bsc.es)
#     Prof. Rachel Lowe     (rachel.lowe@bsc.es)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

## Packages --------------------------------------------------------------------

# specify packages
packages <- c("RColorBrewer", "geofacet", "ggpubr", "ggthemes", "ggtext", 
              "leaflet", "reactable", "viridis", "cowplot", "pROC", "zoo", 
              "lubridate", "glue", "sf", "tsModel", "INLA", "tidyverse", "readxl",
              "gridExtra", "patchwork")

# install packages
# install.packages("INLA", 
#                  repos=c(getOption("repos"),
#                          INLA="https://inla.r-inla-download.org/R/testing"),
#                  dep=TRUE)
# lapply(packages[2:length(packages)], install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

