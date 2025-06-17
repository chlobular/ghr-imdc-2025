# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Random effects ============================================================
# 
# Description:
#     Fit random effect models for selection
# 
# Script authors:
#     Carles Milà  (carles.milagarcia@bsc.es)
#     Chloe Fletcher  (chloe.fletcher@bsc.es)
#
# Env:
#   * Debug: Local
#   * Run: MN5
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# 0. Setup ----

# load packages
library("dplyr")
library("INLA")
library("spdep")
library("sf")
sf::sf_use_s2(FALSE)

# read data, keep relevant columns for RE estimation
data <- read.csv("data/processed/weekly_data.csv")
data <- dplyr::select(data,
                      -starts_with("tas"), -starts_with("prlr"),
                      -starts_with("spi"), -starts_with("spei"),
                      -starts_with("nino"), -starts_with("oni"))

# Read geometries (ordered by ID)
boundaries <- read_sf("data/boundaries/shape_regional_health.gpkg")

# 1. Formulas, priors and graph ----

# Formulas
source("R/00_RE_formulas.R")

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
precision.prior2 <- list(prec = list(prior = "pc.prec", param = c(0.25, 0.01))) 

# Graph
nb <- poly2nb(boundaries)
g <- nb2mat(nb, style = "B")
