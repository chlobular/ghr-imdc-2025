# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Random effects ===============================================================
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

# Load packages
library("dplyr")
library("INLA")
library("spdep")
# library("GHRmodel") # Comment if in MN5
# library("GHRpredict") # Comment if in MN5

# Local packages in MN5 - comment if debugging
library("GHRmodel", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/") 
# library("GHRpredict", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/") 

# Relative paths in MN5 - comment if debugging
setwd("/gpfs/scratch/bsc32/bsc498895/sprint2025")

# Read data, keep relevant columns for RE estimation
data <- read.csv("data/processed/weekly_data.csv")
data <- dplyr::select(data,
                      -starts_with("tas"), -starts_with("prlr"),
                      -starts_with("spi"), -starts_with("spei"),
                      -starts_with("nino"), -starts_with("oni"),
                      "nino_id") |> 
  mutate(date = as.Date(date))
glimpse(data)

# For debugging only
# data <- data[data$year >= 2018,]

# 1. Formulas, priors, graph, utils ----

# Formulas
source("R/00_RE_formuni.R")

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
precision.prior2 <- list(prec = list(prior = "pc.prec", param = c(0.25, 0.01))) 

# Graph
g <- readRDS("data/processed/graph.rds")

# Utils
source("R/00_functions.R")


# 2. Fit models to formulas ----

# # Baseline model
# print("Baseline")
# sprint_mod(all_baseline, data, "", create_dir = T)

# # Spatial RE
# print("Spatial RE")
# for(i in 1:length(all_s)){
#   print(names(all_s)[i])
#   sprint_mod(all_s[i], data, "", create_dir = T)
# }

# Weekly RE
print("Weekly RE")
for(i in 8:length(all_w)){
  print(names(all_w[i]))
  sprint_mod(all_w[i], data, "", create_dir = T)
}

# Yearly RE
print("Yearly RE")
for(i in 1:length(all_y)){
  print(names(all_y[i]))
  sprint_mod(all_y[i], data, "", create_dir = T)
}

# Temporal RE
print("Temporal RE")
for(i in 1:length(all_t)){
  print(names(all_t[i]))
  sprint_mod(all_t[i], data, "", create_dir = T)
}
