# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# CV: baseline models ==========================================================
# 
# Description:
#     Cross-validation model selection with evaluation
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

# Local packages in MN5 
library("GHRmodel", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/")
library("GHRpredict", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/")

# Relative paths in MN5 
setwd("/gpfs/scratch/bsc32/bsc498895/sprint2025")

# Read data
data <- read.csv("data/processed/weekly_data.csv")
data$date <- as.Date(data$date)

# More indices
data$koppen_id2 <- data$koppen_id
data$koppen_id3 <- data$koppen_id
data$koppen_id4 <- data$koppen_id
data$koppen_id5 <- data$koppen_id
data$koppen_id6 <- data$koppen_id
data$koppen_id7 <- data$koppen_id
data$koppen_id8 <- data$koppen_id

# Interaction variables 
data$v1 <- data$tasan6.l1
data$v2 <- data$spei3.l1
data$v3 <- data$spei12.l3
data$v1v2 <- data$v1 * data$v2
data$v1v3 <- data$v1 * data$v3
data$v2v3 <- data$v2 * data$v3
data$v1v2v3 <- data$v1 * data$v2 * data$v3

# Spatial replicate index
data$period_id <- ifelse(data$epiyear <= 2018, 1, 2)


# 1. Priors, graph, utils ----

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# Graph
g <- readRDS("data/processed/graph.rds")

# Utils
source("R/00_functions.R")


# 2. RE ----

# Spatial
re_s <- paste("f(hr_id, model = 'bym2', replicate = period_id, graph = g, scale.model = TRUE,",
              "hyper = precision.prior, constr = TRUE)")
# Weekly
re_w <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
              "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
# Yearly
re_y <- "f(year_id, model = 'iid', hyper = precision.prior)"

# 3. FE ----
fe_lsl <- paste(cov_varying("v1", "koppen_id", ""),
                cov_varying("v2", "koppen_id2", ""),
                cov_varying("v3", "koppen_id3", ""),
                cov_varying("v1v2", "koppen_id4", ""),
                cov_varying("v1v3", "koppen_id5", ""),
                cov_varying("v2v3", "koppen_id6", ""),
                cov_varying("v1v2v3", "koppen_id7", ""),
                sep = " + ")
fe_other <- paste(cov_varying("tas6.l1", "koppen_id8", ""),
                  cov_nl("oni.l6", pattern = "", method = "cut")[[1]],
                  sep = " + ")

# 4. Fit ----
(form_fit <- paste("", fe_lsl, fe_other, re_s, re_w, sep = " + "))
sprint_eval(form_fit, data, "output/CV3", "mod26_2020", 2020); gc()
sprint_eval(form_fit, data, "output/CV3", "mod26_2021", 2021); gc()
sprint_eval(form_fit, data, "output/CV3", "mod26_2022", 2022); gc()
sprint_eval(form_fit, data, "output/CV3", "mod26_2023", 2023); gc()
sprint_eval(form_fit, data, "output/CV3", "mod26_2024", 2024); gc()
