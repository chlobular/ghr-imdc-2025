# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fixed effects: multivariate =====================================================
# 
# Description:
#     Selection of multivariate climatic effects
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

# Local packages in MN5 
library("GHRmodel", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/")

# Relative paths in MN5 
setwd("/gpfs/scratch/bsc32/bsc498895/sprint2025")

# Read data
data <- read.csv("data/processed/weekly_data.csv")

# More koppen indices
data$koppen_id2 <- data$koppen_id
data$koppen_id3 <- data$koppen_id
data$koppen_id4 <- data$koppen_id
data$koppen_id5 <- data$koppen_id


# 1. Priors, graph, utils ----

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# Graph
g <- readRDS("data/processed/graph.rds")

# Utils
source("R/00_functions.R")


# 2. RE ----
re_s <- paste("f(hr_id, model = 'bym2', replicate = nino_id, graph = g,",
              "scale.model = TRUE, hyper = precision.prior, constr = TRUE)")
re_w <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
              "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_y <- "f(year_id, model = 'iid', hyper = precision.prior, constr = TRUE)"


# 3. FE ----

# Baseline model
fe_base <- paste(unlist(cov_varying("tas6.l1", pattern = "", unit = "koppen_id")), 
                 unlist(cov_varying("tasan6.l1", pattern = "", unit = "koppen_id2")), 
                 unlist(cov_varying("spei12.l3", pattern = "", unit = "koppen_id3")),
                 unlist(cov_varying("spei3.l1", pattern = "", unit = "koppen_id4")),
                 sep=" + ")
fe_base <- paste0(" + ", fe_base)

# ONI candidates
fe_candidate <- paste0("oni.l", 1:6)

# Linear
fe_linear <- paste(fe_base, fe_candidate, sep = " + ")

# Non-linear, equal
fe_equal <- paste(fe_base, cov_nl(fe_candidate, pattern = "", 
                                  method = "cut")[[1]], 
                  sep = " + ")

# Non-linear, quantile
fe_quant <- paste(fe_base, cov_nl(fe_candidate, pattern = "", n = 8)[[1]], 
                  sep = " + ")

# Random slopes
fe_linear_koppen <- paste(fe_base, cov_varying(fe_candidate, pattern = "", 
                                        unit = "koppen_id5")[[1]], 
                   sep = " + ")


# 4. Fit models ----

# # Baseline 
# (fe_base <- paste(fe_base, re_s, re_w, re_y, sep = " + "))
# sprint_mod(fe_base, data, "output/oni_fe", "baseline")
# 
# # Linear
# (fe_linear <- paste(fe_linear, re_s, re_w, re_y, sep = " + "))
# sprint_mod(fe_linear, data, "output/oni_fe", "linear")

# Non-linear, equal
(fe_equal <- paste(fe_equal, re_s, re_w, re_y, sep = " + "))
sprint_mod(fe_equal, data, "output/oni_fe", "equal")

# Non-linear, quantile
(fe_quant <- paste(fe_quant, re_s, re_w, re_y, sep = " + "))
sprint_mod(fe_quant, data, "output/oni_fe", "quant")

# # Random slopes
# (fe_linear_koppen <- paste(fe_linear_koppen, re_s, re_w, re_y, sep = " + "))
# sprint_mod(fe_linear_koppen, data, "output/oni_fe", "linear_koppen")
