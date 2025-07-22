# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# CV: baseline models ==========================================================
# 
# Description:
#     Cross-validation model selection of baseline models
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
library("splines")

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


# 1. Priors, graph, utils ----

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# Graph
g <- readRDS("data/processed/graph.rds")

# Utils
source("R/00_functions.R")


# 2. RE ----

# Spatial
re_s_norep <- paste("f(hr_id, model = 'bym2', graph = g, scale.model = TRUE,",
                    "hyper = precision.prior, constr = TRUE)")
re_s_rep <- paste("f(hr_id, model = 'bym2', replicate = nino_id, graph = g,",
                  "scale.model = TRUE, hyper = precision.prior, constr = TRUE)")

# Weekly
re_w <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
              "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

# Yearly
re_y_iid_norep <- "f(year_id, model = 'iid', hyper = precision.prior)"
re_y_rw1_norep <- "f(year_id, model = 'rw1', hyper = precision.prior, constr = TRUE)"
re_y_rw2_norep <- "f(year_id, model = 'rw2', hyper = precision.prior, constr = TRUE)"
re_y_iid_rep <- paste("f(year_id, model = 'iid', replicate = region_id, ",
                      "hyper = precision.prior, constr = TRUE)")
re_y_rw1_rep <- paste("f(year_id, model = 'rw1', replicate = region_id, ",
                      "hyper = precision.prior, constr = TRUE)")
re_y_rw2_rep <- paste("f(year_id, model = 'rw2', replicate = region_id, ",
                      "hyper = precision.prior, constr = TRUE)")

# 3. FE ----
fe_lsl <- paste(cov_varying("v1", "koppen_id", ""),
                cov_varying("v2", "koppen_id2", ""),
                cov_varying("v3", "koppen_id3", ""),
                cov_varying("v1v2", "koppen_id4", ""),
                cov_varying("v1v3", "koppen_id5", ""),
                cov_varying("v2v3", "koppen_id6", ""),
                cov_varying("v1v2v3", "koppen_id7", ""),
                sep = " + ")

# 4. Fit ----

# mod7.lsl
(f7.lsl <- paste("", fe_lsl, re_s_rep, re_w, re_y_rw1_rep,
                 sep = " + "))
sprint_mod(f7.lsl, data, "output/CV", "mod7.lsl", eval_train = FALSE, eval_test = TRUE)
