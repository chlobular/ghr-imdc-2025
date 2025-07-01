# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fixed effects: univariate ====================================================
# 
# Description:
#     Fit complete random effect models for selection
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
# data <- data[data$year >= 2023,] # For debugging only


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

# baseline 
baseline_vars <- "baseline"
baseline_form <- list()
baseline_form[[1]] <- ""
data_baseline <- data[c("hr_id", "week_id", "year_id", "nino_id", "state_id")]

# tas
tas_vars <- extract_names(data, pattern = "tas")
tas_vars <- tas_vars[!grepl("tasan", tas_vars)]
tas_form_lin <- cov_uni(tas_vars, pattern = "tas")
tas_form_nl <- cov_nl(tas_form_lin, pattern = "", method = "cut")
tas_form_rep1 <- cov_varying(tas_form_lin, pattern = "", unit = "region_id")
tas_form_rep2 <- cov_varying(tas_form_lin, pattern = "", unit = "koppen_id")
tas_form_rep3 <- cov_varying(tas_form_lin, pattern = "", unit = "biome_id")

# tasan
tasan_vars <- extract_names(data, pattern = "tasan")
tasan_form_lin <- cov_uni(tasan_vars, pattern = "tasan")
tasan_form_nl <- cov_nl(tasan_form_lin, pattern = "", method = "cut")
tasan_form_rep1 <- cov_varying(tasan_form_lin, pattern = "", unit = "region_id")
tasan_form_rep2 <- cov_varying(tasan_form_lin, pattern = "", unit = "koppen_id")
tasan_form_rep3 <- cov_varying(tasan_form_lin, pattern = "", unit = "biome_id")

# prlr
prlr_vars <- extract_names(data, pattern = "prlr")
prlr_form_lin <- cov_uni(prlr_vars, pattern = "prlr")
prlr_form_nl <- cov_nl(prlr_form_lin, pattern = "", method = "cut")
prlr_form_rep1 <- cov_varying(prlr_form_lin, pattern = "", unit = "region_id")
prlr_form_rep2 <- cov_varying(prlr_form_lin, pattern = "", unit = "koppen_id")
prlr_form_rep3 <- cov_varying(prlr_form_lin, pattern = "", unit = "biome_id")

# spi and spei
sp_vars <- extract_names(data, pattern = "sp")
sp_form_lin <- cov_uni(sp_vars, pattern = "sp")
sp_form_nl <- cov_nl(sp_form_lin, pattern = "", method = "cut")
sp_form_rep1 <- cov_varying(sp_form_lin, pattern = "", unit = "region_id")
sp_form_rep2 <- cov_varying(sp_form_lin, pattern = "", unit = "koppen_id")
sp_form_rep3 <- cov_varying(sp_form_lin, pattern = "", unit = "biome_id")

# nino
nino_vars <- c(extract_names(data, pattern = "nino"),
               extract_names(data, pattern = "oni"))
nino_form_lin <- cov_uni(nino_vars, pattern = "nino")
nino_form_nl <- cov_nl(nino_form_lin, pattern = "", method = "cut")
nino_form_rep1 <- cov_varying(nino_form_lin, pattern = "", unit = "region_id")
nino_form_rep2 <- cov_varying(nino_form_lin, pattern = "", unit = "koppen_id")
nino_form_rep3 <- cov_varying(nino_form_lin, pattern = "", unit = "biome_id")


# 4. Formulas ----

# baseline 
baseline_form <- write_FE_form(baseline_form)

# tas
tas_form_lin <- write_FE_form(tas_form_lin)
tas_form_nl <- write_FE_form(tas_form_nl)
tas_form_rep1 <- write_FE_form(tas_form_rep1)
tas_form_rep2 <- write_FE_form(tas_form_rep2)
tas_form_rep3 <- write_FE_form(tas_form_rep3)

# tasan
tasan_form_lin <- write_FE_form(tasan_form_lin)
tasan_form_nl <- write_FE_form(tasan_form_nl)
tasan_form_rep1 <- write_FE_form(tasan_form_rep1)
tasan_form_rep2 <- write_FE_form(tasan_form_rep2)
tasan_form_rep3 <- write_FE_form(tasan_form_rep3)

# prlr
prlr_form_lin <- write_FE_form(prlr_form_lin)
prlr_form_nl <- write_FE_form(prlr_form_nl)
prlr_form_rep1 <- write_FE_form(prlr_form_rep1)
prlr_form_rep2 <- write_FE_form(prlr_form_rep2)
prlr_form_rep3 <- write_FE_form(prlr_form_rep3)

# spi and spei
sp_form_lin <- write_FE_form(sp_form_lin)
sp_form_nl <- write_FE_form(sp_form_nl)
sp_form_rep1 <- write_FE_form(sp_form_rep1)
sp_form_rep2 <- write_FE_form(sp_form_rep2)
sp_form_rep3 <- write_FE_form(sp_form_rep3)

# nino
nino_form_lin <- write_FE_form(nino_form_lin)
nino_form_nl <- write_FE_form(nino_form_nl)
nino_form_rep1 <- write_FE_form(nino_form_rep1)
nino_form_rep2 <- write_FE_form(nino_form_rep2)
nino_form_rep3 <- write_FE_form(nino_form_rep3)


# 5. Fit models ----

# # baseline 
# sprint_mod(baseline_form, data, "output/uni_fe", "baseline")
# 
# # tas
# sprint_mod(tas_form_lin, data, "output/uni_fe", "tas_lin")
# sprint_mod(tas_form_nl, data, "output/uni_fe", "tas_nl")
# sprint_mod(tas_form_rep1, data, "output/uni_fe", "tas_rep1")
# sprint_mod(tas_form_rep2, data, "output/uni_fe", "tas_rep2")
# sprint_mod(tas_form_rep3, data, "output/uni_fe", "tas_rep3")

# tasan
sprint_mod(tasan_form_lin, data, "output/uni_fe", "tasan_lin")
sprint_mod(tasan_form_nl, data, "output/uni_fe", "tasan_nl")
sprint_mod(tasan_form_rep1, data, "output/uni_fe", "tasan_rep1")
sprint_mod(tasan_form_rep2, data, "output/uni_fe", "tasan_rep2")
sprint_mod(tasan_form_rep3, data, "output/uni_fe", "tasan_rep3")

# # prlr
# sprint_mod(prlr_form_lin, data, "output/uni_fe", "prlr_lin")
# sprint_mod(prlr_form_nl, data, "output/uni_fe", "prlr_nl")
# sprint_mod(prlr_form_rep1, data, "output/uni_fe", "prlr_rep1")
# sprint_mod(prlr_form_rep2, data, "output/uni_fe", "prlr_rep2")
# sprint_mod(prlr_form_rep3, data, "output/uni_fe", "prlr_rep3")
# 
# # spi and spei
# sprint_mod(sp_form_lin, data, "output/uni_fe", "sp_lin")
# sprint_mod(sp_form_nl, data, "output/uni_fe", "sp_nl")
# sprint_mod(sp_form_rep1, data, "output/uni_fe", "sp_rep1")
# sprint_mod(sp_form_rep2, data, "output/uni_fe", "sp_rep2")
# sprint_mod(sp_form_rep3, data, "output/uni_fe", "sp_rep3")
# 
# # nino
# sprint_mod(nino_form_lin, data, "output/uni_fe", "nino_lin")
# sprint_mod(nino_form_nl, data, "output/uni_fe", "nino_nl")
# sprint_mod(nino_form_rep1, data, "output/uni_fe", "nino_rep1")
# sprint_mod(nino_form_rep2, data, "output/uni_fe", "nino_rep2")
# sprint_mod(nino_form_rep3, data, "output/uni_fe", "nino_rep3")
