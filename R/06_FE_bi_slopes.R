# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fixed effects: bivariate =====================================================
# 
# Description (phase 1):
#     Selection of random slope unit in selected bivariate models
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

# Standard region
data$region_id2 <- data$region_id

# Standard koppen
data$koppen_id2 <- data$koppen_id

# Level 1 koppen (A, B, C)
data$koppen_lvl1 <- substr(data$koppen, 1, 1)
data$koppen_lvl1_id <- as.integer(as.factor(data$koppen_lvl1))
data$koppen_lvl1_id2 <- data$koppen_lvl1_id

# Mixed koppen 1 (Af+Am, As+Aw, B, C)
data$koppen_mixed1 <- case_when(
  data$koppen %in% c("Af", "Am") ~ "Af+Am",
  data$koppen %in% c("As", "Aw") ~ "As+Aw",
  substr(data$koppen, 1, 1) == "B" ~ "B",
  substr(data$koppen, 1, 1) == "C" ~ "C"
)
data$koppen_mixed1_id <- as.integer(as.factor(data$koppen_mixed1))
data$koppen_mixed1_id2 <- data$koppen_mixed1_id

# Mixed koppen 2 (Af, Am+As, Aw, B, C)
data$koppen_mixed2 <- case_when(
  data$koppen == "Af" ~ "Af",
  data$koppen %in% c("Am", "As") ~ "Am+As",
  data$koppen == "Aw" ~ "Aw",
  substr(data$koppen, 1, 1) == "B" ~ "B",
  substr(data$koppen, 1, 1) == "C" ~ "C"
)
data$koppen_mixed2_id <- as.integer(as.factor(data$koppen_mixed2))
data$koppen_mixed2_id2 <- data$koppen_mixed2_id


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

c1 <- c("tas6", "tas6.l1", "spei12.l2", "spei12.l3", "spei12.l3")
c2 <- c("tasan6", "tasan6.l1", "spei3", "spei3", "spei3.l1")
all(c(c1, c2) %in% names(data))
covs <- data.frame(v1 = c1, v2 = c2, stringsAsFactors = FALSE)
write.csv(covs, row.names = FALSE, "output/bi_fe_slopes/covs.csv")

# Random slopes per region
covs_region <- covs
covs_region$form <- write_FE_form(
  paste0(unlist(cov_varying(covs$v1, pattern = "", unit = "region_id")), " + ",
         unlist(cov_varying(covs$v2, pattern = "", unit = "region_id2"))))

# Random slopes per koppen
covs_koppen <- covs
covs_koppen$form <- write_FE_form(
  paste0(unlist(cov_varying(covs$v1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(covs$v2, pattern = "", unit = "koppen_id2"))))

# Random slopes per koppen level 1
covs_koppen_lvl1 <- covs
covs_koppen_lvl1$form <- write_FE_form(
  paste0(unlist(cov_varying(covs$v1, pattern = "", unit = "koppen_lvl1_id")), " + ",
         unlist(cov_varying(covs$v2, pattern = "", unit = "koppen_lvl1_id2"))))

# Random slopes per koppen mixed 1
covs_koppen_mixed1 <- covs
covs_koppen_mixed1$form <- write_FE_form(
  paste0(unlist(cov_varying(covs$v1, pattern = "", unit = "koppen_mixed1_id")), " + ",
         unlist(cov_varying(covs$v2, pattern = "", unit = "koppen_mixed1_id2"))))

# Random slopes per koppen mixed 2
covs_koppen_mixed2 <- covs
covs_koppen_mixed2$form <- write_FE_form(
  paste0(unlist(cov_varying(covs$v1, pattern = "", unit = "koppen_mixed2_id")), " + ",
         unlist(cov_varying(covs$v2, pattern = "", unit = "koppen_mixed2_id2"))))


# 4. Fit models ----

# Random slopes per region
sprint_mod(covs_region$form, data, "output/bi_fe_slopes", "region")

# Random slopes per koppen
sprint_mod(covs_koppen$form, data, "output/bi_fe_slopes", "koppen")

# Random slopes per koppen level 1
sprint_mod(covs_koppen_lvl1$form, data, "output/bi_fe_slopes", "koppen_lvl1")

# Random slopes per koppen mixed 1
sprint_mod(covs_koppen_mixed1$form, data, "output/bi_fe_slopes", "koppen_mixed1")

# Random slopes per koppen mixed 2
sprint_mod(covs_koppen_mixed2$form, data, "output/bi_fe_slopes", "koppen_mixed2")
