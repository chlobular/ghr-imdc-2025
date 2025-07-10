# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fixed effects: bivariate ====================================================
# 
# Description:
#     Bivariate FE model selection
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
data$koppen_id2 <- data$koppen_id
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

# M0: baseline
baseline_vars <- "baseline"
baseline_form <- list()
baseline_form[[1]] <- ""
data_baseline <- data[c("hr_id", "week_id", "year_id", "nino_id", "state_id")]

# M1: tas6 + tasan6 (lags 0-3)
c1 <- c("tas6", "tas6.l1", "tas6.l2", "tas6.l3")
c2 <- c("tasan6", "tasan6.l1", "tasan6.l2", "tasan6.l3")
all(c(c1, c2) %in% names(data))
m1_form <- expand.grid(c1, c2, stringsAsFactors = FALSE)
names(m1_form) <- c("c1", "c2")
m1_form$form <- write_FE_form(
  paste0(unlist(cov_varying(m1_form$c1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(m1_form$c2, pattern = "", unit = "koppen_id2"))))
write.csv(m1_form, row.names = FALSE, "output/bi_fe/m1_vars.csv")
rm("c1", "c2")

# M2: tas12 + tasan12 (lags 0-3)
c1 <- c("tas12", "tas12.l1", "tas12.l2", "tas12.l3")
c2 <- c("tasan12", "tasan12.l1", "tasan12.l2", "tasan12.l3")
all(c(c1, c2) %in% names(data))
m2_form <- expand.grid(c1, c2, stringsAsFactors = FALSE)
names(m2_form) <- c("c1", "c2")
m2_form$form <- write_FE_form(
  paste0(unlist(cov_varying(m2_form$c1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(m2_form$c2, pattern = "", unit = "koppen_id2"))))
write.csv(m2_form, row.names = FALSE, "output/bi_fe/m2_vars.csv")
rm("c1", "c2")

# M3: spei12 (lags 2-6) + spei3 (lags 0-2)
c1 <- c("spei12.l2", "spei12.l3", "spei12.l4", "spei12.l5", "spei12.l6")
c2 <- c("spei3", "spei3.l1", "spei3.l2")
all(c(c1, c2) %in% names(data))
m3_form <- expand.grid(c1, c2, stringsAsFactors = FALSE)
names(m3_form) <- c("c1", "c2")
m3_form$form <- write_FE_form(
  paste0(unlist(cov_varying(m3_form$c1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(m3_form$c2, pattern = "", unit = "koppen_id2"))))
write.csv(m3_form, row.names = FALSE, "output/bi_fe/m3_vars.csv")
rm("c1", "c2")

# M4: spei12 (lags 2-6) + spei1 (lags 0-3)
c1 <- c("spei12.l2", "spei12.l3", "spei12.l4", "spei12.l5", "spei12.l6")
c2 <- c("spei1", "spei1.l1", "spei1.l2", "spei1.l3")
all(c(c1, c2) %in% names(data))
m4_form <- expand.grid(c1, c2, stringsAsFactors = FALSE)
names(m4_form) <- c("c1", "c2")
m4_form$form <- write_FE_form(
  paste0(unlist(cov_varying(m4_form$c1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(m4_form$c2, pattern = "", unit = "koppen_id2"))))
write.csv(m4_form, row.names = FALSE, "output/bi_fe/m4_vars.csv")
rm("c1", "c2")

# M5: spei6 (lags 4-6) + spei6 (lags 0-3), excluding the 3 * 4 combo
c1 <- c("spei6.l4", "spei6.l5", "spei6.l6")
c2 <- c("spei6", "spei6.l1", "spei6.l2", "spei6.l3")
all(c(c1, c2) %in% names(data))
m5_form <- expand.grid(c1, c2, stringsAsFactors = FALSE)
names(m5_form) <- c("c1", "c2")
m5_form <- m5_form[!(m5_form$c1=="spei6.l4" & m5_form$c2=="spei6.l3"),]
m5_form$form <- write_FE_form(
  paste0(unlist(cov_varying(m5_form$c1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(m5_form$c2, pattern = "", unit = "koppen_id2"))))
write.csv(m5_form, row.names = FALSE, "output/bi_fe/m5_vars.csv")
rm("c1", "c2")

# M6: spei12 (lags 1-4) + nino6 (lags 4-6)
c1 <- c("spei12.l1", "spei12.l2", "spei12.l3", "spei12.l4")
c2 <- c("nino6.l4", "nino6.l5", "nino6.l6")
all(c(c1, c2) %in% names(data))
m6_form <- expand.grid(c1, c2, stringsAsFactors = FALSE)
names(m6_form) <- c("c1", "c2")
m6_form$form <- write_FE_form(
  paste0(unlist(cov_varying(m6_form$c1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(m6_form$c2, pattern = "", unit = "koppen_id2"))))
write.csv(m6_form, row.names = FALSE, "output/bi_fe/m6_vars.csv")
rm("c1", "c2")

# M7: spei12 (lags 1-4) + oni (lags 5-6)
c1 <- c("spei12.l1", "spei12.l2", "spei12.l3", "spei12.l4")
c2 <- c("oni.l5", "oni.l6")
all(c(c1, c2) %in% names(data))
m7_form <- expand.grid(c1, c2, stringsAsFactors = FALSE)
names(m7_form) <- c("c1", "c2")
m7_form$form <- write_FE_form(
  paste0(unlist(cov_varying(m7_form$c1, pattern = "", unit = "koppen_id")), " + ",
         unlist(cov_varying(m7_form$c2, pattern = "", unit = "koppen_id2"))))
write.csv(m7_form, row.names = FALSE, "output/bi_fe/m7_vars.csv")
rm("c1", "c2")


# 4. Fit models ----

# M0: baseline
sprint_mod(baseline_form, data, "output/bi_fe", "baseline")

# M1: tas6 + tasan6 (lags 0-3)
sprint_mod(m1_form$form, data, "output/bi_fe", "m1")

# M2: tas12 + tasan12 (lags 0-3)
sprint_mod(m2_form$form, data, "output/bi_fe", "m2")

# M3: spei12 (lags 2-6) + spei3 (lags 0-2)
sprint_mod(m3_form$form, data, "output/bi_fe", "m3")

# M4: spei12 (lags 2-6) + spei1 (lags 0-3)
sprint_mod(m4_form$form, data, "output/bi_fe", "m4")

# M5: spei6 (lags 4-6) + spei6 (lags 0-3), excluding the 3 * 4 combo
sprint_mod(m5_form$form, data, "output/bi_fe", "m5")

# M6: spei12 (lags 1-4) + nino6 (lags 4-6)
sprint_mod(m6_form$form, data, "output/bi_fe", "m6")

# M7: spei12 (lags 1-4) + oni (lags 5-6)
sprint_mod(m7_form$form, data, "output/bi_fe", "m7")
