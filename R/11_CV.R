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

## 4.1 No inter models ----

# mod1
(f1 <- paste("", re_s_norep, re_w, 
             sep = " + "))
sprint_mod(f1, data, "output/CV", "mod1", eval_train = FALSE, eval_test = TRUE)

# mod1.lsl
(f1.lsl <- paste("", fe_lsl, re_s_norep, re_w, 
             sep = " + "))
sprint_mod(f1.lsl, data, "output/CV", "mod1.lsl", eval_train = FALSE, eval_test = TRUE)

# mod2
(f2 <- paste("", re_s_rep, re_w, 
             sep = " + "))
sprint_mod(f2, data, "output/CV", "mod2", eval_train = FALSE, eval_test = TRUE)

# mod2.lsl
(f2.lsl <- paste("", fe_lsl, re_s_rep, re_w, 
                 sep = " + "))
sprint_mod(f2.lsl, data, "output/CV", "mod2.lsl", eval_train = FALSE, eval_test = TRUE)


## 4.2 IID models ----

# mod3
(f3 <- paste("", re_s_norep, re_w, re_y_iid_norep,
            sep = " + "))
sprint_mod(f3, data, "output/CV", "mod3", eval_train = FALSE, eval_test = TRUE)

# mod3.lsl
(f3.lsl <- paste("", fe_lsl, re_s_norep, re_w, re_y_iid_norep,
             sep = " + "))
sprint_mod(f3.lsl, data, "output/CV", "mod3.lsl", eval_train = FALSE, eval_test = TRUE)

# mod4
(f4 <- paste("", re_s_rep, re_w, re_y_iid_norep,
             sep = " + "))
sprint_mod(f4, data, "output/CV", "mod4", eval_train = FALSE, eval_test = TRUE)

# mod4.lsl
(f4.lsl <- paste("", fe_lsl, re_s_rep, re_w, re_y_iid_norep,
             sep = " + "))
sprint_mod(f4.lsl, data, "output/CV", "mod4.lsl", eval_train = FALSE, eval_test = TRUE)

# mod5
(f5 <- paste("", re_s_rep, re_w, re_y_iid_rep,
             sep = " + "))
sprint_mod(f5, data, "output/CV", "mod5", eval_train = FALSE, eval_test = TRUE)

# mod5.lsl
(f5.lsl <- paste("", fe_lsl, re_s_rep, re_w, re_y_iid_rep,
             sep = " + "))
sprint_mod(f5.lsl, data, "output/CV", "mod5.lsl", eval_train = FALSE, eval_test = TRUE)


## 4.3 RW1 models ----

# mod6
(f6 <- paste("", re_s_rep, re_w, re_y_rw1_norep,
             sep = " + "))
sprint_mod(f6, data, "output/CV", "mod6", eval_train = FALSE, eval_test = TRUE)

# mod6.lsl
(f6.lsl <- paste("", fe_lsl, re_s_rep, re_w, re_y_rw1_norep,
                 sep = " + "))
sprint_mod(f6.lsl, data, "output/CV", "mod6.lsl", eval_train = FALSE, eval_test = TRUE)

# mod7
(f7 <- paste("", re_s_rep, re_w, re_y_rw1_rep,
             sep = " + "))
sprint_mod(f7, data, "output/CV", "mod7", eval_train = FALSE, eval_test = TRUE)

# mod7.lsl
(f7.lsl <- paste("", fe_lsl, re_s_rep, re_w, re_y_rw1_rep,
                 sep = " + "))
sprint_mod(f7.lsl, data, "output/CV", "mod7.lsl", eval_train = FALSE, eval_test = TRUE)


## 4.3 RW2 models ----

# mod8
(f8 <- paste("", re_s_rep, re_w, re_y_rw2_norep,
             sep = " + "))
sprint_mod(f8, data, "output/CV", "mod8", eval_train = FALSE, eval_test = TRUE)

# mod8.lsl
(f8.lsl <- paste("", fe_lsl, re_s_rep, re_w, re_y_rw2_norep,
                 sep = " + "))
sprint_mod(f8.lsl, data, "output/CV", "mod8.lsl", eval_train = FALSE, eval_test = TRUE)

# mod9
(f9 <- paste("", re_s_rep, re_w, re_y_rw2_rep,
             sep = " + "))
sprint_mod(f9, data, "output/CV", "mod9", eval_train = FALSE, eval_test = TRUE)

# mod9.lsl
(f9.lsl <- paste("", fe_lsl, re_s_rep, re_w, re_y_rw2_rep,
                 sep = " + "))
sprint_mod(f9.lsl, data, "output/CV", "mod9.lsl", eval_train = FALSE, eval_test = TRUE)
