# Load packages
library("dplyr")
library("INLA")
library("spdep")
library("GHRmodel") # Comment if in MN5

# Local packages in MN5 
# library("GHRmodel", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/")

# Relative paths in MN5 
# setwd("/gpfs/scratch/bsc32/bsc498895/sprint2025")

# Read data
data <- read.csv("data/processed/weekly_data.csv") |> 
  select(casos, pop, time_id, hr_id, week_id, nino_id, state_id, year_id)

# Basis
oneb_time <- onebasis_inla(data[,"time_id"], fun = "ns", basis_name = "ob_time", df = 4)
data <- cbind(data, as.data.frame(oneb_time))

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
form <- paste("casos ~ 1 + ob_time1 + ob_time2 + ob_time3 + ob_time4",
              re_s, re_w, sep = " + ")
mod <- fit_models(as_GHRformulas(form), 
                  data, 
                  family = "nbinomial",
                  offset = "pop",
                  name = "mod", 
                  control_compute = list(config = FALSE, vcov = TRUE))
op_pred <- crosspred_inla(mod, oneb_time, mod_id = "mod1")
plot_coef_crosspred(op_pred, type = "slices", var = 1:800)
