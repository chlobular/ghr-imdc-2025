# 0. Setup ----

# Load packages
library("dplyr")
library("INLA")
library("spdep")
library("GHRmodel") 
library("GHRpredict") 

# 1. Priors, graph, utils ----

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# Graph
g <- readRDS("data/processed/graph.rds")

# Utils
source("R/00_functions.R")

# Read data
data <- read.csv("data/processed/weekly_data.csv")
data$date <- as.Date(data$date)
data <- data[data$year >= 2023,] # For debugging only

# 1. Priors, graph, utils ----

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# Graph
g <- readRDS("data/processed/graph.rds")

# Utils
source("R/00_functions.R")

# Formula
re_w <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
              "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
form <- paste("casos ~ 1 + oni.l3", re_w, sep = " + ")
form <- as_GHRformulas(form)
  
# Validation 3
data3 <- data[data$dataset_3!="Out",]
limit3 <- max(data3[data3$dataset_3 == "Train", "date"])
form
mod_3 <- ghr_predict(formula = form,
                     data = data3,
                     family = "nbinomial",
                     offset = "pop",
                     spatial_unit = "hr_id", 
                     control_strategy = list(fixed_until = limit3),
                     nsamples = 10)

