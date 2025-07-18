# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fixed effects: multivariate =====================================================
# 
# Description:
#     Selection of multivariate ONI effects
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
re_s1 <- paste("f(hr_id, model = 'bym2', graph = g,",
               "scale.model = TRUE, hyper = precision.prior, constr = TRUE)")
re_s2 <- paste("f(hr_id, model = 'bym2', replicate = nino_id, graph = g,",
              "scale.model = TRUE, hyper = precision.prior, constr = TRUE)")
re_w <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
              "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_y <- "f(year_id, model = 'iid', hyper = precision.prior)"


# 3. FE ----

# Baseline model
fe_base <- paste(cov_varying("v1", "koppen_id", ""),
                 cov_varying("v2", "koppen_id2", ""),
                 cov_varying("v3", "koppen_id3", ""),
                 cov_varying("v1v2", "koppen_id4", ""),
                 cov_varying("v1v3", "koppen_id5", ""),
                 cov_varying("v2v3", "koppen_id6", ""),
                 cov_varying("v1v2v3", "koppen_id7", ""),
                 sep = " + ")
fe_base <- paste0(" + ", fe_base)

# Linear
linear <- paste0("oni.l", 1:6)

# Non-linear, equal
nl_equal <- cov_nl(linear, pattern = "", method = "cut")[[1]]

# Non-linear, quantile
nl_quant <- cov_nl(linear, pattern = "", n = 8)[[1]]

# Random slopes - koppen
linear_koppen <- cov_varying(linear, pattern = "", unit = "koppen_id8")[[1]]

# Random slopes - region
linear_region <- cov_varying(linear, pattern = "", unit = "region_id")[[1]]

# Random slopes - biome
linear_biome <- cov_varying(linear, pattern = "", unit = "biome_id")[[1]]

forms <- c(fe_base, 
           paste(fe_base, 
                 c(linear, nl_equal, nl_quant, linear_koppen, linear_region, linear_biome),
                 sep = " + "))

modtab <- data.frame(ONI = c("Baseline", rep(linear, 6)),
                     Model = c("Baseline",
                               rep("Linear", 6), rep("NL - equal", 6), rep("NL - quant", 6),
                               rep("Rep. koppen", 6), rep("Rep. region", 6),
                               rep("Rep. biome", 6)))
write.csv(modtab, "output/oni_fe/modtab.csv", row.names = FALSE)


# 4. Fit models ----

## Without replicated spatial random effect
forms1 <- paste(forms, re_s1, re_w, re_y, sep = " + ")
sprint_mod(forms1, data, "output/oni_fe", "spatial_norep")

# ## With replicated spatial random effect
# forms2 <- paste(forms, re_s2, re_w, re_y, sep = " + ")
# sprint_mod(forms2, data, "output/oni_fe", "spatial_rep")