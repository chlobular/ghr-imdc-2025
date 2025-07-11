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
# data <- data[data$year >= 2023,] # For debugging only

# More koppen indices
data$koppen_id2 <- data$koppen_id
data$koppen_id3 <- data$koppen_id
data$koppen_id4 <- data$koppen_id


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

# Table of covariates
temp_form <- data.frame(v1 = c("tas6", "tas6.l1"),
                        v2 = c("tasan6", "tasan6.l1"))
prec_form <- data.frame(v3 = c("spei12.l2", "spei12.l3", "spei12.l3", 
                               "spei12", "spei12.l1"),
                        v4 = c("spei3", "spei3", "spei3.l1", "", ""))
idx <- expand.grid(i = seq_len(nrow(temp_form)), j = seq_len(nrow(prec_form)))
covs <- Map(function(i, j) cbind(temp_form[i, , drop = FALSE],
                                 prec_form[j, , drop = FALSE]),
            idx$i, idx$j)
covs <- do.call(rbind, covs) |> 
  arrange(v1, v3)
write.csv(covs, row.names = FALSE, "output/multi_fe/covs.csv")

# Random slopes per region
forms <- paste(unlist(cov_varying(covs$v1, pattern = "", unit = "koppen_id")), 
               unlist(cov_varying(covs$v2, pattern = "", unit = "koppen_id2")), 
               unlist(cov_varying(covs$v3, pattern = "", unit = "koppen_id3")), 
               sep=" + ")
for(i in 1:nrow(covs)){
  if(covs$v4[i] != ""){
    forms[i] <- paste(forms[i],
                      cov_varying(covs$v4[i], pattern = "", unit = "koppen_id4"), 
                      sep = " + ")
  }
}
covs$form <- write_FE_form(forms)

# 4. Fit models ----
sprint_mod(covs$form, data, "output/multi_fe", "multi")
