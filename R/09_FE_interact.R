# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fixed effects: interactions ==================================================
# 
# Description:
#     Selection of interactions of climatic effects (LSL)
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
re_s <- paste("f(hr_id, model = 'bym2', replicate = nino_id, graph = g,",
              "scale.model = TRUE, hyper = precision.prior, constr = TRUE)")
re_w <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
              "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_y <- "f(year_id, model = 'iid', hyper = precision.prior, constr = TRUE)"


# 3. FE ----

# Table of candidates 
candtab1 <- data.frame(Model = 1:5,
                       cov1 = "v1", cov2 = "v2", cov3 = "v3",
                       cov4 = c("", "v1v2", "v1v2", "v1v3", "v2v3"),
                       cov5 = c("" , "v1v3", "", "", ""),
                       cov6 = c("" , "v2v3", "", "", ""),
                       cov7 = c("", "v1v2v3", "", "", ""),
                       cov8 = c("", "", "", "", ""))
candtab2 <- data.frame(Model = 6:10,
                       cov1 = "v1", cov2 = "v2", cov3 = "v3",
                       cov4 = c("tas6.l1", "v1v2", "v1v2", "v1v3", "v2v3"),
                       cov5 = c("" , "v1v3", "tas6.l1", "tas6.l1", "tas6.l1"),
                       cov6 = c("", "v2v3", "", "", ""),
                       cov7 = c("", "v1v2v3", "", "", ""),
                       cov8 = c("", "tas6.l1", "", "", ""))
candtab <- rbind(candtab1, candtab2)
candtab

# Formulas
candtab$forms <- ""
for(i in 1:nrow(candtab)){
  
  # First three, all have them
  candtab$forms[i] <- paste(cov_varying(candtab$cov1[i], pattern = "", unit = "koppen_id"), 
                            cov_varying(candtab$cov2[i], pattern = "", unit = "koppen_id2"), 
                            cov_varying(candtab$cov3[i], pattern = "", unit = "koppen_id3"), 
                            sep = " + ")
  
  # Four to eight
  if(candtab$cov4[i] != ""){
    candtab$forms[i] <- paste(candtab$forms[i],
                              cov_varying(candtab$cov4[i], pattern = "", unit = "koppen_id4"),
                              sep = " + ")
  }
  if(candtab$cov5[i] != ""){
    candtab$forms[i] <- paste(candtab$forms[i],
                              cov_varying(candtab$cov5[i], pattern = "", unit = "koppen_id5"),
                              sep = " + ")
  }
  if(candtab$cov6[i] != ""){
    candtab$forms[i] <- paste(candtab$forms[i],
                              cov_varying(candtab$cov6[i], pattern = "", unit = "koppen_id6"),
                              sep = " + ")
  }
  if(candtab$cov7[i] != ""){
    candtab$forms[i] <- paste(candtab$forms[i],
                              cov_varying(candtab$cov7[i], pattern = "", unit = "koppen_id7"),
                              sep = " + ")
  }
  if(candtab$cov8[i] != ""){
    candtab$forms[i] <- paste(candtab$forms[i],
                              cov_varying(candtab$cov8[i], pattern = "", unit = "koppen_id8"),
                              sep = " + ")
  }
}

candtab$forms <- paste("", candtab$forms, re_s, re_w, re_y, sep = " + ")
write.csv(candtab, "output/inter_fe/modtab.csv", row.names = FALSE)

# 4. Fit models ----
candtab$forms
sprint_mod(candtab$forms, data, "output/inter_fe", "inter")
