# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fixed effects: trend =====================================================
# 
# Description:
#     Selection of temporal trend
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
library("splines")

# Local packages in MN5 
library("GHRmodel", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/")

# Relative paths in MN5 
setwd("/gpfs/scratch/bsc32/bsc498895/sprint2025")

# Read data
data <- read.csv("data/processed/weekly_data.csv")

# More indices
data$koppen_id2 <- data$koppen_id
data$koppen_id3 <- data$koppen_id
data$koppen_id4 <- data$koppen_id
data$koppen_id5 <- data$koppen_id
data$koppen_id6 <- data$koppen_id
data$koppen_id7 <- data$koppen_id
data$koppen_id8 <- data$koppen_id
data$koppen_id9 <- data$koppen_id
data$koppen_id10 <- data$koppen_id
data$region_id2 <- data$region_id
data$region_id3 <- data$region_id
data$biome_id2 <- data$biome_id
data$biome_id3 <- data$biome_id

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
re_s <- paste("f(hr_id, model = 'bym2', graph = g,",
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
form_base <- paste("", fe_base, re_s, re_w, re_y, sep = " + ")

# NS
NS2 <- ns(data$time_id, df = 2)
names(NS2) <- c("NS2_1", "NS2_2")
saveRDS(NS2, "output/trend_fe/ns2.rds")

NS3 <- ns(data$time_id, df = 3)
names(NS3) <- c("NS3_1", "NS3_2", "NS3_3")
saveRDS(NS3, "output/trend_fe/ns3.rds")

data <- cbind(data, NS2, NS3)


# Linear
f_linear <- paste(form_base, "time_id", sep = " + ")
f_linear_koppen <- paste(form_base, 
                         cov_varying("time_id", pattern = "", unit = "koppen_id8")[[1]],
                         sep = " + ")
f_linear_region <- paste(form_base, 
                         cov_varying("time_id", pattern = "", unit = "region_id")[[1]],
                         sep = " + ")
f_linear_biome <- paste(form_base, 
                        cov_varying("time_id", pattern = "", unit = "biome_id")[[1]],
                        sep = " + ")

# ns2
f_ns2 <- paste(form_base, "NS2_1", "NS2_2", sep = " + ")
f_ns2_koppen <- paste(form_base, 
                      cov_varying("NS2_1", pattern = "", unit = "koppen_id8")[[1]],
                      cov_varying("NS2_2", pattern = "", unit = "koppen_id9")[[1]],
                      sep = " + ")
f_ns2_region <- paste(form_base, 
                      cov_varying("NS2_1", pattern = "", unit = "region_id")[[1]],
                      cov_varying("NS2_2", pattern = "", unit = "region_id2")[[1]],
                      sep = " + ")
f_ns2_biome <- paste(form_base, 
                     cov_varying("NS2_1", pattern = "", unit = "biome_id")[[1]],
                     cov_varying("NS2_2", pattern = "", unit = "biome_id2")[[1]],
                     sep = " + ")

# ns3
f_ns3 <- paste(form_base, "ns3_1", "ns3_2", "ns3_3", sep = " + ")
f_ns3_koppen <- paste(form_base, 
                      cov_varying("NS3_1", pattern = "", unit = "koppen_id8")[[1]],
                      cov_varying("NS3_2", pattern = "", unit = "koppen_id9")[[1]],
                      cov_varying("NS3_3", pattern = "", unit = "koppen_id10")[[1]],
                      sep = " + ")
f_ns3_region <- paste(form_base, 
                      cov_varying("NS3_1", pattern = "", unit = "region_id")[[1]],
                      cov_varying("NS3_2", pattern = "", unit = "region_id2")[[1]],
                      cov_varying("NS3_3", pattern = "", unit = "region_id3")[[1]],
                      sep = " + ")
f_ns3_biome <- paste(form_base, 
                     cov_varying("NS3_1", pattern = "", unit = "biome_id")[[1]],
                     cov_varying("NS3_2", pattern = "", unit = "biome_id2")[[1]],
                     cov_varying("NS3_3", pattern = "", unit = "biome_id3")[[1]],
                     sep = " + ")
form_all <- c(form_base, f_linear, f_linear_koppen, f_linear_region, f_linear_biome,
              f_ns2, f_ns2_koppen, f_ns2_region, f_ns2_biome,
              f_ns3, f_ns3_koppen, f_ns3_region, f_ns3_biome)

modtab <- data.frame(Models = c("Baseline", rep("Linear", 4), 
                                rep("NS2", 4), rep("NS3", 4)),
                     Replication = c("Baseline", 
                                     rep(c("None", "Koppen", "Region", "Biome"), 3)),
                     Formula = form_all)
modtab
write.csv(modtab, "output/trend_fe/modtab.csv", row.names = FALSE)

# 4. Fit models ----

## Without replicated spatial random effect
sprint_mod(modtab$Formula, data, "output/trend_fe", "trend")
