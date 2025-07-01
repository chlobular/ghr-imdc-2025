# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Random effects: combinations =================================================
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

# Read data, keep relevant columns for RE estimation
data <- read.csv("data/processed/weekly_data.csv")
# data <- data[data$year >= 2020,] # For debugging only

data <- dplyr::select(data,
                      -starts_with("tas"), -starts_with("prlr"),
                      -starts_with("spi"), -starts_with("spei"),
                      -starts_with("nino"), -starts_with("oni"),
                      "nino_id") |> 
  mutate(date = as.Date(date)) 
glimpse(data)

# 1. Formulas, priors, graph, utils ----

# Formulas
source("R/00_RE_formuni.R")

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# Graph
g <- readRDS("data/processed/graph.rds")

# Utils
source("R/00_functions.R")


# 2. Formulas ----

# Combinations of RE
spatial <- all_s[c(1, 3)]
weekly <- all_w[c(4,6,10)]
yearly <- all_y[c(6,15)]
combined_df <- expand.grid(names(spatial), names(weekly), names(yearly)) |> 
  mutate(Model = paste0("mod", sprintf("%02d", 1:12))) |> 
  select(Model, Var1, Var2, Var3) |> 
  mutate(across(everything(), as.character)) |> 
  rename(Spatial = Var1, Weekly = Var2, Yearly = Var3) |> 
  group_by(Model)
combined_df$Nino <- ""

# Adding final candidates
final_df <- data.frame(Model = paste0("mod", 13:15),
                       Spatial = c("re_s03", "re_s01", "re_s01"),
                       Weekly = c("re_w04", "re_w04", "re_w04"),
                       Yearly = c("re_y01", "re_y01", ""),
                       Nino = c("", "re_n01", "re_n01"))

# Writing formulas
combined_df <- rbind(combined_df, final_df)
combined_df$form <- sapply(1:nrow(combined_df), function(i){
  paste0(all_s[combined_df$Spatial[i]], all_w[combined_df$Weekly[i]], 
         all_y[combined_df$Yearly[i]], all_n[combined_df$Nino[i]])
})
combined_df$form <- gsub("NA", "", combined_df$form)
combined_df
write.csv(combined_df, "output/multi_re/models.csv", row.names = FALSE)


# 2. Fit models ----
for(i in 13:nrow(combined_df)){ 
  
  print(combined_df[i,])
  form_i <- as.character(combined_df[i,"form"])
  names(form_i) <- as.character(combined_df[i,"Model"])
  sprint_mod(form_i, data, "output/multi_re/", create_dir = T)
}
