# 0. Setup ----

# Load packages
library("dplyr")
library("INLA")
library("spdep")
library("GHRmodel") # Comment if in MN5
library("GHRpredict") # Comment if in MN5

# Local packages in MN5 
# library("GHRmodel", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/")
# library("GHRpredict", lib.loc = "/gpfs/scratch/bsc32/bsc498895/libraries/")

# Relative paths in MN5 
# setwd("/gpfs/scratch/bsc32/bsc498895/sprint2025")

# Read data
data <- read.csv("data/processed/weekly_data.csv")
data$date <- as.Date(data$date)
data <- data[data$year >= 2023,] # For debugging only

# More koppen indices
data$koppen_id2 <- data$koppen_id
data$koppen_id3 <- data$koppen_id
data$koppen_id4 <- data$koppen_id


# 1. Priors, graph ----

# Priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# Graph
g <- readRDS("data/processed/graph.rds")


# 2. Formula ----
re_s <- paste("f(hr_id, model = 'bym2', replicate = nino_id, graph = g,",
              "scale.model = TRUE, hyper = precision.prior, constr = TRUE)")
re_w <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
              "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_y <- "f(year_id, model = 'iid', hyper = precision.prior, constr = TRUE)"

form <- paste(
  "casos ~ 1 ",
  cov_varying("tas6", pattern = "", unit = "koppen_id"),
  cov_varying("tasan6", pattern = "", unit = "koppen_id2"),
  cov_varying("spei3", pattern = "", unit = "koppen_id3"),
  cov_varying("spei12.l2", pattern = "", unit = "koppen_id4"),
  re_s, re_w, re_y,
  sep = " + "
)
form  

# 3. Fit ----
(limit <- as.Date(max(data[data$dataset_3 == "Train", "date"])))
pred_mod <- ghr_predict(formula = as_GHRformulas(form),
                        data = data,
                        family = "nbinomial",
                        offset = "pop",
                        spatial_unit = "hr_id", 
                        control_strategy = list(fixed_until = limit),
                        nsamples = 10) # CHANGE IN MN5

pred_stats <- prediction_stats(
  samples = pred_mod,
  data = data,
  temporal_unit = "week",           
  spatial_unit = "hr_id",
  control_stats = list(crps = TRUE, mae = TRUE, rmse = TRUE ))


summarise_spatial$crps$edf_crps_summary
summarise_spatial$mae$mae
summarise_spatial$rmse$rmse

# Compute CRPS for each spatial unit
# Extract row identifiers
prediction_IDs <- summarise_spatial$metadata$data

# Extract unique CRPS values
crps_values <- summarise_spatial$crps$edf_crps_values

# Add CRPS values to the identifiers
crps_district <- prediction_IDs |> 
  drop_na(id_out) |> 
  bind_cols(crps_values) |>
  pivot_longer(5, names_to = "model", values_to = "crps") |> 
  group_by(hr_id, model) |> 
  summarise(
    mean_crps = mean(crps),
    median_crps = median(crps),
    q0.025 = quantile(crps, 0.025),
    q0.975 = quantile(crps, 0.975)
  ) 
crps_district
