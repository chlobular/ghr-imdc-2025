# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## DATA PREPARATION ============================================================

# Description:
#     Merge Disease and climate data
#     Create the three validation datasets as well as the forecast dataset
#     Add selected Lagged Values for Selected Climatic Variables
# 
# Script authors:
#     Carles Milà  (carles.milagarcia@bsc.es)
#     Chloe Fletcher  (chloe.fletcher@bsc.es)
#
# Env: HUB with module R-bundle-CRAN/2024.06-foss-2023b
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

## 1. Setup ----------------------------

# load packages
library("dplyr")
library("tidyr")
library("lubridate")
library("zoo")
library("sf")
library("spdep")
library("GHRmodel")
sf::sf_use_s2(FALSE)

# Extra functions
source("R/00_functions.R")


# 2. Dengue data ----------------------------------------------------

# read in raw weekly dengue data, add extra vars
dengue_week <- read.csv("data/raw-challenge/dengue.csv") |> 
  select(-macroregional_geocode) |> 
  mutate(date = as.Date(date))

# Fill in values until EW40 2025
dengue_fill <- dengue_week[c("geocode", "uf", "regional_geocode")]
dengue_fill <- dengue_fill[!duplicated(dengue_fill),]
dengue_dates <- data.frame(date = seq(max(dengue_week$date)+7,  as.Date("2025-09-28"), "1 week"),
                           epiweek = seq(202517+1, 202540))
dengue_fill <- tidyr::crossing(dengue_fill, dengue_dates)
dengue_fill$casos <- NA
dengue_fill$train_1 <- "False"
dengue_fill$train_2 <- "False"
dengue_fill$train_3 <- "False"
dengue_fill$target_1 <- "False"
dengue_fill$target_2 <- "False"
dengue_fill$target_3 <- "True"

# Concatenate
dengue_week <- bind_rows(dengue_week, dengue_fill)
sapply(dengue_week, function(x) sum(is.na(x)))
rm("dengue_fill", "dengue_dates")

# Spatial IDs
dengue_week <- dengue_week |> 
  mutate(hr_id = as.numeric(factor(regional_geocode)), # Health regions
         state_id = as.numeric(factor(uf)), # States
         region = as.numeric(substr(regional_geocode, 1, 1)), # Top health regions
         region_id = as.numeric(factor(region))) 

# Temporal IDs
dengue_week <- dengue_week |>
  mutate(time_id = as.numeric(as.factor(epiweek)), # Continuous time index
         week = as.numeric(substr(epiweek, 5, 6)), # Epi weeks
         week = ifelse(week == 53, 52, week),
         week_id = ifelse(week >= 41, week - 40, week + 12), # Weeks within season
         month = month(ymd(date) + 3),
         month_id = month,
         year = year(ymd(date) + 3)) |>
  mutate(epiyear = year) |>
  mutate(epiyear = ifelse(as.numeric(substr(epiweek, 5, 6)) <= 40, # Epi years
                           epiyear, epiyear + 1),
         year_id = as.numeric(factor(epiyear)))

# define datasets - missing forecast dataset, to be added
dengue_week <- dengue_week |> 
  mutate(
    validation_1 = case_when(train_1 == "True" ~ "Train",
                             target_1 == "True" ~ "Test",
                             epiweek < 202340 ~ "Hold",
                             .default = "Out"),
    validation_2 = case_when(train_2 == "True" ~ "Train",
                             target_2 == "True" ~ "Test",
                             epiweek < 202440 ~ "Hold",
                             .default = "Out"),
    validation_3 = case_when(train_3 == "True" ~ "Train",
                             target_3 == "True" ~ "Test",
                             epiweek < 202540 ~ "Hold",
                             .default = "Out"))

# create a matching matrix between municipality code and health region code
matching_matrix <- dengue_week |>
  select(geocode, regional_geocode) |>
  distinct(geocode, regional_geocode)

# Aggregate weekly dengue cases from municipality to health region level
dengue_week <- dengue_week |>
  group_by(uf, regional_geocode, epiweek) |>
  summarise(casos = sum(casos), across(-c(geocode), first))
glimpse(dengue_week)
sapply(dengue_week, function(x) sum(is.na(x)))
gc()

# Create datasets
val1 <- dengue_week[dengue_week$validation_1!="Out",] |> 
  select(-ends_with("_2"), -ends_with("_3"))
val2 <- dengue_week[dengue_week$validation_2!="Out",] |> 
  select(-ends_with("_1"), -ends_with("_3"))
val3 <- dengue_week[dengue_week$validation_3!="Out",]|> 
  select(-ends_with("_1"), -ends_with("_2"))


# 3. Climate covariates -----

# read in raw monthly climatic data and merge to dengue data
clim1 <- read.csv("data/raw-monthly/clim_validation1.csv") |> 
  mutate(date = as.Date(date))
clim2 <- read.csv("data/raw-monthly/clim_validation2.csv") |> 
  mutate(date = as.Date(date))
clim3 <- read.csv("data/raw-monthly/clim_validation3.csv") |> 
  mutate(date = as.Date(date))

# Scale tas6
tas6_mean <- mean(clim1$tas6[clim1$year <= 2021])
tas6_sd <- sd(clim1$tas6[clim1$year <= 2021])
clim1$tas6 <- (clim1$tas6 - tas6_mean)/tas6_sd
clim2$tas6 <- (clim2$tas6 - tas6_mean)/tas6_sd
clim3$tas6 <- (clim3$tas6 - tas6_mean)/tas6_sd

# 4. Lags ----

# tas6.l1
clim1 <- lag_cov(data = clim1, time = "date", group = "regional_geocode",
                 name = "tas6", lag = 1, add = TRUE)
clim2 <- lag_cov(data = clim2, time = "date", group = "regional_geocode",
                 name = "tas6", lag = 1, add = TRUE)
clim3 <- lag_cov(data = clim3, time = "date", group = "regional_geocode",
                 name = "tas6", lag = 1, add = TRUE)

# oni.l6
clim1 <- lag_cov(data = clim1, time = "date", group = "regional_geocode",
                 name = "oni", lag = 6, add = TRUE)
clim2 <- lag_cov(data = clim2, time = "date", group = "regional_geocode",
                 name = "oni", lag = 6, add = TRUE)
clim3 <- lag_cov(data = clim3, time = "date", group = "regional_geocode",
                 name = "oni", lag = 6, add = TRUE)

# tasan6.l1
clim1 <- lag_cov(data = clim1, time = "date", group = "regional_geocode",
                 name = "tasan6", lag = 1, add = TRUE)
clim2 <- lag_cov(data = clim2, time = "date", group = "regional_geocode",
                 name = "tasan6", lag = 1, add = TRUE)
clim3 <- lag_cov(data = clim3, time = "date", group = "regional_geocode",
                 name = "tasan6", lag = 1, add = TRUE)

# spei3.l1
clim1 <- lag_cov(data = clim1, time = "date", group = "regional_geocode",
                 name = "spei3", lag = 1, add = TRUE)
clim2 <- lag_cov(data = clim2, time = "date", group = "regional_geocode",
                 name = "spei3", lag = 1, add = TRUE)
clim3 <- lag_cov(data = clim3, time = "date", group = "regional_geocode",
                 name = "spei3", lag = 1, add = TRUE)

# spei12.l3
clim1 <- lag_cov(data = clim1, time = "date", group = "regional_geocode",
                 name = "spei12", lag = 3, add = TRUE)
clim2 <- lag_cov(data = clim2, time = "date", group = "regional_geocode",
                 name = "spei12", lag = 3, add = TRUE)
clim3 <- lag_cov(data = clim3, time = "date", group = "regional_geocode",
                 name = "spei12", lag = 3, add = TRUE)

# Clean
clim1 <- select(clim1, -tas6, -oni, -tasan6, -spei3, -spei12, -date)
sapply(clim1, function(x) sum(is.na(x)))
clim2 <- select(clim2, -tas6, -oni, -tasan6, -spei3, -spei12, -date)
sapply(clim2, function(x) sum(is.na(x)))
clim3 <- select(clim3, -tas6, -oni, -tasan6, -spei3, -spei12, -date)
sapply(clim3, function(x) sum(is.na(x)))


# 5. Challenge covs ----

# read environ data and tranform them into health region resolution
env_data <- read.csv("data/raw-challenge/environ_vars.csv")
env_data <- env_data |>
  left_join(matching_matrix, by = "geocode") |>
  group_by(regional_geocode) |>
  summarise(biome = names(which.max(table(biome))),
            koppen = names(which.max(table(koppen)))) |> 
  ungroup() |> 
  mutate(biome_id = as.numeric(factor(biome)),
         koppen_id = as.numeric(factor(koppen)))
glimpse(env_data)

# read in pop data and tranform them into health region resolution
pop_data <- read.csv("data/raw-challenge/datasus_population_2001_2024.csv")

pop_data <- pop_data |> 
  filter(year >=2010) |>
  left_join(matching_matrix, by = "geocode") |>
  group_by(regional_geocode, year) |>
  summarise(pop = sum(population)) 
pop_data <- pop_data[complete.cases(pop_data),]
glimpse(pop_data)

# Assign pop data for 2025 from 2024
pop_data_2025 <- pop_data |> 
  filter(year == 2024) |>
  mutate(year = 2025)
pop_data <- rbind(pop_data, pop_data_2025)


# 6. Merge ----
val1 <- val1 |>
  left_join(clim1, by = c("regional_geocode", "month", "year")) |>
  left_join(env_data, by = c("regional_geocode")) |>
  left_join(pop_data, by = c("regional_geocode", "year")) 
glimpse(val1)
sapply(val1, function(x) sum(is.na(x)))

val2 <- val2 |>
  left_join(clim2, by = c("regional_geocode", "month", "year")) |>
  left_join(env_data, by = c("regional_geocode")) |>
  left_join(pop_data, by = c("regional_geocode", "year")) 
glimpse(val2)
sapply(val2, function(x) sum(is.na(x)))

val3 <- val3 |>
  left_join(clim3, by = c("regional_geocode", "month", "year")) |>
  left_join(env_data, by = c("regional_geocode")) |>
  left_join(pop_data, by = c("regional_geocode", "year")) 
glimpse(val3)
sapply(val3, function(x) sum(is.na(x)))

# 7. Arrange and prepare graph ----
val1 <- arrange(val1, regional_geocode, date)
val2 <- arrange(val2, regional_geocode, date)
val3 <- arrange(val3, regional_geocode, date)

boundaries <- read_sf("data/boundaries/shape_regional_health.gpkg")
boundaries <- boundaries[boundaries$regional_geocode %in% val1$regional_geocode,]
boundaries <- arrange(boundaries, regional_geocode)
nb <- poly2nb(boundaries)
g <- nb2mat(nb, style = "B")

# 9. Write final dataset, clean boundaries, and graph ----
write.csv(val1, row.names = FALSE, file = "data/processed/dataset_val1.csv")
write.csv(val2, row.names = FALSE, file = "data/processed/dataset_val2.csv")
write.csv(val3, row.names = FALSE, file = "data/processed/dataset_val3.csv")
saveRDS(g, file = "data/processed/graph_predict.rds")
