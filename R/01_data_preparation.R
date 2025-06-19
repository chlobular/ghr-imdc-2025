# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## DATA PREPARATION ============================================================

# Description:
#     Merge Disease, Climate, Socioeconomic and Environmental Data
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
sf::sf_use_s2(FALSE)

# local package
# library("GHRmodel")

# Extra functions
source("R/00_functions.R")


# 2. Dengue data ----------------------------------------------------

# read in raw weekly dengue data, add extra vars
dengue_week <- read.csv("data/raw-challenge/dengue.csv") |> 
  select(-macroregional_geocode)

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

# define datasets and train/test sets
dengue_week <- dengue_week |> 
  mutate(
    dataset_01 = case_when(epiweek <= 202025 ~ "Train",
                           epiweek <= 202040 ~ "Hold",
                           epiweek <= 202140 ~ "Test",
                           .default = "Out"),
    dataset_02 = case_when(epiweek <= 202125 ~ "Train",
                           epiweek <= 202140 ~ "Hold",
                           epiweek <= 202240 ~ "Test",
                           .default = "Out"),
    dataset_1 = case_when(train_1 == "True" ~ "Train",
                          target_1 == "True" ~ "Test",
                          epiweek < 202340 ~ "Hold",
                          .default = "Out"),
    dataset_2 = case_when(train_2 == "True" ~ "Train",
                          target_2 == "True" ~ "Test",
                          epiweek < 202440 ~ "Hold",
                          .default = "Out"),
    dataset_3 = case_when(train_3 == "True" ~ "Train",
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


# 3. Climate covariates -----

# read in raw monthly climatic data and merge to dengue data
clim_data <- read.csv("data/raw-monthly/brazil_monthly_hist_200801-202505.csv")
clim_data <- clim_data |>
  select(-adm_name) |> 
  rename(regional_geocode = adm_id)
glimpse(clim_data)

# Scale tas and prlr variables
vars_scale <- c("tas", "tasmin", "tasmax","prlr")
vars_scaled <- scale(clim_data[,c(vars_scale)])
scale_params <- list("mu" = attributes(vars_scaled)[[3]],
                     "sigma"  = attributes(vars_scaled)[[4]])
saveRDS(scale_params, file = "data/processed/scale_params.rds")
clim_data[,c(vars_scale)] <- vars_scaled
summary(clim_data)  

# create moving averages of med, min, max temp, oni and nino
clim_data <- clim_data |>
  group_by(regional_geocode) |>
  arrange(year, month) |>
  mutate(tas3 = rollmean(tas, 3, fill = NA, align = "right"),
         tasmin3 = rollmean(tasmin, 3, fill = NA, align = "right"),
         tasmax3 = rollmean(tasmax, 3, fill = NA, align = "right"),
         prlr3 = rollsum(prlr, 3, fill = NA, align = "right"),
         tas6 = rollmean(tas, 6, fill = NA, align = "right"),
         tasmin6 = rollmean(tasmin, 6, fill = NA, align = "right"),
         tasmax6 = rollmean(tasmax, 6, fill = NA, align = "right"),
         prlr6 = rollsum(prlr, 6, fill = NA, align = "right"),
         tas12 = rollmean(tas, 12, fill = NA, align = "right"),
         tasmin12 = rollmean(tasmin, 12, fill = NA, align = "right"),
         tasmax12 = rollmean(tasmax, 12, fill = NA, align = "right"),
         prlr12 = rollsum(prlr, 12, fill = NA, align = "right"),
         nino6 = rollmean(nino, 6, fill = NA, align = "right"),
         nino12 = rollmean(nino, 12, fill = NA, align = "right")) |> 
  ungroup()

# create nino year based on epiyear
nino_year <- clim_data |> 
  mutate(epiyear = year) |>
  mutate(epiyear = ifelse(month < 10, epiyear, epiyear + 1)) |> 
  group_by(epiyear) |>
  summarise(nino = mean(oni, na.rm = TRUE)) |>
  mutate(nino_year = case_when(
    nino < -0.5 ~ "Nina",
    nino >= -0.5 & nino <= 0.5 ~ "Neutral",
    nino > 0.5 ~ "Nino")) |>
  select(nino_year, epiyear) |> 
  mutate(nino_id = as.numeric(factor(nino_year)))

# 4. Lags ----
clim_data$date <- with(clim_data, ymd(paste0(year, "-", month, "-01")))
glimpse(clim_data)
vlag <- setdiff(names(clim_data),
                c("month", "year", "regional_geocode", "regional_name", "date"))
clim_data <- lag_cov(data = clim_data,
                     time = "date",
                     group = "regional_geocode",
                     name = vlag,
                     lag = 1:6,
                     add = TRUE)
clim_data$date <- NULL


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

# Merge weekly dengue cases with covariates by region, year and month
data_week <- dengue_week |>
  left_join(clim_data, by = c("regional_geocode", "month", "year")) |>
  left_join(env_data, by = c("regional_geocode")) |>
  left_join(pop_data, by = c("regional_geocode", "year")) |>
  left_join(nino_year, by = c("epiyear"))
glimpse(data_week)

# 7. Arrange  ----
data_week <- arrange(data_week, regional_geocode, date)
glimpse(data_week)

# 8. Prepare graph
boundaries <- read_sf("data/boundaries/shape_regional_health.gpkg")
boundaries <- boundaries[boundaries$regional_geocode %in% data_week$regional_geocode,]
boundaries <- arrange(boundaries, regional_geocode)
nb <- poly2nb(boundaries)
g <- nb2mat(nb, style = "B")
# coords <- st_coordinates(st_centroid(st_geometry(boundaries)))
# plot(nb, coords, col="grey")

# 7. Write final dataset, clean boundaries, and graph ----
write.csv(data_week, row.names = FALSE, file = "data/processed/weekly_data.csv")
write_sf(boundaries, "data/boundaries/clean_hr.gpkg")
saveRDS(g, file = "data/processed/graph.rds")