# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## DATA PREPARATION ============================================================

# Description:
#     Merge Disease, Climate, Socioeconomic and Environmental Data
#     Add selected Lagged Values for Selected Climatic Variables
# 
# Script authors:
#     Carles Milà  (carles.milagarcia@bsc.es)
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Rachel Lowe     (rachel.lowe@bsc.es)
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

## 1. Setup ----------------------------

# load packages
packages <- c("dplyr", "tidyr", "lubridate", "zoo")
lapply(packages, library, character.only = TRUE)

# local packages
source("R/00_functions.R") # Maybe I can remove once I incorporate GHRmodel

# 2. Dengue data----------------------------------------------------

# read in raw weekly dengue data
dengue_week <- read.csv("data/raw-challenge/dengue.csv") |> 
  select(-macroregional_geocode)
glimpse(dengue_week)

# create a matching matrix between municipality code and health region code
matching_matrix <- dengue_week |>
  select(geocode, regional_geocode) |>
  distinct(geocode, regional_geocode)

# Aggregate weekly dengue cases from municipality to health region level
dengue_week <- dengue_week |>
  group_by(uf, regional_geocode, epiweek) |>
  summarise(casos = sum(casos), across(-c(geocode), first))
glimpse(dengue_week)

# Retrieve month and year from the 4th day of each week of weekly dengue cases
dengue_week <- dengue_week |>
  mutate(month = month(ymd(date) + 3),
         year = year(ymd(date) + 3))

# Define epi_year (starting at epiweek 41)
dengue_week <- dengue_week |>
  mutate(epi_year = year - 2009) |>
  mutate(epi_year = ifelse(as.numeric(substr(epiweek,5,6)) <= 40,
         epi_year, epi_year + 1 ))

# 3. Generated climate covs -----

# read in raw monthly climatic data and merge to dengue data
clim_data <- read.csv("data/raw-monthly/brazil_monthly_hist_200901-202504.csv")
clim_data <- clim_data |>
  select(-adm_name) |> 
  rename(regional_geocode = adm_id)
glimpse(clim_data)

# Scale tas and plrl variables
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
         plrl3 = rollsum(prlr, 3, fill = NA, align = "right"),
         tas6 = rollmean(tas, 6, fill = NA, align = "right"),
         tasmin6 = rollmean(tasmin, 6, fill = NA, align = "right"),
         tasmax6 = rollmean(tasmax, 6, fill = NA, align = "right"),
         plrl6 = rollsum(prlr, 6, fill = NA, align = "right"),
         tas12 = rollmean(tas, 12, fill = NA, align = "right"),
         tasmin12 = rollmean(tasmin, 12, fill = NA, align = "right"),
         tasmax12 = rollmean(tasmax, 12, fill = NA, align = "right"),
         plrl12 = rollsum(prlr, 12, fill = NA, align = "right"),
         nino6 = rollmean(nino, 6, fill = NA, align = "right"),
         nino12 = rollmean(nino, 12, fill = NA, align = "right"))

# create nino year based on epi_year
nino_year <- clim_data |> 
  mutate(epi_year = year - 2009) |>
  mutate(epi_year = ifelse(month >=10, epi_year + 1, epi_year)) |>
  filter(!c(year == 2024 & month >2)) |>
  group_by(epi_year) |>
  summarise(nino = mean(oni, na.rm = TRUE)) |>
  mutate(nino_year = case_when(
    nino < -0.5 ~ "Nina",
    nino >= -0.5 & nino <= 0.5 ~ "Neutral",
    nino > 0.5 ~ "Nino")) |>
  select(nino_year,epi_year)

# 4. Lags ----
clim_data$date <- with(clim_data, ymd(paste0(year, "-", month, "-01")))
glimpse(clim_data)
vlag <- setdiff(names(clim_data),
                c("month", "year", "regional_geocode", "regional_name", "date"))
clim_data <- lag_cov(data = clim_data,
                     time = "date",
                     group = "regional_geocode",
                     name = vlag,
                     lag = c(1, 3, 6),
                     add = TRUE)
clim_data$date <- NULL


# 5. Challenge covs ----

# read environ data and tranform them into health region resolution
env_data <- read.csv("data/raw-challenge/environ_vars.csv")
env_data <- env_data |>
  left_join(matching_matrix, by = "geocode") |>
  group_by(regional_geocode) |>
  summarise(biome = mode(biome),
            koppen = mode(koppen))
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
  left_join(nino_year, by = c("epi_year"))
glimpse(data_week)

# 7. Write final dataset ----
write.csv(data_week, file = "data/processed/weekly_data.csv")
