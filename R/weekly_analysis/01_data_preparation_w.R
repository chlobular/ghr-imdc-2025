# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## DATA PREPARATION ============================================================

# Description:
#     Merge Disease, Climate, Socioeconomic and Environmental Data
#     Add selected Lagged Values for Selected Climatic Variables

# Script authors:
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Dr Giovenale Moirano  (giovenale.moirano@bsc.es)
#     Prof. Rachel Lowe     (rachel.lowe@bsc.es)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

## Clear the environment
rm(list = ls())

## 1.Source packages and define customized functions----------------------------

# load packages
packages <- c("dplyr", "tidyr", "lubridate", "zoo")
lapply(packages, library, character.only = TRUE)

source("lsl_interaction_brazil/functions/00_functions.R")

## 2.Import dengue data----------------------------------------------------

# read in raw weekly dengue data
dengue_data_week <- read.csv("data/raw_data/raw_data_infodengue/dengue_week.csv")
glimpse(dengue_data_week)
dim(dengue_data_week)

# Retrieve month from the 4th day of each week of weekly dengue cases

dengue_data_week <- dengue_data_week %>%
  mutate(month = month(ymd(date) + 3))

# create a matching matrix between municipality code and health region code
matching_matrix <- dengue_data_week %>%
  select(geocode, regional_geocode) %>%
  distinct(geocode, regional_geocode)

## 3.Aggregate weekly dengue cases from municipality to health region level---

dengue_data_week <- dengue_data_week %>%
  group_by(regional_geocode, epiweek) %>%
  summarise(casos = sum(casos), across(-c(geocode), first))

# Define epi_year (From 1 to 15, starting at epiweek 41)

dengue_data_week <- dengue_data_week %>% mutate(epi_year = year - 2009) %>%
  mutate(epi_year = ifelse(as.numeric(substr(epiweek,5,6)) <= 40,
         epi_year, epi_year + 1 ))

## 4. Import climatic and environmental data and merge with dengue data -----
# read in raw monthly climatic data and merge to dengue data
clim_data <- read.csv("data/raw_data/raw_data_ghr/brazil_monthly_climate_updatedNino.csv")
clim_data <- clim_data %>%
  rename(regional_geocode = adm_id,
         regional_name = adm_name)
glimpse(clim_data)

# create 3 months moving average of med, min, max temp, oni and nino

temp3_m <- clim_data %>%
  group_by(regional_geocode) %>%
  arrange(year, month) %>%
  mutate(temp3_med_m = rollmean(temp_med_m, 3, fill = NA, align = "right"),
         temp3_min_m = rollmean(temp_min_m, 3, fill = NA, align = "right"),
         temp3_max_m = rollmean(temp_max_m, 3, fill = NA, align = "right"),
         nino34_6_m = rollmean(oni_m, 6, fill = NA, align = "right"),
         nino34_12_m = rollmean(nino34_m, 12, fill = NA, align = "right")) %>%
  select(regional_geocode, temp3_med_m, temp3_min_m, temp3_max_m, nino34_6_m, nino34_12_m,
         year, month) %>%
  arrange(regional_geocode, year, month)

# Scale temp_3m variables by the overall brazilian mean 
overall_med_3m <- mean(temp3_m$temp3_med_m, na.rm = TRUE)
overall_min_3m <- mean(temp3_m$temp3_min_m, na.rm = TRUE)
overall_max_3m <- mean(temp3_m$temp3_max_m, na.rm = TRUE)

temp3_m <- temp3_m  %>%
  mutate(temp3_med_m = temp3_med_m - overall_med_3m,
         temp3_min_m = temp3_med_m - overall_min_3m,
         temp3_max_m = temp3_med_m - overall_max_3m)

clim_data <- clim_data %>%
  left_join(temp3_m, by = c("regional_geocode", "year", "month"))

# create nino year based on epi_year
nino_year <- clim_data %>% 
  mutate(epi_year = year - 2009) %>%
  mutate(epi_year = ifelse(month >=10, epi_year + 1, epi_year)) %>%
  filter(!c(year == 2024 & month >2)) %>%
  group_by(epi_year) %>%
  summarise(nino = mean(oni_m, na.rm = TRUE)) %>%
  mutate(nino_year = case_when(
    nino < -0.5 ~ "Nina",
    nino >= -0.5 & nino <= 0.5 ~ "Neutral",
    nino > 0.5 ~ "Nino"
  )) %>% select(nino_year,epi_year)


## Lag selected climatic variable (vars)
clim_data$index_date <- paste0(clim_data$year, "-",
                               clim_data$month, "-01")
clim_data$index_date <- ymd(clim_data$index_date)
glimpse(clim_data)
var <- c("spi1_m", "spei1_m",
         "spi3_m", "spei3_m",
         "spi6_m", "spei6_m",
         "spi12_m", "spei12_m",
         "temp_med_m", "temp3_med_m",
         "temp_min_m", "temp3_min_m",
         "temp_max_m", "temp3_max_m",
         "nino34_m", "oni_m",
         "nino34_6_m","nino34_12_m")

clim_data <- lag_cov(data = clim_data,
  time = "index_date",
  group = "regional_geocode",
  var = var,
  lag = c(1, 6), merge = TRUE)

# read in enso data and tranform it into monthly

enso_data <- read.csv("data/raw_data/raw_data_infodengue/enso.csv")
enso_data <- enso_data %>%
  mutate(date = ymd(date) + 3) %>%
  mutate(year =  year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(enso = mean(enso))
glimpse(enso_data)

# read in environ data and tranform them into health region resolution

env_data <- read.csv("data/raw_data/raw_data_infodengue/environ_vars.csv")
env_data <- env_data %>% left_join(matching_matrix, by = "geocode") %>%
  group_by(regional_geocode) %>%
  summarise(biome = mode(biome),
            koppen = mode(koppen),
            altitude = mean(altitude))
glimpse(env_data)

# read in pop data and tranform them into health region resolution

pop_data <- read.csv("data/raw_data/raw_data_infodengue/IBGE_POPTCU.csv")

pop_data <- pop_data %>% filter(ANO >=2010) %>%
  left_join(matching_matrix, by = c("MUNIC_RES" = "geocode")) %>%
  mutate(year = ANO) %>%
  group_by(regional_geocode, year) %>%
  summarise(pop = sum(POPULACAO)) %>%
  filter(!is.na(regional_geocode))
glimpse(pop_data)

# Update 2022, 2023, 2024 pop from 2021
pop_data_2022 <- pop_data %>% filter(year == 2021) %>%
  mutate(year = 2022)

pop_data_2023 <- pop_data %>% filter(year == 2021) %>%
  mutate(year = 2023)

pop_data_2024 <- pop_data %>% filter(year == 2021) %>%
  mutate(year = 2024)

pop_data <- pop_data %>%
  rbind(pop_data_2022) %>%
  rbind(pop_data_2023) %>%
  rbind(pop_data_2024)


## 5. Merge dengue_data and climatic, enso, environ, and pop data

# Merge weekly dengue cases with covariates by region, year and month
data_week <- dengue_data_week %>%
  left_join(clim_data, by = c("regional_geocode", "month", "year")) %>%
  left_join(enso_data, by = c("month", "year")) %>%
  left_join(env_data, by = c("regional_geocode")) %>%
  left_join(pop_data, by = c("regional_geocode", "year")) %>%
  left_join(nino_year, by = c("epi_year"))
glimpse(data_week)

## Save final monthly dataset
write.csv(data_week, file = "data/analysis_data/weekly_data/weekly_data.csv")