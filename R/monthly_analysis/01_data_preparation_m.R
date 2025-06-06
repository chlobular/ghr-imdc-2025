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

## 1.Source packages and define customized functions --------------------------

# load packages
packages <- c("dplyr", "tidyr", "lubridate", "zoo")
lapply(packages, library, character.only = TRUE)

source("lsl_interaction_brazil/functions/00_functions.R")

# define function mode()
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## 2.Import dengue data----------------------------------------------------

# read in raw monthly dengue data 
dengue_data_month <- read.csv("data/raw_data/raw_data_infodengue/dengue_month.csv")
names(dengue_data_month) <- c("nrow", "year", "month", "geocode", "casos")
glimpse(dengue_data_month)

# read in raw weekly dengue data to retrieve health region code
dengue_data_week <- read.csv("data/raw_data/raw_data_infodengue/dengue_week.csv")
glimpse(dengue_data_week)
dim(dengue_data_week)

# create a matching matrix between municipality,health region, and state code
matching_matrix <- dengue_data_week %>%
  select(geocode, regional_geocode, uf) %>%
  distinct(geocode, regional_geocode, uf)

rm(dengue_data_week)

## 3.Aggregate monthly dengue cases from municipality to health region level---

dengue_data_month <- dengue_data_month %>%
  left_join(matching_matrix, by = "geocode") %>%
  group_by(uf, regional_geocode, year, month) %>%
  summarise(casos = sum(casos)) %>%
  drop_na() %>%
  mutate(date = paste0(year, "-", month, "-01")) %>%
  mutate(date = ymd(date))
glimpse(dengue_data_month)

# Define epi_year (From 1 to 15, starting at epiweek 41)

dengue_data_month <- dengue_data_month %>% mutate(epi_year = year - 2009) %>%
  mutate(epi_year = ifelse(as.numeric(month) <= 9,
         epi_year, epi_year + 1))


## 4. Import climatic and environmental data and merge with dengue data -----

# read in raw monthly climatic data and merge to dengue data

clim_data <- read.csv("data/raw_data/raw_data_ghr/brazil_monthly_climate_updatedNino.csv")
clim_data <- clim_data %>%
  rename(regional_geocode = adm_id, regional_name = adm_name)
glimpse(clim_data)

# create 3 months moving average of med, min, max temp
# (scaled by the overall brazilian mean)
temp3_m <- clim_data %>%
  group_by(regional_geocode) %>%
  arrange(year, month) %>%
  mutate(temp3_med_m = rollmean(temp_med_m, 3, fill = NA, align = "right"),
         temp3_min_m = rollmean(temp_min_m, 3, fill = NA, align = "right"),
         temp3_max_m = rollmean(temp_max_m, 3, fill = NA, align = "right")) %>%
  select(regional_geocode, temp3_med_m, temp3_min_m, temp3_max_m,
         year, month) %>%
  arrange(regional_geocode, year, month)

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
    nino < -0.5 ~ -1,
    nino >= -0.5 & nino <= 0.5 ~ 0,
    nino > 0.5 ~ 1
  )) %>% select(nino_year,epi_year)


## Lag selected climatic variable (vars)
clim_data$index_date <- paste0(clim_data$year, "-",
                               clim_data$month, "-01")
clim_data$index_date <- ymd(clim_data$index_date)

var <- c("spi1_m", "spei1_m",
         "spi3_m", "spei3_m",
         "spi6_m", "spei6_m",
         "spi12_m", "spei12_m",
         "temp_med_m", "temp3_med_m",
         "temp_min_m", "temp3_min_m",
         "temp_max_m", "temp3_max_m",
         "nino34_m", "oni_m")

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

pop_data <- pop_data %>% filter(ANO >=2009) %>%
  left_join(matching_matrix, by = c("MUNIC_RES" = "geocode")) %>%
  mutate(year = ANO) %>%
  group_by(regional_geocode, year) %>%
  summarise(pop = sum(POPULACAO))
glimpse(pop_data)

## 5. Merge dengue_data and climatic, enso, environ, and pop data 
data <- dengue_data_month %>%
  left_join(clim_data, by = c("regional_geocode", "month", "year")) %>%
  #left_join(enso_data, by = c("month", "year")) %>%
  #left_join(env_data, by = c("regional_geocode")) %>%
  #  left_join(nino_year, by = c("year"))
  left_join(pop_data, by = c("regional_geocode", "year")) %>%
  left_join(temp3_m, by = c("regional_geocode", "month", "year"))
glimpse(data)

## 6. Recreate the train_1,train_2,target_1,target_2 variables

threshold_1 <-  ymd("2022-10-01")
threshold_2 <-  ymd("2023-10-01")

data <- data %>%
  mutate(train_1 = ifelse(date > threshold_1,
                          "False",
                          "True"),
         train_2 = ifelse(date > threshold_2,
                          "False",
                          "True"),
         target_1 = ifelse (date > threshold_1 & date < threshold_2, 
                            "True",
                            "False"),
         target_2 = ifelse (date > threshold_2, 
                            "True",
                            "False"),
         )


## Save final monthly dataset
glimpse(data)
write.csv(data, file = "data/analysis_data/monthly_data/monthly_data.csv")

