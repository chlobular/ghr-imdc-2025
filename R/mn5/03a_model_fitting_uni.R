# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## MODEL FITTING ===============================================================

# Description:
#     Fit models with combos of interacting covariates (temperature anomaly, 
#     long lag SPI and short lag SPI) as well as other variables (urbanisation,
#     SE vars) with disease cases as the response and population as the offset.

# Script authors:
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Dr Giovenale Moirano  (giovenale.moirano@bsc.es)
#     Prof. Rachel Lowe     (rachel.lowe@bsc.es)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

## Source packages, functions and data -----------------------------------------

# load packages and functions
packages <- c("INLA", "dplyr","sf")
lapply(packages, library, character.only = TRUE)

source("lsl_interaction_brazil/functions/00_functions.R")

# create neighbourhood matrix
shp <- read_sf("data/shp/BR_Regionais.shp")  
shp <- shp %>% arrange (reg_id)

#nb.map <- spdep::poly2nb(shp, queen=T)
#if (!file.exists("data/shp/map.graph")) spdep::nb2INLA("data/shp/map.graph", nb.map)

# set inla graph
g <- inla.read.graph("data/shp/map.graph")

#prior
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# read in harmonised data
data <- read.csv("data/analysis_data/weekly_data/weekly_data.csv")  
data <- data[data$train_1 == "True", ]  
glimpse(data)
dim(data)


# Format variables for inla 

data <- data %>%
  mutate(week_id = substr(epiweek,5,6),
         year_id = as.numeric(factor(epi_year)),
         regional_id = as.numeric(factor(regional_geocode)),
         uf_id = as.numeric(factor(uf)),
         uf_id2 =  as.numeric(factor(uf)))



## Baseline models -------------------------------------------------------------

# INTERCEPT MODEL
# - intercept

# run intercept only model
intercept <- "casos ~ 1"
int.mod <- runinlamod(formula(intercept), data)

# save intercept only model
saveRDS(int.mod, file = "lsl_interaction_brazil/weekly_analysis/outputs/int_mod.rds")

# read in intercept only model
int.mod <- readRDS("lsl_interaction_brazil/weekly_analysis/outputs/int_mod.rds")


# BASELINE MODEL
# - intercept
# - monthly cyclic random effect (RE) using random walk (RW2)
# - interannual RE using random walk (RW2)
# - health region level spatial RE using modified Besag-York-Mollie (BYM2)

re1 <- paste("f(week_id, replicate = uf_id, model = 'rw2', cyclic = TRUE,",
             "constr = TRUE, scale.model=TRUE, hyper=precision.prior)")
re2 <- paste("f(year_id, model='rw2', hyper=precision.prior)")
re3 <- paste("f(regional_id, model = 'bym2', graph = g,",
             "scale.model = TRUE, hyper = precision.prior)")

baseformula <- paste(c("casos ~ 1", re1, re2, re3), collapse=" + ")

# run base model with negative binomial and poisson likelihood
base.mod <- runinlamod(formula(baseformula), data)
base.mod.p <- runinlamod(formula(baseformula), data, family="poisson")
dic(base.mod) - dic(base.mod.p)

# save base model
saveRDS(base.mod, file = "lsl_interaction_brazil/weekly_analysis/outputs/base_mod.rds")

# read in base model
base.mod <- readRDS("lsl_interaction_brazil/weekly_analysis/outputs/base_mod.rds")

## Univariable analysis --------------------------------------------------------

# create list of variables for analysis
tas_vars <- grep("temp", colnames(data), value=TRUE)
spi_vars <- grep("spi", colnames(data), value=TRUE)
spei_vars <- grep("spei", colnames(data), value=TRUE)
oni_vars <- grep("oni", colnames(data), value=TRUE)
nino34_vars <- grep("nino34", colnames(data), value=TRUE)

# oth_vars (may want to include lagged versions of these!)

oth_vars <- c("factor(nino_year)", "koppen", "biome","nino34_m", "oni_m",
              "f(uf_id2, nino_year, model='iid')",
              "f(uf_id2, oni_m_lag_3, model='iid')",
              "f(uf_id2, oni_m_lag_4, model='iid')",
              "f(uf_id2, oni_m_lag_5, model='iid')",
              "f(uf_id2, oni_m_lag_6, model='iid')")


# create dataframe of univariable models
uni.mod <- data.frame(vars = c(tas_vars, spi_vars, spei_vars,oni_vars,
                      nino34_vars, oth_vars))

# create list with gof dataframe, fitted values, fixed effects, random effects
uni.mod.out <- create.mod.out(uni.mod)

# run models and save ouputs
a<-Sys.time()
uni.mod.out <- fit.models(uni.mod.out, data, base.mod, int.mod, 
                          fname="lsl_interaction_brazil/weekly_analysis/outputs/univariable_mods")
b<-Sys.time

print(b-a)

