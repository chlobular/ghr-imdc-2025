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

source("lsl_interaction_brazil/functions/00_packages.R")
source("lsl_interaction_brazil/functions/00_functions.R")




# create neighbourhood matrix
shp <- read_sf("data/shp/BR_Regionais.shp")  
shp <- shp %>% arrange (reg_id)

#nb.map <- spdep::poly2nb(shp, queen=T)
#if (!file.exists("data/shp/map.graph")) spdep::nb2INLA("data/shp/map.graph", nb.map)

#prior 

precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# set inla graph
g <- inla.read.graph("data/shp/map.graph")

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
int.mod <- readRDS("outputs/int_mod.rds")


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
#saveRDS(base.mod, file = "outputs/base_mod.rds")

# read in base model
base.mod <- readRDS("outputs/base_mod.rds")

## Univariable analysis --------------------------------------------------------

# create list of variables for analysis
tas_vars <- grep("temp", colnames(data), value=TRUE)
spi_vars <- grep("spi", colnames(data), value=TRUE)
spei_vars <- grep("spei", colnames(data), value=TRUE)
oni_vars<- grep("oni", colnames(data), value=TRUE)
nino_vars<- grep("nino34", colnames(data), value=TRUE)

# oth_vars (may want to include lagged versions of these!)

oth_vars <- c("factor(nino_year)", "koppen", "biome","nino34", "oni",
              "f(uf_id2, nino_year, model='iid')",
              "f(uf_id2, oni_m_lag_3, model='iid')",
              "f(uf_id2, oni_m_lag_4, model='iid')",
              "f(uf_id2, oni_m_lag_5, model='iid')",
              "f(uf_id2, oni_m_lag_6, model='iid')")

# create dataframe of univariable models
uni.mod <- data.frame(vars = c(tas_vars[2], spi_vars[3], spei_vars[5], oth_vars[8]))

# create list with gof dataframe, fitted values, fixed effects, random effects
uni.mod.out <- create.mod.out(uni.mod)

# run models and save ouputs
uni.mod.out <- fit.models(uni.mod.out, data, base.mod, int.mod, 
                          fname="univariable_mods")


## Long-short SPI-6 ------------------------------------------------------------

# update list of variables for analysis
spi6_vars_long <- grep("spi6.*\\.[4-6]$", colnames(data), value=TRUE)
spi6_vars_short <- grep("spi6.*\\.[1-3]$", colnames(data), value=TRUE)

# create dataframe of spi6 models
spi6.mod <- expand.grid(tas_vars, spi6_vars_long, spi6_vars_short,
                        oth_vars) %>% 
  mutate(vars=paste0(Var1, " * ", Var2, " * ", Var3, " + ", Var4),
         vars=str_replace(vars, "\\+$|^\\+", ""))

# create list with gof dataframe, fitted values, fixed effects, random effects
spi6.mod.out <- create.mod.out(spi6.mod)

# run models and save ouputs
spi6.mod.out <- fit.models(spi6.mod.out, data, base.mod, int.mod,
                           fname="outputs/spi6_mods")

# save spi6 models
saveRDS(spi6.mod.out, file = "outputs/spi6_mods.rds")


## Long-short SPI-3 ------------------------------------------------------------

# update list of variables for analysis
spi3_vars_long <- grep("spi3.*\\.[4-6]$", colnames(data), value=TRUE)
spi3_vars_short <- grep("spi3.*\\.[1-3]$", colnames(data), value=TRUE)

# create dataframe of spi3 models
spi3.mod <- expand.grid(tas_vars, spi3_vars_long, spi3_vars_short,
                        oth_vars) %>% 
  mutate(vars=paste0(Var1, " * ", Var2, " * ", Var3, " + ", Var4),
         vars=str_replace(vars, "\\+$|^\\+", ""))

# create list with gof dataframe, fitted values, fixed effects, random effects
spi3.mod.out <- create.mod.out(spi3.mod)

# run models and save ouputs
spi3.mod.out <- fit.models(spi3.mod.out, data, base.mod, int.mod,
                           fname="outputs/spi3_mods")

# save spi3 models
saveRDS(spi3.mod.out, file = "outputs/spi3_mods.rds")


## Long-short SPI-12 -----------------------------------------------------------

# update list of variables for analysis
spi12_vars_long <- grep("spi1.*\\.[5-6]$", colnames(data), value=TRUE)
spi12_vars_short <- c("spi12", grep("spi1.*\\.[1-2]$", colnames(data),
                                    value=TRUE))

# create dataframe of spi12 models
spi12.mod <- expand.grid(tas_vars, spi12_vars_long, spi12_vars_short,
                         oth_vars) %>% 
  mutate(vars=paste0(Var1, " * ", Var2, " * ", Var3, " + ", Var4),
         vars=str_replace(vars, "\\+$|^\\+", ""))

# create list with gof dataframe, fitted values, fixed effects, random effects
spi12.mod.out <- create.mod.out(spi12.mod)

# run models and save ouputs
spi12.mod.out <- fit.models(spi12.mod.out, data, base.mod, int.mod,
                            fname="outputs/spi12_mods")

# save spi12 models
saveRDS(spi12.mod.out, file = "outputs/spi12_mods.rds")


## END
