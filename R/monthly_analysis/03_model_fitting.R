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
source("functions/00_packages_functions.R")

# read in harmonised data
data <- read.csv("data/data.csv")  # *** TO ADD WHEN AVAILABLE!!!!
data <- data[data$train_1 == TRUE, ]  # *** CHECK THIS!!!


## Baseline models -------------------------------------------------------------

# INTERCEPT MODEL
# - intercept

# run intercept only model
intercept <- "casos ~ 1"
int.mod <- runinlamod(formula(intercept), data)

# save intercept only model
#saveRDS(int.mod, file = "outputs/int_mod.rds")

# read in intercept only model
int.mod <- readRDS("outputs/int_mod.rds")


# BASELINE MODEL
# - intercept
# - monthly cyclic random effect (RE) using random walk (RW2)
# - interannual RE using random walk (RW2)
# - health region level spatial RE using modified Besag-York-Mollie (BYM2)

re1 <- paste("f(month, replicate = uf_index, model = 'rw2', cyclic = TRUE,",
             "constr = TRUE, scale.model=TRUE, hyper=precision.prior)")
re2 <- paste("f(year_index, model='rw2', hyper=precision.prior)")
re3 <- paste("f(regional_index, model = 'bym2', graph = g,",
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
tas_vars <- grep("tas.*\\.\\d+$", colnames(data), value=TRUE)
spi_vars <- grep("spi", colnames(data), value=TRUE)
oth_vars <- grep()  # *** TO ADD WHEN WE KNOW WHICH VARIABLES TO INCLUDE

# *** suggestions for oth_vars
# *** may want to include lagged versions of these!
# *** (also, if multiple used in multivariable function, need to create more uf_index versions i.e. uf_index_3, uf_index_4, etc.)
oth_vars <- c("nino_year", "nino34", "oni",
              "f(uf_index_2, nino_year, model='iid')",
              "f(uf_index_2, nino34, model='iid')",
              "f(uf_index_2, oni, model='iid')",
              "f(inla.group(nino34, method='cut'), model='rw2')",
              "f(inla.group(oni, method='cut'), model='rw2')",
              "[ENVIRONMENTAL]",
              "[SOCIOECONOMIC]")

# create dataframe of univariable models
uni.mod <- data.frame(vars = c(tas_vars, spi_vars, oth_vars))

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
