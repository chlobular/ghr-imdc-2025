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

# set up random effects
re1 <- paste("f(month, replicate = uf_index, model = 'rw2', cyclic = TRUE,",
             "constr = TRUE, scale.model=TRUE, hyper=precision.prior)")
re2 <- paste("f(year_index, model='rw2', hyper=precision.prior)")
re3 <- paste("f(regional_index, model = 'bym2', graph = g,",
             "scale.model = TRUE, hyper = precision.prior)")
baseformula <- paste(c("casos ~ 1", re1, re2, re3), collapse=" + ")

# read in intercept only model
int.mod <- readRDS("outputs/int_mod.rds")

# read in base model
base.mod <- readRDS("outputs/base_mod.rds")


## Long-short SPI-12 -----------------------------------------------------------

# create list of variables for analysis
tas_vars <- grep("tas.*\\.\\d+$", colnames(data), value=TRUE)
spi12_vars_long <- grep("spi1.*\\.[5-6]$", colnames(data), value=TRUE)
spi12_vars_short <- c("spi12", grep("spi1.*\\.[1-2]$", colnames(data),
                                    value=TRUE))
oth_vars <- grep()  # *** TO ADD WHEN WE KNOW WHICH VARIABLES TO INCLUDE

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
