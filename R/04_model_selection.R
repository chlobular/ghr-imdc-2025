# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## MODEL SELECTION =============================================================

# Description:
#     Select best temperature anomaly, long lag and short lag SPI interaction
#     model by comparing goodness of fit (GOF) metrics (DIC, WAIC, MAE, RsqLR),
#     fitted values, fixed effects and random effects.

# Script authors:
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Dr Giovenale Moirano  (giovenale.moirano@bsc.es)
#     Prof. Rachel Lowe     (rachel.lowe@bsc.es)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

## Source packages, functions and data -----------------------------------------

# load packages and functions
source("functions/00_packages_functions.R")

# read in harmonised data
data <- read.csv("data/data.csv")
data <- data[data$train_1 == TRUE, ]  # *** CHECK THIS!!!

# load model output
spi.mod.out <- readRDS("outputs/spi6_mods.rds")  # *** if this is the best!!!


## Visualise goodness of fit matrices ------------------------------------------

# mean temperature gof matrix
gof_temp_med_m <- gof_matrix(spi.mod.out, temp_var="temp_med_m")
multi.heatmap.gof(gof_temp_med_m,
                  "Goodness of Fit for Mean Temperature-SPI Models")

gof_temp_med_3m <- gof_matrix(spi.mod.out, temp_var="temp_med_3m")
multi.heatmap.gof(gof_temp_med_3m,
                  "Goodness of Fit for 3-Month Mean Temperature-SPI Models")

# max temperature gof matrix
gof_temp_max_m <- gof_matrix(spi.mod.out, temp_var="temp_max_m")
multi.heatmap.gof(gof_temp_max_m,
                  "Goodness of Fit for Maximum Temperature-SPI Models")

gof_temp_max_3m <- gof_matrix(spi.mod.out, temp_var="temp_max_3m")
multi.heatmap.gof(gof_temp_max_3m,
                  "Goodness of Fit for 3-Month Maximum Temperature-SPI Models")

# min temperature gof matrix
gof_temp_min_m <- gof_matrix(spi.mod.out, temp_var="temp_min_m")
multi.heatmap.gof(gof_temp_min_m,
                  "Goodness of Fit for Minimum Temperature-SPI Models")

gof_temp_min_3m <- gof_matrix(spi.mod.out, temp_var="temp_min_3m")
multi.heatmap.gof(gof_temp_min_3m,
                  "Goodness of Fit for 3-Month Minimum Temperature-SPI Models")


## Extract best models by GOF --------------------------------------------------

# number of best models to select
n <- 20

# find best n models by gof metric
best_dic <- spi.mod.out$mod.gof[with(spi.mod.out$mod.gof,
                                      rev(order(dic_vs_base))),"vars"][1:n]
best_waic <- spi.mod.out$mod.gof[with(spi.mod.out$mod.gof,
                                       rev(order(waic_vs_base))),"vars"][1:n]
best_mae <- spi.mod.out$mod.gof[with(spi.mod.out$mod.gof,
                                      rev(order(mae_vs_base))),"vars"][1:n]
best_rsq <- spi.mod.out$mod.gof[with(spi.mod.out$mod.gof,
                                      rev(order(rsq))),"vars"][1:n]

# identify unique best models
best_mods <- Reduce(union, list(best_dic, best_waic, best_mae, best_rsq))

bm_id <- rownames(spi.mod.out$mod.gof[spi.mod.out$mod.gof$vars %in% best_mods, ])
bm_id <- as.numeric(bm_id)

# identify best models across all metrics
best_all <- Reduce(intersect, list(best_dic, best_waic, best_mae, best_rsq))

ba_id <- rownames(spi.mod.out$mod.gof[spi.mod.out$mod.gof$vars %in% best_all, ])
ba_id <- as.numeric(ba_id)


## Explore best models ---------------------------------------------------------

# evaluate goodness of fit for best models
best_mods_gof <- spi.mod.out$mod.gof[spi.mod.out$mod.gof$vars %in% best_mods, ]

# plot fitted values for each best model
# for (bm in bm_id){
#   print(plot.fit(spi.mod.out$fitted[[bm]], title=spi.mod.out$mod.gof$vars[bm]))
# }

# append baseline model
base.mod <- readRDS("outputs/base_mod.rds")
idx <- nrow(spi.mod.out$mod.gof) + 1

spi.mod.out$mod.gof[idx,"vars"] <- "base"
spi.mod.out$fitted[[idx]] <- base.mod$summary.fitted.values
spi.mod.out$fixed[[idx]] <- base.mod$summary.fixed
spi.mod.out$random[[idx]] <- base.mod$summary.random

# plot random effects of each model incl. baseline
selection <- c(bm_id[grepl("spi6_m.4", best_mods_gof$vars)],idx)  # lag 4 models
selection <- c(bm_id[grepl("spi6_m.5", best_mods_gof$vars)],idx)  # lag 5 models
selection <- c(bm_id[grepl("spi6_m.6", best_mods_gof$vars)],idx)  # lag 6 models

plot.temp.res(mods=spi.mod.out$random[selection],
              titles=spi.mod.out$mod.gof$vars[selection])  # weekly effect
plot.year.res(mods=spi.mod.out$random[selection],
              titles=spi.mod.out$mod.gof$vars[selection])  # interannual effect

red_week <- c(idx)  # reduced weekly        *** insert best here
red_year <- c(idx)  # reduced interannual   *** insert best here

red <- intersect(red_week, red_year)  # reduced weekly and interannual

# print fixed effects of models with reduced temporal effects
for (r in red){
  print(r)
  print(spi.mod.out$fixed[[r]])
}

# print fixed effects of models with best gof metrics
for (b in ba_id){
  print(b)
  print(spi.mod.out$fixed[[b]])
}

# plot spatial random effects
selection <- c()    #  *** insert best here
for (i in 1:length(selection)){
  print(plot.spat.res(shp,
                      spi.mod.out$random[[selection[i]]],
                      ndistricts=unique(shp$COL),  # *** UPDATE COL WHEN AVAILABLE!!!
                      title=spi.mod.out$mod.gof[selection[i],]))
}


## Select best models ----------------------------------------------------------

# list of candidates
candidate_mods <- c()    #  *** insert best here

# name of candidates
spi.mod.out$mod.gof$vars[candidate_mods]


## END