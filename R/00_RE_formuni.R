# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Random effects ============================================================
# 
# Description:
#     List of random effects candidates
# 
# Script authors:
#     Chloe Fletcher  (chloe.fletcher@bsc.es)
#     Carles Milà  (carles.milagarcia@bsc.es)
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# 0. Baseline
all_baseline <- ""

# 1. Spatial random effects ----

re_s01 <- paste("f(hr_id, model = 'bym2', graph = g, scale.model = TRUE,",
                "hyper = precision.prior, constr = TRUE)")
re_s02 <- paste("f(hr_id, model = 'bym2', graph = g, scale.model = TRUE,",
                "hyper = precision.prior2, constr = TRUE)")
re_s03 <- paste("f(hr_id, model = 'bym2', replicate = nino_id, graph = g,",
                "scale.model = TRUE, hyper = precision.prior, constr = TRUE)")
re_s04 <- paste("f(hr_id, model = 'bym2', replicate = nino_id, graph = g,",
                "scale.model = TRUE, hyper = precision.prior2, constr = TRUE)")

# 2. Seasonal random effects ----
re_w01 <- paste("f(week_id, model = 'rw1', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior)")
re_w02 <- paste("f(week_id, model = 'rw2', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior)")

re_w03 <- paste("f(week_id, model = 'rw1', replicate = state_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w04 <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w05 <- paste("f(week_id, model = 'rw1', replicate = region_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w06 <- paste("f(week_id, model = 'rw2', replicate = region_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w07 <- paste("f(week_id, model = 'rw1', replicate = biome_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w08 <- paste("f(week_id, model = 'rw2', replicate = biome_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w09 <- paste("f(week_id, model = 'rw1', replicate = koppen_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w10 <- paste("f(week_id, model = 'rw2', replicate = koppen_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w11 <- paste("f(week_id, model = 'rw1', replicate = nino_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w12 <- paste("f(week_id, model = 'rw2', replicate = nino_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w13 <- paste("f(week_id, model = 'rw1', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior2)")
re_w14 <- paste("f(week_id, model = 'rw2', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior2)")

# 3. Yearly random effects ----
re_y01 <- "f(year_id, model = 'iid', hyper = precision.prior, constr = TRUE)"
re_y02 <- "f(year_id, model = 'rw1', hyper = precision.prior, constr = TRUE)"
re_y03 <- "f(year_id, model = 'rw2', hyper = precision.prior, constr = TRUE)"

re_y04 <- paste("f(year_id, model = 'iid', replicate = state_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y05 <- paste("f(year_id, model = 'rw1', replicate = state_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y06 <- paste("f(year_id, model = 'rw2', replicate = state_id, ",
                "hyper = precision.prior, constr = TRUE)")

re_y07 <- paste("f(year_id, model = 'iid', replicate = region_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y08 <- paste("f(year_id, model = 'rw1', replicate = region_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y09 <- paste("f(year_id, model = 'rw2', replicate = region_id, ",
                "hyper = precision.prior, constr = TRUE)")

re_y10 <- paste("f(year_id, model = 'iid', replicate = biome_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y11 <- paste("f(year_id, model = 'rw1', replicate = biome_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y12 <- paste("f(year_id, model = 'rw2', replicate = biome_id, ",
                "hyper = precision.prior, constr = TRUE)")

re_y13 <- paste("f(year_id, model = 'iid', replicate = koppen_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y14 <- paste("f(year_id, model = 'rw1', replicate = koppen_id, ",
                "hyper = precision.prior, constr = TRUE)")
re_y15 <- paste("f(year_id, model = 'rw2', replicate = koppen_id, ",
                "hyper = precision.prior, constr = TRUE)")

re_y16 <- "f(year_fct, model = 'iid', hyper = precision.prior2, constr = TRUE)"
re_y17 <- "f(year_id, model = 'rw1', hyper = precision.prior2, constr = TRUE)"
re_y18 <- "f(year_id, model = 'rw2', hyper = precision.prior2, constr = TRUE)"

# 4. Temporal random effects ----
re_t01 <- "f(time_id, model = 'rw1', hyper = precision.prior, constr = TRUE)"
re_t02 <- "f(time_id, model = 'rw2', hyper = precision.prior, constr = TRUE)"
re_t03 <- "f(time_id, model = 'ar1', hyper = precision.prior, constr = TRUE)"

re_t04 <- "f(time_id, model = 'rw1', replicate = state_id, hyper = precision.prior, constr = TRUE)"
re_t05 <- "f(time_id, model = 'rw2', replicate = state_id, hyper = precision.prior, constr = TRUE)"
re_t06 <- "f(time_id, model = 'ar1', replicate = state_id, hyper = precision.prior, constr = TRUE)"

re_t07 <- "f(time_id, model = 'rw1', replicate = region_id, hyper = precision.prior, constr = TRUE)"
re_t08 <- "f(time_id, model = 'rw2', replicate = region_id, hyper = precision.prior, constr = TRUE)"
re_t09 <- "f(time_id, model = 'ar1', replicate = region_id, hyper = precision.prior, constr = TRUE)"

re_t10 <- "f(time_id, model = 'rw1', replicate = biome_id, hyper = precision.prior, constr = TRUE)"
re_t11 <- "f(time_id, model = 'rw2', replicate = biome_id, hyper = precision.prior, constr = TRUE)"
re_t12 <- "f(time_id, model = 'ar1', replicate = biome_id, hyper = precision.prior, constr = TRUE)"

re_t13 <- "f(time_id, model = 'rw1', replicate = koppen_id, hyper = precision.prior, constr = TRUE)"
re_t14 <- "f(time_id, model = 'rw2', replicate = koppen_id, hyper = precision.prior, constr = TRUE)"
re_t15 <- "f(time_id, model = 'ar1', replicate = koppen_id, hyper = precision.prior, constr = TRUE)"

re_t16 <- "f(time_id, model = 'rw1', hyper = precision.prior2, constr = TRUE)"
re_t17 <- "f(time_id, model = 'rw2', hyper = precision.prior2, constr = TRUE)"
re_t18 <- "f(time_id, model = 'ar1', hyper = precision.prior2, constr = TRUE)"

# 5. Create lists for each type ----
names(all_baseline) <- "re_baseline"

# spatial
all_s <- unlist(mget(c(paste0("re_s", sprintf("%02d", 1:4)))))
names_s <- names(all_s)
all_s <- paste0(" + ", all_s)
names(all_s) <- names_s

# seasonal
all_w <- unlist(mget(c(paste0("re_w", sprintf("%02d", 1:14)))))
names_w <- names(all_w)
all_w <- paste0(" + ", all_w)
names(all_w) <- names_w

# yearly
all_y <- unlist(mget(c(paste0("re_y", sprintf("%02d", 1:18)))))
names_y <- names(all_y)
all_y <- paste0(" + ", all_y)
names(all_y) <- names_y

# temporal
all_t <- unlist(mget(c(paste0("re_t", sprintf("%02d", 1:18)))))
names_t <- names(all_t)
all_t <- paste0(" + ", all_t)
names(all_t) <- names_t

rm(list = ls()[grepl("re", ls()) | grepl("names", ls())])