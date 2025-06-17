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

# 0. Null ----
re_null <- ""

# 1. Spatial random effects ----

re_s01 <- paste("f(hr_id, model = 'bym2', graph = g, scale.model = TRUE,",
                "hyper = precision.prior)")
re_s02 <- paste("f(hr_id, model = 'bym2', graph = g, scale.model = TRUE,",
                "hyper = precision.prior2)")

re_s03 <- paste("f(hr_id, model = 'bym2', replicate = 'nino_id', graph = g,",
                "scale.model = TRUE, hyper = precision.prior)")
re_s04 <- paste("f(hr_id, model = 'bym2', replicate = 'nino_id', graph = g,",
                "scale.model = TRUE, hyper = precision.prior2)")

# 2. Seasonal random effects ----

re_w01 <- paste("f(week_id, model = 'rw1', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior)")
re_w02 <- paste("f(week_id, model = 'rw2', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior)")
re_w03 <- paste("f(week_id, model = 'ar1', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior)")

re_w04 <- paste("f(week_id, model = 'rw1', replicate = state_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w05 <- paste("f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w06 <- paste("f(week_id, model = 'ar', replicate = state_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w07 <- paste("f(week_id, model = 'rw1', replicate = region_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w08 <- paste("f(week_id, model = 'rw2', replicate = region_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w09 <- paste("f(week_id, model = 'ar', replicate = region_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w10 <- paste("f(week_id, model = 'rw1', replicate = biome_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w11 <- paste("f(week_id, model = 'rw2', replicate = biome_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w12 <- paste("f(week_id, model = 'ar', replicate = biome_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w13 <- paste("f(week_id, model = 'rw1', replicate = koppen_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w14 <- paste("f(week_id, model = 'rw2', replicate = koppen_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w15 <- paste("f(week_id, model = 'ar', replicate = koppen_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w16 <- paste("f(week_id, model = 'rw1', replicate = nino_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w17 <- paste("f(week_id, model = 'rw2', replicate = nino_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")
re_w18 <- paste("f(week_id, model = 'ar', replicate = nino_id, cyclic = TRUE,",
                "constr = TRUE, scale.model = TRUE, hyper = precision.prior)")

re_w19 <- paste("f(week_id, model = 'rw1', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior2)")
re_w20 <- paste("f(week_id, model = 'rw2', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior2)")
re_w21 <- paste("f(week_id, model = 'ar1', cyclic = TRUE, constr = TRUE,",
                "scale.model = TRUE, hyper = precision.prior2)")

# 3. Yearly random effects ----
re_y01 <- "f(year_id, model = 'iid', hyper = precision.prior)"
re_y02 <- "f(year_id, model = 'rw1', hyper = precision.prior)"
re_y03 <- "f(year_id, model = 'rw2', hyper = precision.prior)"

re_y04 <- paste("f(year_id, model = 'iid', replicate = state_id, ",
                "hyper = precision.prior)")
re_y05 <- paste("f(year_id, model = 'rw1', replicate = state_id, ",
                "hyper = precision.prior)")
re_y06 <- paste("f(year_id, model = 'rw2', replicate = state_id, ",
                "hyper = precision.prior)")

re_y07 <- paste("f(year_id, model = 'iid', replicate = region_id, ",
                "hyper = precision.prior)")
re_y08 <- paste("f(year_id, model = 'rw1', replicate = region_id, ",
                "hyper = precision.prior)")
re_y09 <- paste("f(year_id, model = 'rw2', replicate = region_id, ",
                "hyper = precision.prior)")

re_y10 <- paste("f(year_id, model = 'iid', replicate = biome_id, ",
                "hyper = precision.prior)")
re_y11 <- paste("f(year_id, model = 'rw1', replicate = biome_id, ",
                "hyper = precision.prior)")
re_y12 <- paste("f(year_id, model = 'rw2', replicate = biome_id, ",
                "hyper = precision.prior)")

re_y13 <- paste("f(year_id, model = 'iid', replicate = koppen_id, ",
                "hyper = precision.prior)")
re_y14 <- paste("f(year_id, model = 'rw1', replicate = koppen_id, ",
                "hyper = precision.prior)")
re_y15 <- paste("f(year_id, model = 'rw2', replicate = koppen_id, ",
                "hyper = precision.prior)")

re_y16 <- "f(year_id, model = 'iid', hyper = precision.prior2)"
re_y17 <- "f(year_id, model = 'rw1', hyper = precision.prior2)"
re_y18 <- "f(year_id, model = 'rw2', hyper = precision.prior2)"


# 4. Temporal random effects ----
re_t01 <- "f(time_id, model = 'rw1', hyper = precision.prior)"
re_t02 <- "f(time_id, model = 'rw2', hyper = precision.prior)"
re_t03 <- "f(time_id, model = 'ar1', hyper = precision.prior)"

re_t04 <- "f(time_id, model = 'rw1', replicate = state_id, hyper = precision.prior)"
re_t05 <- "f(time_id, model = 'rw2', replicate = state_id, hyper = precision.prior)"
re_t06 <- "f(time_id, model = 'ar1', replicate = state_id, hyper = precision.prior)"

re_t07 <- "f(time_id, model = 'rw1', replicate = hr_id, hyper = precision.prior)"
re_t08 <- "f(time_id, model = 'rw2', replicate = hr_id, hyper = precision.prior)"
re_t09 <- "f(time_id, model = 'ar1', replicate = hr_id, hyper = precision.prior)"

re_t10 <- "f(time_id, model = 'rw1', replicate = biome_id, hyper = precision.prior)"
re_t11 <- "f(time_id, model = 'rw2', replicate = biome_id, hyper = precision.prior)"
re_t12 <- "f(time_id, model = 'ar1', replicate = biome_id, hyper = precision.prior)"

re_t13 <- "f(time_id, model = 'rw1', replicate = koppen_id, hyper = precision.prior)"
re_t14 <- "f(time_id, model = 'rw2', replicate = koppen_id, hyper = precision.prior)"
re_t15 <- "f(time_id, model = 'ar1', replicate = koppen_id, hyper = precision.prior)"

re_t16 <- "f(time_id, model = 'rw1', hyper = precision.prior2)"
re_t17 <- "f(time_id, model = 'rw2', hyper = precision.prior2)"
re_t18 <- "f(time_id, model = 'ar1', hyper = precision.prior2)"

# 5. Create lists for each type ----
all_n <- c("re_null" = re_null)
all_w <- unlist(mget(c(paste0("re_w", sprintf("%02d", 1:21)))))
all_y <- unlist(mget(c(paste0("re_y", sprintf("%02d", 1:18)))))
all_t <- unlist(mget(c(paste0("re_t", sprintf("%02d", 1:18)))))
all_s <- unlist(mget(c(paste0("re_s", sprintf("%02d", 1:4)))))
rm(list = ls()[grepl("re", ls())])