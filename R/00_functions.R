# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## LOAD FUNCTIONS =================================================

# Description:
#     This script contains helper functions

# Script authors:
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Dr Giovenale Moirano  (giovenale.moirano@bsc.es)
#     Prof. Rachel Lowe     (rachel.lowe@bsc.es)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#' Model selection for the dengue sprint
#'
#' @param formulas Named vector with INLA formulas
#' @param data Data frame to fit the model to
#' @param path Path where the output of the function will be saved to.
#' @param filename String specifying the model name when writing to disk.
#' @param create_dir If TRUE, create a directory in the path with name equal to 
#' the formula name. Only possible if a single formula is supplied
#' @param eval_train If TRUE, obtain GOF in the training data.
#' @param eval_test If TRUE, obtain GOF in the test data.
#'
#' @returns
#' @export
#'
#' @examples
sprint_mod <- function(formulas, 
                       data, 
                       path,
                       filename = "train",
                       create_dir = FALSE,
                       eval_train = TRUE,
                       eval_test = FALSE){
  
  # Convert to GHR formulas
  mod_names <- names(formulas)
  GHRformulas <- as_GHRformulas(paste0("casos ~ 1", formulas))
  
  # Create a folder in the specified path with the name of interest
  if(length(formulas == 1) & isTRUE(create_dir)){
    path <- paste0(path, mod_names)
    dir.create(path)
  }else if (isTRUE(create_dir)){
    stop("Directory creation is only possible for formulas of length 1.")
  }
  
  # Fit model for the whole period, store it as rds. Store GOF metrics as csv.
  if(isTRUE(eval_train)){
    
    mod_names <- ifelse(is.null(mod_names), "mod", mod_names)
    mod_all <- fit_models(formulas = GHRformulas,
                          data = data,
                          family = "nbinomial",
                          name = mod_names,
                          offset = "pop",
                          control_compute = list(config = FALSE, vcov = FALSE))
    saveRDS(mod_all, paste0(path, "/", filename, "_mod.rds"))
    write.csv(mod_all$mod_gof, paste0(path, "/", filename, "_gof.csv"), row.names = FALSE)
  }
  
  if(isTRUE(eval_test)){
    
    # Results list and pred utils
    res_list <- list()
    cntrl <-  list(threshold_method = "percentile", p = 0.75,                        
                   threshold_floor = 5, direction = "full")
    
    # Validation 01
    print("Validation 01")
    data01 <- data[data$dataset_01!="Out",]
    limit01 <- max(data01[data01$dataset_01 == "Train", "date"])
    mod_01 <- ghr_predict(formula = GHRformulas,
                          data = data01,
                          family = "nbinomial",
                          offset = "pop",
                          control_strategy = list(fixed_until = limit01),
                          nsamples = 500)
    stats_01 <- prediction_stats(
      samples = mod_01,
      data = data01,
      temporal_unit = "week",
      spatial_unit = "hr_id",
      control_summary = cntrl, 
      control_stats = list(crps = TRUE, mae = TRUE, rmse = TRUE))
    res_list[[1]] <- stats_01
    rm("data01", "limit01", "mod_01", "stats_01")
    gc()
    
    # Validation 02
    print("Validation 02")
    data02 <- data[data$dataset_02!="Out",]
    limit02 <- max(data02[data02$dataset_02 == "Train", "date"])
    mod_02 <- ghr_predict(formula = GHRformulas,
                          data = data02,
                          family = "nbinomial",
                          offset = "pop",
                          control_strategy = list(fixed_until = limit02),
                          nsamples = 500)
    stats_02 <- prediction_stats(
      samples = mod_02,
      data = data02,
      temporal_unit = "week",
      spatial_unit = "hr_id",
      control_summary = cntrl, 
      control_stats = list(crps = TRUE, mae = TRUE, rmse = TRUE))
    res_list[[2]] <- stats_02
    rm("data02", "limit02", "mod_02", "stats_02")
    gc()
    
    # Validation 03
    print("Validation 03")
    data03 <- data[data$dataset_1!="Out",]
    limit03 <- max(data03[data03$dataset_1 == "Train", "date"])
    mod_03 <- ghr_predict(formula = GHRformulas,
                          data = data03,
                          family = "nbinomial",
                          offset = "pop",
                          control_strategy = list(fixed_until = limit03),
                          nsamples = 500)
    stats_03 <- prediction_stats(
      samples = mod_03,
      data = data03,
      temporal_unit = "week",
      spatial_unit = "hr_id",
      control_summary = cntrl, 
      control_stats = list(crps = TRUE, mae = TRUE, rmse = TRUE))
    res_list[[3]] <- stats_03
    rm("data03", "limit03", "mod_03", "stats_03")
    gc()
    
    # Validation 04
    print("Validation 04")
    data04 <- data[data$dataset_2!="Out",]
    limit04 <- max(data04[data04$dataset_2 == "Train", "date"])
    mod_04 <- ghr_predict(formula = GHRformulas,
                          data = data04,
                          family = "nbinomial",
                          offset = "pop",
                          control_strategy = list(fixed_until = limit04),
                          nsamples = 500)
    stats_04 <- prediction_stats(
      samples = mod_04,
      data = data04,
      temporal_unit = "week",
      spatial_unit = "hr_id",
      control_summary = cntrl, 
      control_stats = list(crps = TRUE, mae = TRUE, rmse = TRUE))
    res_list[[4]] <- stats_04
    rm("data04", "limit04", "mod_04", "stats_04")
    gc()
    
    # Validation 05
    print("Validation 05")
    data05 <- data[data$dataset_3!="Out",]
    limit05 <- max(data05[data05$dataset_3 == "Train", "date"])
    mod_05 <- ghr_predict(formula = GHRformulas,
                          data = data05,
                          family = "nbinomial",
                          offset = "pop",
                          control_strategy = list(fixed_until = limit05),
                          nsamples = 500)
    stats_05 <- prediction_stats(
      samples = mod_05,
      data = data05,
      temporal_unit = "week",
      spatial_unit = "hr_id",
      control_summary = cntrl, 
      control_stats = list(crps = TRUE, mae = TRUE, rmse = TRUE))
    res_list[[5]] <- stats_05
    rm("data05", "limit05", "mod_05", "stats_05")
    gc()
    
    # Write
    saveRDS(res_list, paste0(path, "/", filename, "_cv.rds"))
  }
  
}


#' CV evaluation for model selection saving the samples and accuracy statistics
#'
#' @param formula INLA formula.
#' @param data Data frame to fit the model to.
#' @param path Path where the output of the function will be saved to.
#' @param filename String specifying the model name when writing to disk.
#' @param season Season to validate. 
#'
#' @returns
#' @export
#'
#' @examples
sprint_eval <- function(formulas, 
                          data, 
                          path,
                          filename,
                          season){
  
  # Convert to GHR formulas
  GHRformulas <- as_GHRformulas(paste0("casos ~ 1", formulas))
  
  # pred utils
  cntrl <-  list(threshold_method = "percentile", p = 0.75,                        
                 threshold_floor = 5, direction = "full")
  
  # Data and limit
  print(paste0("Season: ", season))
  if(season == 2020){
    
    data_fit <- data[data$dataset_01!="Out",]
    limit_fit <- max(data_fit[data_fit$dataset_01 == "Train", "date"])
    
  }else if(season == 2021){
    
    data_fit <- data[data$dataset_02!="Out",]
    limit_fit <- max(data_fit[data_fit$dataset_02 == "Train", "date"])
    
  }else if(season == 2022){
    
    data_fit <- data[data$dataset_1!="Out",]
    limit_fit <- max(data_fit[data_fit$dataset_1 == "Train", "date"])
    
  }else if(season == 2023){
    
    data_fit <- data[data$dataset_2!="Out",]
    limit_fit <- max(data_fit[data_fit$dataset_2 == "Train", "date"])
    
  }else if(season == 2024){
    
    data_fit <- data[data$dataset_3!="Out",]
    limit_fit <- max(data_fit[data_fit$dataset_3 == "Train", "date"])
    
  }
  print(paste0("Limit: ", limit_fit))
  
  
  # Fit
  mod <- ghr_predict(formula = GHRformulas,
                     data = data_fit,
                     family = "nbinomial",
                     offset = "pop",
                     control_strategy = list(fixed_until = limit_fit),
                     nsamples = 500)
  saveRDS(mod, paste0(path, "/", filename, "_samples.rds"))
  
  # Stats
  stats <- prediction_stats(
    samples = mod,
    data = data_fit,
    temporal_unit = "week",
    spatial_unit = "hr_id",
    control_summary = cntrl, 
    control_stats = list(crps = TRUE, mae = TRUE, rmse = TRUE))
  saveRDS(stats, paste0(path, "/", filename, "_stats.rds"))
  
}


write_FE_form <- function(FE, re_spatial = re_s, re_weekly = re_w, re_yearly = re_y){
  form <- sapply(FE, function(x){
    if(x!=""){
      x2 <- paste(x, re_spatial, re_weekly, re_yearly, sep = " + ")
      x2 <- paste0(" + ", x2)
    }else{
      x2 <- paste(re_spatial, re_weekly, re_yearly, sep = " + ")
      x2 <- paste0(" + ", x2)
    }
    x2
  })
  form
}
