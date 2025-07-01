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
    
    # # Validation 01
    # data01 <- data[data$dataset_01!="Out",]
    # limit01 <- max(data01[data01$dataset_01 == "Train", "date"])
    # mod_01 <- ghr_predict(formula = GHRformulas,
    #                       data = data01,
    #                       family = "nbinomial",
    #                       offset = "pop",
    #                       control_strategy = list(fixed_until = limit01))
    
  }
  
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
