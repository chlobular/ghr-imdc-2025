# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## LOAD FUNCTIONS =================================================

# Description:
#     This script contains helper functions

# Script authors:
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Dr Giovenale Moirano  (giovenale.moirano@bsc.es)
#     Prof. Rachel Lowe     (rachel.lowe@bsc.es)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# create lagged variables
# Borrowed from GHRmodel, to delete once I can install the dependency
lag_cov <- function(data,
                    name,
                    time,
                    lag,
                    group = NULL,
                    add   = TRUE) {
  
  ## 1) Checks 
  if (missing(data) || missing(name) || missing(time) || missing(lag)) {
    missing_args <- c()
    if (missing(data))  missing_args <- c(missing_args, "data")
    if (missing(name))   missing_args <- c(missing_args, "name")
    if (missing(time))  missing_args <- c(missing_args, "time")
    if (missing(lag))   missing_args <- c(missing_args, "lag")
    stop("Missing required argument(s): ", paste(missing_args, collapse = ", "))
  }
  
  if (!is.character(name))
    stop("'name' must be a character vector.")
  
  if (!is.character(time) || length(time) != 1)
    stop("'time' must be a single character string.")
  
  # `lag` must be a numeric vector ---------------------------
  if (!is.numeric(lag) || any(lag < 0) || any(lag %% 1 != 0)) {
    stop("'lag' must be a vector of positive integers.")
  }
  lag <- sort(unique(as.integer(lag)))  # ensure sorted, unique integers
  
  if (!all(c(name, time) %in% names(data))) {
    missing_vars <- c(name, time)[!(c(name, time) %in% names(data))]
    stop("The following 'name' or 'time' are not present in the dataset: ",
         paste(missing_vars, collapse = ", "))
  }
  if (!is.null(group) && !all(group %in% names(data))) {
    missing_groups <- group[!(group %in% names(data))]
    stop("The following 'group' variables are not present in the dataset: ",
         paste(missing_groups, collapse = ", "))
  }
  if (any(is.na(data[[time]])))
    stop("The 'time' column contains missing values. Please clean it before lagging.")
  
  ## 2) ordering 
  if (!is.null(group)) {
    data <- data |>
      dplyr::group_by(dplyr::across(tidyr::all_of(group))) |>
      dplyr::arrange(dplyr::across(tidyr::all_of(c(group, time))))
  } else {
    data <- dplyr::arrange(data, .data[[time]])
  }
  
  ## 3) create lagged columns 
  lag_data <- data |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyr::all_of(name),
        .fns  = stats::setNames(
          lapply(lag, function(k) function(x) dplyr::lag(x, n = k)),
          paste0("l", lag)
        ),
        .names = "{.col}.{.fn}"
      )
    ) |>
    dplyr::ungroup()
  
  ## 4) drop originals if add = FALSE 
  if (!add) {
    lag_cols <- paste0(rep(name, each = length(lag)),
                       ".l",
                       rep(lag, times = length(var)))
    if (!all(lag_cols %in% names(lag_data)))
      stop("Some lagged columns were not created as expected.")
    
    lag_data <- lag_data |>
      dplyr::select(tidyr::all_of(lag_cols)) |>
      as.matrix()
  }
  
  return(lag_data)
}


#' Model selection for the dengue sprint
#'
#' @param formulas Named vector with INLA formulas
#' @param data Data frame to fit the model to
#' @param path Path where the output of the function will be saved to.
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
    
    mod_all <- fit_models(formulas = GHRformulas,
                          data = data,
                          family = "nbinomial",
                          name = mod_names,
                          offset = "pop",
                          control_compute = list(config = FALSE, vcov = FALSE))
    saveRDS(mod_all, paste0(path, "/train_mod.rds"))
    write.csv(mod_all$mod_gof, paste0(path, "/train_gof.csv"), row.names = FALSE)
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