# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## LOAD FUNCTIONS =================================================

# Description:
#     This script contains all of the functions with the long and
#     short lag interaction repository for Brazil.

# Script authors:
#     Chloe Fletcher        (chloe.fletcher@bsc.es)
#     Dr Giovenale Moirano  (giovenale.moirano@bsc.es)
#     Prof. Rachel Lowe     (rachel.lowe@bsc.es)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


## Variables -------------------------------------------------------------------

# set maximum lag
nlag = 6

## Data processing functions ---------------------------------------------------

# define function mode()
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# use year and month columns to add date column
monthyeartodate <- function(data, year_col="year", month_col="month") {
  # add a month-year column for plotting
  data$date <- zoo::as.yearmon(paste(data[[year_col]], data[[month_col]]),
                               "%Y %m")
  # move column to first in dataframe
  data <- data %>% relocate(date) %>%
    mutate(date=as.Date(date, format="%d-%m-%Y"))
  
  return(data)
}

# convert date to year and month column format
datetomonthyear <- function(data_frame) {
  # create columns year/month as integers from date column
  # move to first two columns and drop date
  data_frame_monthly <- data_frame %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(format(date, "%m"))) %>%
    relocate(date, year, month)
  return(data_frame_monthly)
}


# create lagged variables
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



## Goodness of fit functions ---------------------------------------------------

# bayesian r squared function
rsq <- function(model, null){
  dev_model <- model$dic$deviance.mean
  dev_null <- null$dic$deviance.mean
  n <- nrow(model$summary.fitted.values)
  
  rsq <- 1-exp((-2/n)*((dev_model/-2)-(dev_null/-2)))
  return(round(rsq, 3))
}

# deviance information criterion (dic)
dic <- function(model){
  dic <- round(model$dic$dic,0)
  return(round(dic, 0))
}

# difference in dic between model and baseline
dic.diff <- function(model, null){
  dic_model <- dic(model)
  dic_null <- dic(null)
  
  diff <- dic_null - dic_model
  return(diff)
}

# waikake information criterion (waic)
waic <- function(model){
  waic <- round(model$waic$waic,0)
  return(round(waic, 0))
}

# difference in waic between model and baseline
waic.diff <- function(model, null){
  waic_model <- waic(model)
  waic_null <- waic(null)
  
  diff <- waic_null - waic_model
  return(diff)
}

# mean absolute error (mae)
mae <- function(obs, model){
  fit <- model$summary.fitted.values[["0.5quant"]]
  mae <- mean(abs(fit-obs), na.rm=TRUE)
  return(round(mae, 3))
}

# difference in mae between model and baseline
mae.diff <- function(obs, model, null){
  mae_model <- mae(obs, model)
  mae_null <- mae(obs, null)
  
  diff <- mae_null - mae_model
  return(diff)
}


## Model fitting functions -----------------------------------------------------

# run model in INLA
runinlamod <- function(formula, data = data, family = "nbinomial", config = FALSE){
  # formula     :  formula for INLA
  # data        :  data as dataframe object
  # family      :  likelihood distribution (default: negative binomial)
  # config      :  enable sampling (default: FALSE, set to TRUE in final runs)
  mod <- inla(formula = formula, data = data, family = family,
              offset = log(pop/10^5),
              control.inla = list(strategy = 'adaptive'), 
              control.compute = list(dic = TRUE, waic = TRUE, config = config,
                                     cpo = TRUE, return.marginals = FALSE),
              control.fixed = list(correlation.matrix = TRUE, 
                                   prec.intercept = 1, prec = 1),
              control.predictor = list(link = 1, compute = TRUE), 
              verbose = FALSE,
              inla.setOption(num.threads = 32))
  mod <- inla.rerun(mod)
  return(mod)
}

# update base formula
form.update <- function(x){
  form <- paste(baseformula, x, sep=" + ")
  return(form)
}

# create mod.out list for fit.models
create.mod.out <- function(mod.list){
  #  input:
  #    -  mod.list : dataframe w/ single column containing variable combinations
  #                  (representing fixed effects to be run in models)
  #  output:
  #    list (mod.out) with elements:
  #    -  mod.gof  : dataframe of GOF metrics (vars, dic, dic_vs_base, waic,
  #                                            waic_vs_base, mae, mae_vs_base,
  #                                            rsq, formula)
  #    -  fitted   : list of fitted values (length = number of models)
  #    -  fixed    : list of fixed effects (length = number of models)
  #    -  random   : list of random effects (length = number of models)
  mod.out <- list(mod.gof= data.frame(vars = mod.list$vars,
                                      dic = NA, dic_vs_base = NA,
                                      waic = NA, waic_vs_base = NA,
                                      mae = NA, mae_vs_base = NA,
                                      rsq = NA, formula = NA),
                  fitted=vector("list", length=nrow(mod.list)),
                  fixed=vector("list", length=nrow(mod.list)),
                  random=vector("list", length=nrow(mod.list)))
  return(mod.out)
}

# run inla models and save outputs as rds
fit.models <- function(mod.out, data=data, base, int, fname=""){
  #  input:
  #    -  mod.out : empty output (list) except dataframe mod.gof is populated by
  #                 variable combinations to run in `vars` col
  #    -  data    : dataframe containing response and covariates (default=data)
  #    -  base    : baseline (null) model object
  #    -  int     : intercept only model object
  #    -  fname   : character string of filename to save rds output
  #  output:
  #    save rds files (1 per 20 models) containing list (mod.out) with elements:
  #    -  mod.gof : dataframe of GOF metrics (as from create.mod.out)
  #    -  fitted  : list of fitted values (length = number of models)
  #    -  fixed   : list of fixed effects (length = number of models)
  #    -  random  : list of random effects (length = number of models)
  #    and return list object (mod.out)
  
  # loop over number of vars in mod.gof
  for (i in 1:nrow(mod.out$mod.gof)){
    # message
    print(paste("Running model",i,"of",nrow(mod.out$mod.gof)))
    
    # assign formula and model
    form <- form.update(mod.out$mod.gof[i,"vars"])
    mod <- runinlamod(formula(form), data)
    
    # save gof metrics
    mod.out$mod.gof[i,"dic"] <- dic(mod)
    mod.out$mod.gof[i,"dic_vs_base"] <- dic.diff(mod, base)
    mod.out$mod.gof[i,"waic"] <- waic(mod)
    mod.out$mod.gof[i,"waic_vs_base"] <- waic.diff(mod, base)
    mod.out$mod.gof[i,"mae"] <- mae(data$casos, mod)
    mod.out$mod.gof[i,"mae_vs_base"] <- mae.diff(data$casos, mod, base)
    mod.out$mod.gof[i,"rsq"] <- rsq(mod, int)
    mod.out$mod.gof[i,"formula"] <- form
    
    # store fitted values, fixed effects and random effects
    mod.out$fitted[[i]] <- mod$summary.fitted.values
    mod.out$fixed[[i]] <- mod$summary.fixed
    mod.out$random[[i]] <- mod$summary.random
    
    # save intermediate results every 100 models
    if (i %% 100 == 0) {
      filename <- paste0(fname, "_", i, ".rds")
      saveRDS(mod.out, filename)
    }
  }
  return(mod.out)
}


## Model evaluation functions --------------------------------------------------

# create goodness of fit matrix from mod.out output to support plotting
# each gof metric as raster
gof_matrix <- function(mod.out, temp_var=""){
  # create goodness of fit dataframe
  gof_tmp <- data.frame(temp_lag=rep(1:nlag, floor(nlag/2)^2),
                        spi_long=rep((nlag/2 + 1):nlag, each=(nlag/2)*nlag),
                        spi_short=rep(rep(1:(nlag/2), each=nlag), (nlag/2)),
                        dic=NA, waic=NA, mae=NA, rsq=NA)
  gof_tmp$spi_combo <- paste(gof_tmp$spi_long, gof_tmp$spi_short, sep=".")
  gof_tmp <- gof_tmp %>% relocate(spi_combo, .after=temp_lag)
  
  # goodness of fit variables
  gof_vars <- c("dic", "waic", "mae", "rsq")
  
  # populate goodness of fit dataframe from mod.out output
  for (g in gof_vars){
    for (tlag in 1:6){
      for (spil in 4:6){
        for (spis in 1:3){
          v <- paste0(temp_var, ".", tlag, " * ", "spi6.", spil, " * ", "spi6.",
                      spis)
          
          gof_tmp[gof_tmp$temp_lag == tlag & gof_tmp$spi_long == spil &
                    gof_tmp$spi_short == spis, g] <-
            mod.out$mod.gof[mod.out$mod.gof$vars==v, g]
        }
      }
    }
  }
  
  return(gof_tmp)
}


## Plotting functions ----------------------------------------------------------

# function to plot fitted values
# *** FACET WRAP WITH uf VARIABLE FOR STATE LEVEL - CHECK!!!
plot.fit <- function(mod, title=""){
  mod %>%
    #mod[["summary.fitted.values"]] %>% 
    bind_cols(data %>% dplyr::select(date, cases)) %>%
    group_by(date, uf) %>% 
    summarise(cases=sum(cases, na.rm=TRUE),
              mean=sum(mean),
              lower=sum(`0.025quant`),
              upper=sum(`0.975quant`), .groups="drop") %>%
    mutate(date=as.Date(date)) %>% 
    ggplot(aes(x=date)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), fill="deeppink2", alpha=0.4)+
    geom_line(aes(y=mean, colour="Fitted")) +
    geom_line(aes(y=cases, colour="Observed")) +
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    facet_wrap(~uf, scales="free") +
    labs(x="", y="", colour="") +
    scale_color_manual(values = c("Observed"="#006666", "Fitted"="deeppink2")) +
    ggtitle(title) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="top",
          plot.title=element_text(hjust=0.5), panel.border = element_blank())
}

# plot temporal random effects
plot.temp.res <- function(mods, titles=""){
  out <- NULL
  for(i in 1:length(mods)){
    add <- mods[[i]][["month"]] %>%
      mutate(state=rep(unique(data$uf), each=52),
             week=rep(c(1:52), times=length(unique(data$uf))),
             model=titles[[i]])
    
    out <- bind_rows(out, add)
  }
  
  out %>%
    ggplot() +
    geom_pointrange(aes(y=mean, ymin=`0.025quant`,
                        ymax=`0.975quant`, x=factor(ID),
                        col=model), position=position_dodge(0.3)) +
    scale_x_discrete(labels=c(41:52, 1:40)) +
    geom_hline(yintercept=0, linetype="dashed", col="grey") +
    facet_wrap(~state, scales="free") +
    labs(x="Epi Week", y=expression(delta[m(t)])) +
    theme_classic() +
    theme(axis.text.x=element_text(size=9, hjust=1),
          axis.title=element_text(size=12))
}

# plot yearly random effect
plot.year.res <- function(mods, titles="", xlabs=""){
  out <- NULL
  for(i in 1:length(mods)){
    add <- mods[[i]][["year_index"]] %>%
      mutate(type="Interannual Random Effect",
             time=1:nrow(mods[[i]][["year_index"]]),
             model=titles[[i]])
    
    out <- bind_rows(out, add)
  }
  
  out %>%
    ggplot() +
    geom_pointrange(aes(y=mean, ymin=`0.025quant`,
                        ymax=`0.975quant`, x=factor(ID),
                        col=model), position=position_dodge(0.3)) +
    scale_x_discrete(labels=xlabs) +
    geom_hline(yintercept=0, linetype="dashed", col="grey") +
    facet_wrap(~type, scales="free") +
    labs(x="Dengue Year", y=expression(gamma[a(t)])) +
    theme_classic() +
    theme(axis.text.x=element_text(size=9, angle=45, hjust=1),
          axis.title=element_text(size=12))
}

# plot nonlinear effect
plot.nonlinear.ef <- function(mod, idx, var="", titles=""){
  mod[[idx]] %>%
    ggplot() +
    geom_line(aes(y=mean, x=ID), colour="#006666") +
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`, x=ID),
                alpha=0.4, fill="#17becf") +
    labs(x=paste("Standardised",var), y="Effect", title=titles) +
    theme_classic() +
    theme(axis.text.x=element_text(size=10, hjust=1),
          axis.title=element_text(size=10), plot.title=element_text(hjust=0.5))
}


# heatmap of dic with spi_combo vs temp
heatmap.dic <- function(gof_matrix, var_title=""){
  gof_matrix %>%
    ggplot(aes(x = factor(spi_combo), y = factor(temp_lag), fill = dic)) +
    geom_raster() +
    ylab(paste(var_title, "Temperature Lag")) + 
    xlab("SPI-6 Lag Combination") +
    scale_fill_gradientn(name = "DIC", colours = rev(brewer.pal(9, "Greens"))) + 
    scale_y_discrete(expand = expansion(0), labels=c(0:5)) +
    scale_x_discrete(expand = expansion(0)) +
    theme_bw()
}

# heatmap of waic with spi_combo vs temp
heatmap.waic <- function(gof_matrix, var_title=""){
  gof_matrix %>%
    ggplot(aes(x = factor(spi_combo), y = factor(temp_lag), fill = waic)) +
    geom_raster() +
    ylab(paste(var_title, "Temperature Lag")) + 
    xlab("SPI-6 Lag Combination") +
    scale_fill_gradientn(name = "WAIC", colours = rev(brewer.pal(9, "Blues"))) + 
    scale_y_discrete(expand = expansion(0), labels=c(0:5)) +
    scale_x_discrete(expand = expansion(0)) +
    theme_bw()
}

# heatmap of mae with spi_combo vs temp
heatmap.mae <- function(gof_matrix, var_title=""){
  gof_matrix %>%
    ggplot(aes(x = factor(spi_combo), y = factor(temp_lag), fill = mae)) +   
    geom_raster() +
    ylab(paste(var_title, "Temperature Lag")) + 
    xlab("SPI-6 Lag Combination") +
    scale_fill_gradientn(name = "MAE",
                         colours = rev(brewer.pal(9, "Oranges"))) + 
    scale_y_discrete(expand = expansion(0), labels=c(0:5)) +
    scale_x_discrete(expand = expansion(0)) +
    theme_bw()
}

# heatmap of rsq with spi_combo vs temp
heatmap.rsq <- function(gof_matrix, var_title=""){
  gof_matrix %>%
    ggplot(aes(x = factor(spi_combo), y = factor(temp_lag), fill = rsq)) +   
    geom_raster() +
    ylab(paste(var_title, "Temperature Lag")) + 
    xlab("SPI-6 Lag Combination") +
    scale_fill_gradientn(name = expression({R[LR]}^2),
                         colours = brewer.pal(9, "Purples")) + 
    scale_y_discrete(expand = expansion(0), labels=c(0:5)) +
    scale_x_discrete(expand = expansion(0)) +
    theme_bw()
}

# plot 4 goodness of fit heatmaps as single plot
multi.heatmap.gof <- function(gof_matrix, title=""){
  g <- ggarrange(heatmap.dic(gof_matrix), heatmap.waic(gof_matrix),
                 heatmap.mae(gof_matrix), heatmap.rsq(gof_matrix),
                 ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), align = "hv",
                 hjust = -1, vjust = 1, font.label = list(size = 18, face = "plain"))
  annotate_figure(g, top=text_grob(title, color = "black", face = "bold", size = 18))
}