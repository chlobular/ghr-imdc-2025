<a href='https://www.bsc.es/es'><img src='https://www.cmb.cat/wp-content/uploads/2014/01/BSC-Logo.jpg' align="right" height="100" width="100" /></a>

# Global Health Resilience model for the 2nd Infodengue-Mosqlimate Dengue Challenge (IMDC) 2025

## Team and Contributors

The Global Health Resilience (GHR) group is based at the Barcelona Supercomputing Center (BSC).

**[Carles Milà](https://www.bsc.es/mila-garcia-carles)**<a href="https://orcid.org/0000-0003-0470-0760">
  <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" />  
</a> Barcelona Supercomputing Center (BSC), Spain

**[Chloe Fletcher](https://www.bsc.es/fletcher-chloe)**<a href="https://orcid.org/0000-0002-6705-7605">
  <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" />  
</a> Barcelona Supercomputing Center (BSC), Spain  
Department of Medicine & Life Sciences, Universitat Pompeu Fabra, Spain

**[Giovenale Moirano](https://www.bsc.es/moirano-giovenale)**<a href="https://orcid.org/0000-0001-8748-3321">
  <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" />  
</a> Università degli Studi di Torino, Italy  
Barcelona Supercomputing Center (BSC), Spain

**[Rachel Lowe](https://www.bsc.es/lowe-rachel)**<a href="https://orcid.org/0000-0003-3939-7343">
  <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" />  
</a> Barcelona Supercomputing Center (BSC), Spain  
Catalan Institution for Research and Advanced Studies (ICREA), Spain  
London School of Hygiene and Tropical Medicine, United Kingdom


## Repository Structure

This repository contains the code, outputs and figures for the GHR team in the 2nd IMDC 2025.

<pre lang="markdown">
<b>sprint2025/</b>
│
├── <b>R/                              # R scripts for functions, data processing, modelling, and cross validation</b>
│   ├── 00_RE_formuni.R             # Specify random effects for univariable analysis
│   ├── 00_functions.R              # Outline helper functions 
│   ├── 01_data_preparation.R       # Data loading and pre-processing
│   ├── 02_RE_univariate.R          # Fitting univariable random-effects models
│   ├── 03_RE_complete.R            # Fitting full random-effects models
│   ├── 04_FE_uni.R                 # Fitting univariable mixed-effects models
│   ├── 05_FE_bi.R                  # Fitting bivariable mixed-effects models
│   ├── 06_FE_bi_slopes.R           # Fitting bivariate mixed-effects models with random slopes
│   ├── 07_FE_multi.R               # Fitting multivariable mixed-effects models
│   ├── 08_FE_interact.R            # Fitting multivariable mixed-effects models with interactions
│   ├── 09_FE_oni.R                 # Fitting multivariable mixed-effects models with Oceanic Niño Index (ONI) fixed effects
│   ├── 10_FE_trend.R               # Fitting multivariable mixed-effects models with trend-based effects
│   ├── 11_CV.R                     # Cross-validation (rolling-origin)
│   ├── 11_CV/                      # Cross-validation outputs (round 1)
│   ├── 12_CV2/                     # Cross-validation outputs (round 2)
│   ├── 13_CV3/                     # Cross-validation outputs (round 3)
│   ├── XX_ghrpredict.R             # Workflow of GHRpredict function
│   ├── XX_onebasis.R               # Application of one basis approach in GHRmodel
│   └── run.sh                      # Shell script for running in BSC MN5
│
├── <b>Rmd/                            # R Markdown files and rendered HTML reports</b>
│   ├── 01_exploratory.Rmd/.html    # Exploratory data analysis and visualisations
│   ├── 02_uniRE.Rmd/.html          # Results for univariable random-effects models
│   ├── 03_multiRE.Rmd/.html        # Results for multivariable random-effects models
│   ├── 04_uniFE.Rmd/.html          # Results for univariable mixed-effects models
│   ├── 05_biFE.Rmd/.html           # Results for bivariable mixed-effects models
│   ├── 06_biFE_slopes.Rmd/.html    # Results for bivariable mixed-effects models with random slopes
│   ├── 07_multiFE.Rmd/.html        # Results for multivariable mixed-effects models
│   ├── 08_interFE.Rmd/.html        # Results for multivariable mixed-effects models with interactions
│   ├── 09_oniFE.Rmd/.html          # Results for multivariable mixed-effects models with ONI fixed effects
│   ├── 10_trendFE.Rmd/.html        # Results for multivariable mixed-effects models with trend-based effects
│   ├── 11_CV.Rmd/.html             # Cross-validation results (round 1)
│   ├── 12_CV2.Rmd/.html            # Cross-validation results (round 2)
│   ├── 12_CV3.Rmd/.html            # Cross-validation results (round 3)
│
├── <b>figures/                        # Saved figures</b>
│   ├── CV_modtab.png               # Table of cross-validation models (round 1)
│   ├── CV_modtab2.png              # Table of cross-validation models (round 2)
│   └── CV_modtab3.png              # Table of cross-validation models (round 3)
</pre>


## Dependencies

dplyr, tidyr, lubridate, zoo, sf, spdep, INLA, splines, here, ggplot2, lares, cowplot, purrr, forcasts, GHRexplore, GHRmodel *(soon on CRAN)*, GHRpredict *(soon on CRAN)*


## Data and Variables

The following variables from documented data sources were used to fit the models, and establish the final prediction model. Note, the following variables were standardised to support convergence in R-INLA: tas, tasmin, tasmax, tas3, tasmin3, tasmax3, tas6, tasmin6, tasmax6, tas12, tasmin12, tasmax12, prlr, prlr3, prlr6, prlr12. The n-month variables represent averages or totals over the preceding n months, ending in the month indicated. These n-monthly variables are matched such that the final month in the time period aligns with the calendar month of day 4 in that particular epidemiological week. All variables were lagged from 0 to 6 months, using the suffix **.ln** where 0 ≤ n ≤ 6.

| Variable | Description | Source | Provided? |
| --- | --- | --- | --- |
| **casos** | Weekly dengue case counts per health region | SINAN | Yes |
| **tas** | Monthly averaged daily mean temperature (°C) per health region | ERA5-Land | No |
| **tasmin** | Monthly averaged daily minimum temperature (°C) per health region | ERA5-Land | No |
| **tasmax** | Monthly averaged daily maximum temperature (°C) per health region | ERA5-Land | No |
| **tasan** | Mean temperature anomaly (°C) per health region per calendar month | ERA5-Land | No |
| **tas3** | 3-monthly averaged daily mean temperature (°C) per health region | ERA5-Land | No |
| **tasmin3** | 3-monthly averaged daily minimum temperature (°C) per health region | ERA5-Land | No |
| **tasmax3** | 3-monthly averaged daily maximum temperature (°C) per health region | ERA5-Land | No |
| **tasan3** | Mean temperature anomaly (°C) per health region per 3-month period | ERA5-Land | No |
| **tas6** | 6-monthly averaged daily mean temperature (°C) per health region | ERA5-Land | No |
| **tasmin6** | 6-monthly averaged daily minimum temperature (°C) per health region | ERA5-Land | No |
| **tasmax6** | 6-monthly averaged daily maximum temperature (°C) per health region | ERA5-Land | No |
| **tasan6** | Mean temperature anomaly (°C) per health region per 6-month period | ERA5-Land | No |
| **tas12** | Annually averaged daily mean temperature (°C) per health region | ERA5-Land | No |
| **tasmin12** | Annually averaged daily minimum temperature (°C) per health region | ERA5-Land | No |
| **tasmax12** | Annually averaged daily maximum temperature (°C) per health region | ERA5-Land | No |
| **tasan12** | Mean temperature anomaly (°C) per health region per 12-month period | ERA5-Land | No |
| **prlr** | Total monthly precipitation (mm) per health region | ERA5-Land | No |
| **prlr3** | Total 3-monthly precipitation (mm) per health region | ERA5-Land | No |
| **prlr6** | Total 6-monthly precipitation (mm) per health region | ERA5-Land | No |
| **prlr12** | Total annual precipitation (mm) per health region | ERA5-Land | No |
| **spi1** | 1-month Standardised Precipitation Index (SPI-1) per health region | ERA5-Land | No |
| **spi3** | 3-month Standardised Precipitation Index (SPI-3) per health region | ERA5-Land | No |
| **spi6** | 6-month Standardised Precipitation Index (SPI-6) per health region | ERA5-Land | No |
| **spi12** | 12-month Standardised Precipitation Index (SPI-12) per health region | ERA5-Land | No |
| **spei1** | 1-month Standardised Precipitation-Evapotranspiration Index (SPEI-1) per health region | ERA5-Land | No |
| **spei3** | 3-month Standardised Precipitation-Evapotranspiration Index (SPEI-3) per health region | ERA5-Land | No |
| **spei6** | 6-month Standardised Precipitation-Evapotranspiration Index (SPEI-6) per health region | ERA5-Land | No |
| **spei12** | 12-month Standardised Precipitation-Evapotranspiration Index (SPEI-12) per health region | ERA5-Land | No |
| **nino** | Monthly averaged Niño 3.4 sea surface temperature anomaly | NOAA ERSST v5 | No |
| **oni** | Oceanic Niño Index (ONI), 3-monthly averaged Niño 3.4 sea surface temperature anomaly | NOAA ERSST v5 | No |
| **nino6** | 6-monthly averaged Niño 3.4 sea surface temperature anomaly | NOAA ERSST v5 | No |
| **nino12** | 12-monthly averaged Niño 3.4 sea surface temperature anomaly | NOAA ERSST v5 | No |
| **nino_year** | Annual Niño classification (El Niño, Neutral, La Niña) from EW 41 to EW 40 | NOAA ERSST v5 | No |
| **koppen** | Köppen climate classification per health region | Köppen Brasil | Yes |
| **biome** | Dominant biome per health region | Embrapa Agricultura Digital | Yes |
| **pop** | Annual population per health region | SVS | Yes |

*`*Note, the ONI values in our dataset were right-aligned, instead of centred (as is standard), to ensure consistency with the nino6 and nino12 variables. This means that the ONI values differ to datasets published by NOAA such that it represents the NOAA values lagged by 1 month. Therefore, ONI lagged 7 months (as standard) is denoted as oni.l6 in our dataset.`*


## Modelling Approach

For our modelling approach, we specified a Bayesian hierarchical mixed-effects model using dengue case counts $y_{s,t}$ as our response variable per health region ($s$) per epidemiological week in the time series ($t$). To account for potential overdispersion, case counts were assumed to follow a negative binomial distribution such that:

$$
y_{s,t} \mid \mu_{s,t} \sim \text{NegBin}(\mu_{s,t}, \kappa)
$$

$$
\log(\mu_{s,t}) = \log(p_{s,a(t)}) + \log(\rho_{s,t})
$$

with distribution mean $\mu_{s,t}$ and overdispersion parameter $\kappa$. The distribution mean represents the population per 100,000 $p_{s,a(t)}$ in a given health region $s$ and year $a(t)$ multiplied by the dengue incidence rate $\rho_{s,t}$.

For our final model, we formulated a three-way interaction between the 6-month temperature anomaly lagged 1 month (tasan6.l1, $X_T$), the SPEI-12 lagged by 3 months (spei12.l3, $X_L$), and the SPEI-3 lagged by 1 month (spei3.l1, $X_S$), capturing individual effects ($\beta_T$, $\beta_L$, $\beta_S$) and interacting effects ($\beta_{T,L}$, $\beta_{T,S}$, $\beta_{L,S}$, $\beta_{T,L,S}$). This follows the long-short-lag interaction approach applied to predict dengue outbreaks in Barbados (Fletcher et al., in press; Fletcher et al., 2025), which was adapted for the GHR model in the first sprint to predict dengue cases in Brazil (Araujo et al., in review). For the interaction, each of the 7 terms were specified using random slopes per Köppen ($K$) climate classification (Af, Am, As, Aw, BSh, Cfa, Cfb, Cwa, Cwb), giving different effect sizes per classification. The model also includes three additive variables: the 6-month averaged absolute temperature lagged by 1 month (tas6.l1, $X_A$) using random slopes for separate effects by Köppen classification ($\beta_A$), the ONI lagged 7 months (oni.l6*, $X_N$) using a nonlinear effect with 10 equal cuts ($\beta_N$), and a binary cut-off variable indicating if the week preceeds 2018 or not ($X_C$) with a linear effect ($\beta_C$). Additionally, the model comprises an intercept ($\alpha$), a temporal random effect $\delta_{w(t)}$ to account for weekly variation in dengue cases specified as a cyclic second-order random walk (RW2) model for each epidemiological week ($w(t)$), and a spatial random effect specified as a modified Besag-York-Mollie (BYM2) model at the health region level which includes structured ($u_s$) and unstructured ($v_s$) components.

$$
\log(\rho_{s,t}) = \alpha + (\beta_T X_T + \beta_L X_L + \beta_S X_S + \beta_{T,L} X_T X_L + \beta_{T,S} X_T X_S + \beta_{L,S} X_L X_S + \beta_{T,L,S} X_T X_L X_S + \beta_A X_A)_K + \beta_N X_N + \beta_C X_C + \delta_{w(t)} + u_s + v_s
$$

During model fitting and cross-validation, we also tested this formulation with a temporal random effect $\delta_{a(t)}$ to account for interannual variation in dengue cases as an independent and identically distributed (IID) model for each epidemiological year (spanning EW41 to EW40). However, the models without the interannual random effect were found to have an enhanced predictive performance during cross-validation.


## Model Selection and Cross-Validation

We started model fitting by conducting a comprehensive random-effects model selection process (scripts 02-03). Firstly, we performed a univariable analysis, testing different specifications of spatial, weekly, yearly and time-trend random effects (script 02), as defined in script 00. By using goodness-of-fit metrics, we selected 3 weekly random effects, 2 yearly random effects, 2 spatial random effects, and 0 time-trend random effects to test all combinations of multivariable random-effects models (script 03). Our final multivariable random-effects model was identified by inspecting the Deviance Information Criterion (DIC), Watanabe-Akaike Information Criterion (WAIC), the Log Mean Score (LMS) and Mean Absolute Error (MAE); the fitted vs observed values aggregated by state; and the behaviour of the random effects.

Subsequently, we conducted a comprehensive mixed-effects model selection process (scripts 04-10), where we performed a univariable analysis (script 04) with different covariates (as outlined in Table), bivariable analysis (script 05), bivariable analysis with random slopes (script 06), multivariable (3+) analysis (script 07), interaction model analysis (script 08), incorporate of the interaction model with ONI (script 09) and exploration of fixed-effect time-trend variables (script 10). 

Lastly, we selected our best prediction model by testing full mixed-effects models with a rolling-origin cross-validation process. We predicted a 12-month season (EW41 to EW40), issuing the forecast from EW25, replicating the real-world forecasting challenge in Brazil. For our internal cross-validation, we opted to use observed climate covariates instead of forecasted climate covariates to evaluate the effectiveness of the model without taking climate forecast uncertainty into consideration.


## Prediction and Validation Tests

- Aggregate by state
- Data up until EW25
- Use of forecasts and climatologies


## References

**Fletcher C**, Moirano G, Alcayna T, Rollock L et al. Compound and cascading effects of climatic extremes on dengue outbreak risk in the Caribbean: an impact-based modelling framework with long-lag and short-lag interactions, *The Lancet Planetary Health* in press.  
**Fletcher C**, Moirano G, Alcayna T, Rollock L et al. Data and R code to accompany "Compound and cascading effects of climatic extremes on dengue outbreak risk in the Caribbean: an impact-based modelling framework with long-lag and short-lag interactions" (version v1.0.0). Zenodo 2025. https://doi.org/10.5281/zenodo.15731719  
**Araujo EC**, Carvalho LM, Ganem F, Vacaro LB et al. Leveraging probabilistic forecasts for dengue preparedness and control: the 2024 Dengue Forecasting Sprint in Brazil, medRxiv 2025.05.12.25327419 [Preprint], 2025.