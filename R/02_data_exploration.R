### ==========================================================================
#### DATA VISUALISATION AND EXPLORATION #####
### ==========================================================================

#      This script focuses on visualising and
#      exploring the harmonised data.

#      Code written by:
#      Chloe Fletcher (BSC)
#      Giovenale Moirano (BSC)
#      Rachel Lowe (BSC)

## -----------------------------------------
### Source packages, functions and data
## -----------------------------------------
# load packages and functions
source("lsl_interaction_brazil/functions/00_packages.R")
source("lsl_interaction_brazil/functions/00_functions.R")


# read in harmonised data
data <- read.csv("weekly_data.csv")
date_start <- min(as.Date(data$date))
date_end <- max(as.Date(data$date))

# Data by state 

data_sate <-  data %>% group_by(uf, date) %>%
  summarise(casos = sum(casos), pop = sum (pop),
            temp = mean(temp_med_m), spi1 = mean(spei1_m),
            spei3 = mean (spei3_m), spei6 = mean(spei6_m),
            spei12 = mean(spei12_m), nino = mean(nino34_m),
            nino_year = mean(nino_year)) %>%
  mutate(date = as.Date(date), inc = (casos/pop) * 100000) 

## -----------------------------------------
### Plot time series
## -----------------------------------------
# Time series of dengue by state 
# Function to create time series plot
create_time_series_plot <- function(data, y_var, color, title) {
  data %>%
    ggplot(aes(x = date, y = .data[[y_var]])) + 
    geom_line(color = color) +
    xlab("Time") +
    ylab("") + 
    ggtitle(title) +
    scale_x_date(limits = c(date_start, date_end), expand = c(0,0),
                 date_labels = "%b %Y", date_breaks = "2 years") +
    theme_bw() + 
    theme(axis.text.x = element_text( hjust = 1),
          plot.title = element_text(size = 10, hjust = 0.5))
}

# Get the unique values of uf
unique_ufs <- unique(data_sate$uf)


# Loop through each uf and create plots
for (uf in unique_ufs[1:2]) {
  data_subset <- data_sate %>% filter(uf == !!uf)
  
  # Create each time series plot
  inc_plot <- create_time_series_plot(data_subset, "inc", "purple", "Dengue Incidence")
  temp_plot <- create_time_series_plot(data_subset, "temp", "red", "Temperature")
  spei6_plot <- create_time_series_plot(data_subset, "spei6", "brown", "SPEI-6")
  nino_plot <- create_time_series_plot(data_subset, "nino_year", "darkgreen", "Nino Index")
  
  # Combine the plots using patchwork
  combined_plot <- (inc_plot / temp_plot / spei6_plot / nino_plot) +
    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +  # Adjust heights if necessary
    plot_annotation(title = uf,
                    theme = theme(plot.title = element_text(size = 20, hjust = 0.52)))
  
  # Display the combined plot (adjust height of the combined plot as necessary)
  print(combined_plot)
}

