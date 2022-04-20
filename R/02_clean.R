# Load libraries ----------------------------------------------------------
library("tidyverse")
library("dplyr")
library("gridExtra")
library("ggpubr")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Describe data --------------------------------------------------------
# tempstat <- final_data %>%
#   summarise(across(
#     .cols = is.numeric, 
#     .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
#     .names = "{col}_{fn}"
#   ))
# tempstat <-cbind(row.names = "Mean", tempstat)
# sumstats <- describe(final_data)


# Wrangle data ------------------------------------------------------------
# my_data_clean <- my_data # %>% ...


myplots <- lapply(colnames(final_data), plot_distribution, data = final_data)
ggarrange(plotlist=myplots, nrow = 3, ncol = 3)
data_distributions(final_data)

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")