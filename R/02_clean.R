# Load libraries ----------------------------------------------------------
library("tidyverse")
library("dplyr")
library("ggpubr")
library("viridis")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data <- as_tibble(read_tsv(file = "data/01_my_data.tsv"))


# Eliminate unnecessary variables --------------------------------------------------------
my_data_clean <- my_data  %>% 
  select(-(matches("Groupnumber_|Population_|Sex_|groupnumeric")))

# Check for NAs --------------------------------------------------------
NAs <- na_count(my_data_clean)

# Describe data --------------------------------------------------------
data_distributions(my_data_clean)
data_boxplots(my_data_clean)

# Wrangle data ------------------------------------------------------------
# my_data_clean <- my_data # %>% ...

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")