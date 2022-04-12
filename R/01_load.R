# Load libraries ----------------------------------------------------------
library("tidyverse")
library(dplyr)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
morphology_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Flight_And_Wing_Morphology_Data.csv")
expression_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Gene_Expression_Data.csv")


# Wrangle data ------------------------------------------------------------
# my_data <- my_data_raw # %>% ...

# Name ID columns the same in the two raw datasets
expression_raw <- rename(expression_raw,
                         ID = Monarch)
# Merge data
data_raw <- expression_raw %>% 
  full_join(morphology_raw,
            by = "ID")



# Write data --------------------------------------------------------------
# write_tsv(x = my_data,
#           file = "data/01_my_data.tsv")