# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
morphology_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Flight_And_Wing_Morphology_Data.csv")
expression_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Gene_Expression_Data.csv")


# Wrangle data ------------------------------------------------------------
my_data <- my_data_raw # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data.tsv")