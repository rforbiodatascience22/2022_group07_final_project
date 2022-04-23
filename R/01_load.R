# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
morphology_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Flight_And_Wing_Morphology_Data.csv")
expression_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Gene_Expression_Data.csv")


# Wrangle data ------------------------------------------------------------
data_wide_expression <- expression_raw  %>% 
  pivot_wider(id_cols = Monarch,
               names_from = Gene,
               values_from = c(LogRelexpr18S28S,Population,Sex,Groupnumber))

data_wide_expression <- rename(data_wide_expression,"ID"="Monarch")
data_wide_expression

full_dataset <-data_wide_expression %>% 
  full_join(morphology_raw, by = "ID")
full_dataset



# Write data --------------------------------------------------------------
write_tsv(x = full_dataset,
          file = "data/01_my_data.tsv")
