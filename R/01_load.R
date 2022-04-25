# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
morphology_raw <- as_tibble(read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Flight_And_Wing_Morphology_Data.csv"))
expression_raw <- as_tibble(read.csv(file = "data/_raw/Talla_East_West_North_America_Monarch_Genomics_Gene_Expression_Data.csv"))


# Wrangle data ------------------------------------------------------------

#Pivoting expression data to obtain one column for every gene
#The variables already present in the other dataset are not included
#Groupnumber is not informative and is not included
expression_wide <- expression_raw %>% 
  pivot_wider(id_cols = Monarch,
              names_from = Gene,

              values_from = c(LogRelexpr18S28S, Groupnumber, Population, Sex))


#Renaming ID column so that both dataframes match
expression_wide <- rename(expression_wide, "ID" = "Monarch")

#Joining expression and morphology data by ID
final_data <- full_join(expression_wide,morphology_raw,by="ID")

# Renaming PC columns
final_data <- rename(final_data, "PC1_wing_size" = "PC1")
final_data <- rename(final_data, "PC2_wing_shape" = "PC2")

# Write data --------------------------------------------------------------
write_tsv(x = final_data,
          file = "data/01_my_data.tsv")

