# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
morphology_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Mona
                           rch_Genomics_Flight_And_Wing_Morphology_Data.csv")
expression_raw <- read.csv(file = "data/_raw/Talla_East_West_North_America_Mona
                           rch_Genomics_Gene_Expression_Data.csv")


# Wrangle data ------------------------------------------------------------

# Pivoting expression data to obtain one column for every gene
data_wide_expression <- expression_raw %>% 
  pivot_wider(id_cols = Monarch,
              names_from = Gene,
              values_from = c(LogRelexpr18S28S,Groupnumber,Population,Sex))

# Renaming ID column so that both dataframes match
data_wide_expression <- rename(data_wide_expression, "ID" = "Monarch")

# Joining expression and morphology data by ID
final_data <- full_join(data_wide_expression,morphology_raw,by="ID")

# Renaming PC columns
final_data <- rename(final_data, "PC1_wing_size" = "PC1")
final_data <- rename(final_data, "PC2_wing_shape" = "PC2")

# Write data --------------------------------------------------------------
write_tsv(x = final_data,
          file = "data/01_my_data.tsv")