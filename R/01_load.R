# Load libraries ----------------------------------------------------------
library("tidyverse")
library("viridis")
library("patchwork")
library("ggridges")
library("fs")
library("caret")
library("purrr")
library("broom")
library("dplyr")
library(cowplot)

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

expression_raw<- expression_raw %>% 
          pivot_wider(id_cols = Monarch,
                      id_expand = FALSE,
                      names_from = Gene,
                      values_from = "LogRelexpr18S28S",
                                      "Groupnumber",
                                      "Sex",
                                      "Population")
