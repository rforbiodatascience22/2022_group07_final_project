# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# make sure that the gender columns contain the same values, 
#in order to keep only one of those

Sex<- data.frame(data_wide_expression$Sex_379, 
                   data_wide_expression$Sex_203,
                   data_wide_expression$Sex_C2,
                   data_wide_expression$Sex_C4,
                   data_wide_expression$Sex_C5,
                   data_wide_expression$Sex_C7)



# Wrangle data ------------------------------------------------------------
data_wide_expression[data_wide_expression$Sex_379 == data_wide_expression$Sex_203 ,"winner"] <- "a"


# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")