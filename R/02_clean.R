# Load libraries ----------------------------------------------------------
library("tidyverse")
library("dplyr")
library("ggpubr")

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

#generate a list with the names of the columns that contain numeric values
numeric_ones <- my_data_clean %>%
  select(where(is.numeric)) %>%
  colnames() %>%
  set_names() #this function belongs to purr package and uses the values of vector as names

#generate plot iterated for all variables
plots = map(numeric_ones, 
            ~datadistribution_plot("Population",
                                   ., 
                                   my_data_clean))


#generate images of the plots
map(names(plots), ~save_plot_list(plots, .x)) 



# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")

