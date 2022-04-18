# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data <- read_tsv(file = "data/01_my_data.tsv")


# Wrangle data ------------------------------------------------------------

# Eliminating unnecessary variables
my_data_clean <- my_data  %>% 
  select(-(matches("Groupnumber_|Population_|Sex_|groupnumeric")))

# Checking for NA values
na_count(my_data_clean)

my_data_clean %>% 
  group_by(Population) %>% 
  ggplot(aes(x = ID,
           y = premass,
           color = Sex)) +
  geom_point() +
  facet_wrap(~Population,
             scales = 'free') +
  theme_project() +
  rotate_x() +
  rotate_y()

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")
