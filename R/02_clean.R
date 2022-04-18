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
na_tibble <- na_count(my_data_clean)

my_data_clean %>% 
  filter(!is_null(.)) %>% 
  group_by(Population) %>% 
  count(ID) %>% 
  ggplot(aes(x = Population,
             y = n)) +
  geom_boxplot()

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")
