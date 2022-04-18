# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data --------------------------------------------------------------
my_data_clean_aug %>% ...

# for heatmap
targets <- my_data_clean %>% select(matches("LogRelexpr|ID"))

ggplot(targets,
       mapping = aes(x = ,
                     y = ID)) +
  geom_tile()

# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)