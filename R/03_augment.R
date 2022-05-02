
# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug <- my_data_clean  %>% 
    mutate(energy_consumed = (((power/1000) * time.sec) *(3.6*10^6)),#energy consumed in joule
           efficiency = distance/energy_consumed,
           distance_class = case_when (distance <= 1000 ~ "short",
                                      distance > 1000 ~ "long"))



# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")
