# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
gene_expr_data <- my_data_clean_aug %>% 
  select(matches("LogR|ID|Population")) %>% 
  drop_na() %>% 
  pivot_longer(cols = -(c("ID","Population")),
               names_to = "gene",
               values_to = "expr_lvl")
  

# Model data
gene_expr_model <- gene_expr_data %>% 
  mutate(mdl = map(data, ~glm("Population" ~ "expr_lvl",
                              data = .,
                              family = binomial(link = "logit"))))

# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)