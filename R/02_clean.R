# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data <- read_tsv(file = "data/01_my_data.tsv")


# Wrangle data ------------------------------------------------------------
#"sex","population","Groupnumeric" are repeated from the pivoting and joining of the two datasets so we delete them.
#also "groupnumeric" variable is deleted as it is not necessary for our research.
my_data_clean <- my_data %>%
  select(-starts_with(c("Sex_", "Population_","Groupnumber_","groupnumeric")))

#analyse the dataset for NA values 
na_count <- my_data_clean %>%
  select(everything()) %>%  
  summarise_all(~(sum(is.na(.))))

#generate a list with the names of the columns that contain numeric values
numeric_ones <- my_data_clean %>% 
  select(where(is.numeric)) %>% 
  colnames() %>% 
  set_names()#this function belogs to purr package and uses the values of vector as names


# Basic violin+box plot
#ggplot(my_data_clean, aes(x = Sex, y = distance)) + 
#geom_violin() + #alternatively jitter
#geom_boxplot(width=0.1)

#generate plot iterated for all variables
source(file = "R/99_project_functions.R")
plots = map(numeric_ones, ~datadistribution_plot("Population", ., my_data_clean) )


# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")
