#Remember to remove libraries
library(dplyr)
library(tidyverse)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv",
                              show_col_types = FALSE)
gene_expr_data <- read_tsv(file = "data/04_gene_expr_data.tsv",
                           show_col_types = FALSE)

# Wrangle data ------------------------------------------------------------
#creating tibble for gene expression analysis
gene_expr <- gene_expr_data %>%
  select(-matches("ID|Sex")) %>% 
  mutate(Population = case_when(Population == "east" ~ 0,
                                Population == "west" ~ 1)) %>% 
  group_by(Genes) %>%
  nest %>% 
  ungroup 

remove(gene_expr_data)

# Model data -------------------------------------------------------------
#logistic regression model for correlation of gene expression and population
gene_expr_model <- gene_expr %>% 
  mutate(mdl = map(data,
                   ~glm(Population ~ Expression, 
                        data = .x, 
                        family = binomial(link = "logit")))) %>% 
  mutate(mdl_tidy = map(mdl,
                        ~tidy(.x,
                              conf.int = TRUE))) %>% 
  unnest(mdl_tidy) %>% 
  filter(str_detect(term, "Expression"))

remove(gene_expr)

# Analysis -----------------------------------------------------------------
#creating labels based on significance of analysis
gene_expr_analysis <- gene_expr_model %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"), 
         gene_label = case_when(identified_as == "Significant" ~ Genes,
                                identified_as == "Non-significant" ~ "")) %>% 
  mutate(neg_log10_p = -log10(p.value))

remove(gene_expr_model)

# Visualize ---------------------------------------------------------------
#plotting the significance values
gene_expr_result = gene_expr_analysis %>% 
  ggplot(aes(x = Genes,
             y = neg_log10_p,
             colour = identified_as,
             label = gene_label)) + 
  geom_point(alpha = 0.5,
             size = 2) +
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed") +
  theme_project() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom") +
  labs(x = "Gene",
       y = "Minus log10(p)") 

ggsave("05_gene_expression.png",
       path = image_path,
       device = "png")


# PCA without genes----------------------------------------------------------------------

#Genes were removed because the expression levels were not calculated for 
#most observations

#Create a dataset with one-hot encoding and with combined variables removed
#Which means we kept only the "basic" variables
#Various PCAs will be performed with various classes as targets for the visualizations
#Since we have those classes in the datasets, the relative variables will
#be removed prior to each analysis

PCA_data <- my_data_clean_aug %>% 
  select(-matches("ID|Gene|weightloss|time.sec|PC|energy_consumed|efficiency|distance_class")) %>% 
  as_tibble() %>% 
  mutate(value = 1)  %>%
  spread(Sex,
         value,
         fill = 0 ) %>%
  mutate(value = 1)  %>%
  spread(Population,
         value,
         fill = 0 )
# PCA population----------------------------------------------------------------------
#The aim of this section is to understand if it's possible to cluster
#the two populations thanks to the information included in the other variables

#Model PCA
pca_fit_population <- PCA_data %>%
  select(-match("east|west")) %>%  #Do other variables include this info?
  prcomp(scale = TRUE)



