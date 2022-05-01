# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
gene_expr_data <- my_data_clean_aug %>% 
  select(matches("LogR|Population")) %>% 
  drop_na() %>% 
  mutate(Population = case_when(Population == "east" ~ 0,
                                Population == "west" ~ 1)) %>% 
  pivot_longer(cols = -"Population",
               names_to = "gene",
               values_to = "expr_lvl") %>% 
  group_by(gene) %>%
  nest %>% 
  ungroup 
  

# Model data -------------------------------------------------------------
gene_expr_model <- gene_expr_data %>% 
  mutate(mdl = map(data, ~glm(Population ~ expr_lvl,
                                        data = .x,
                                        family = binomial(link = "logit")))) %>% 
  mutate(mdl_tidy = map(mdl, ~tidy(.x, conf.int = TRUE))) %>% 
  unnest(mdl_tidy) %>% 
  filter(str_detect(term, "expr_lvl"))

#Analysis -----------------------------------------------------------------
gene_expr_analysis <- gene_expr_model %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"),
         gene_label = case_when(identified_as == "Significant" ~ gene,
                                identified_as == "Non-significant" ~ "")) %>% 
  mutate(neg_log10_p = -log10(p.value))

#Visualize ---------------------------------------------------------------
gene_expr_result = gene_expr_analysis %>% 
  ggplot(aes(x = gene,
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

ggsave("gene_expr.png", path = image_path, device = "png")


# Write data --------------------------------------------------------------
write_tsv(x = gene_expr_analysis,
          file = "data/04_gene_expr_analysis.tsv")
