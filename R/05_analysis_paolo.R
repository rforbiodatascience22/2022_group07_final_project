# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv",
                              show_col_types = FALSE)
gene_expr_data <- read_tsv(file = "data/04_gene_expr_data.tsv",
                           show_col_types = FALSE)

# Wrangle data ------------------------------------------------------------
# creating tibble for gene expression analysis
gene_expr <- gene_expr_data %>%
  select(-matches("ID|Sex")) %>% 
  mutate(Population = case_when(Population == "east" ~ 0,
                                Population == "west" ~ 1)) %>% 
  group_by(Genes) %>%
  nest %>% 
  ungroup 

remove(gene_expr_data)

# Model data -------------------------------------------------------------
# logistic regression model for correlation of gene expression and population
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
# creating labels based on significance of analysis
gene_expr_analysis <- gene_expr_model %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"), 
         gene_label = case_when(identified_as == "Significant" ~ Genes,
                                identified_as == "Non-significant" ~ "")) %>% 
  mutate(neg_log10_p = -log10(p.value))

remove(gene_expr_model)

# Visualize ---------------------------------------------------------------
# plotting the significance values
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

# Genes were removed because the expression levels were not calculated for 
# most observations

# Create a dataset with one-hot encoding and with combined variables removed
# Which means we kept only the "basic" variables
# Various PCAs will be performed with various classes as targets for the visualizations
# Since we have those classes in the datasets, the relative variables will
# be removed prior to each analysis

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
# The aim of this section is to understand if it's possible to cluster
# the two populations thanks to the information included in the other variables

# Model PCA
PCA_fit_population <- PCA_data %>%
  select(-matches("east|west")) %>%  #Do other variables include this info?
  prcomp(scale = TRUE)

# Explained variance
PCA_population_explvar <- PCA_fit_population %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) +
  theme_project()

# Variables contribution
PCA_population_contribution <- fviz_contrib(PCA_fit_population,
                    "var",
                    axes = 1,
                    xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x() +
  theme_project()

ggtitle("Variables percentage contribution of first Principal Components")

# Define arrow style for rotation matrix
arrow_style <- arrow(angle = 20, 
                     ends = "first",
                     type = "closed",
                     length = grid::unit(8, "pt"))

# Plot rotation matrix
rotation_population <- PCA_fit_population %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1,
             PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02, 
            color = "#904C2F") +
  xlim(-1.25, .5) +
  ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

# Plot PC1 vs PC2
PCA_population <- PCA_fit_population %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Population)) + 
  geom_point(size = 1.5) +
  theme_project()

# PCA Sex----------------------------------------------------------------------
# The aim of this section is to understand if it's possible to cluster
# the two genders thanks to the information included in the other variables

# Model PCA
PCA_fit_sex <- PCA_data %>%
  select(-matches("F|M")) %>%  #Do other variables include this info?
  prcomp(scale = TRUE)

# Explained variance
PCA_sex_explvar <- PCA_fit_sex %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) +
  theme_project()

# Variables contribution
PCA_sex_contribution <- fviz_contrib(PCA_fit_sex,
                                            "var",
                                            axes = 1,
                                            xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x() +
  theme_project()

ggtitle("Variables percentage contribution of first Principal Components")


# Plot rotation matrix
rotation_sex <- PCA_fit_sex %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1,
             PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02, 
            color = "#904C2F") +
  xlim(-1.25, .5) +
  ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

# Plot PC1 vs PC2
PCA_sex <- PCA_fit_sex %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Sex)) + 
  geom_point(size = 1.5) +
  theme_project()

# PCA Distance----------------------------------------------------------------------
#The aim of this section is to understand if it's possible to cluster
#the two distance classes thanks to the information included in the other variables

# Model PCA
PCA_fit_distance <- PCA_data %>%
  select(-matches("distance")) %>%  #Do other variables include this info?
  prcomp(scale = TRUE)

# Explained variance
PCA_distance_explvar <- PCA_fit_distance %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) +
  theme_project()

# Variables contribution
PCA_distance_contribution <- fviz_contrib(PCA_fit_distance,
                                     "var",
                                     axes = 1,
                                     xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x() +
  theme_project()

ggtitle("Variables percentage contribution of first Principal Components")

# Plot rotation matrix
rotation_distance <- PCA_fit_distance %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1,
             PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02, 
            color = "#904C2F") +
  xlim(-1.25, .5) +
  ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

# Plot PC1 vs PC2
PCA_distance <- PCA_fit_distance %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Sex)) + 
  geom_point(size = 1.5) +
  theme_project()


# PCA with only genes----------------------------------------------------------------------

# Maybe gene's expression levels could tell us something about the various
# classes even though we don't have them for many samples
# We have already analized the other variables with PCA
# So let's build a dataset with only gene expressions

PCA_data_genes <- my_data_clean_aug %>% 
  na.omit() %>% 
  select(matches("Gene")) %>% 
  as_tibble()

# Model PCA
PCA_genes_fit <- PCA_data_genes %>%
  prcomp(scale = TRUE)

# Explained variance
PCA_genes_explvar <- PCA_genes_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) +
  theme_project()

# Variables contribution
PCA_genes_contribution <- fviz_contrib(PCA_genes_fit,
                                            "var",
                                            axes = 1,
                                            xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x() +
  theme_project()

ggtitle("Variables percentage contribution of first Principal Components")

# Plot rotation matrix
rotation_genes <- PCA_genes_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1,
             PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02, 
            color = "#904C2F") +
  xlim(-1.25, .5) +
  ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

# PC1 vs PC2 Population
my_data_clean_aug_na <- my_data_clean_aug %>% 
  na.omit() # Need to fit dimensions to augment in next functions

PCA_genes_population <- PCA_genes_fit %>%
  augment(my_data_clean_aug_na) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Population)) + 
  geom_point(size = 1.5) +
  theme_project()

# PC1 vs PC2 Sex
PCA_genes_sex <- PCA_genes_fit %>%
  augment(my_data_clean_aug_na) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Sex)) + 
  geom_point(size = 1.5) +
  theme_project()

# PC1 vs PC2 Distance class
PCA_genes_distance <- PCA_genes_fit %>%
  augment(my_data_clean_aug_na) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = distance_class)) + 
  geom_point(size = 1.5) +
  theme_project()

# PCA without genes, other variables----------------------------------------------------------------------

# Genes were removed because the expression levels were not calculated for 
# most observations

# Similar to the -PCA without genes- chapter, but with "combined" variables
# insted of "basic" ones

PCA_data <- my_data_clean_aug %>% 
  select(-matches("ID|Gene|time.sec|time.min|PC|efficiency|distance_class|premass|postmass|distance|power")) %>% 
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
# The aim of this section is to understand if it's possible to cluster
# the two populations thanks to the information included in the other variables

# Model PCA
PCA_fit_population <- PCA_data %>%
  select(-matches("east|west")) %>%  #Do other variables include this info?
  prcomp(scale = TRUE)

# Explained variance
PCA_population_explvar <- PCA_fit_population %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) +
  theme_project()

# Variables contribution
PCA_population_contribution <- fviz_contrib(PCA_fit_population,
                                            "var",
                                            axes = 1,
                                            xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x() +
  theme_project()

ggtitle("Variables percentage contribution of first Principal Components")

# Plot rotation matrix
rotation_population_1 <- PCA_fit_population %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1,
             PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02, 
            color = "#904C2F") +
  xlim(-1.25, .5) +
  ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

# Plot PC1 vs PC2
PCA_population <- PCA_fit_population %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Population)) + 
  geom_point(size = 1.5) +
  theme_project()

# PCA Sex----------------------------------------------------------------------
# The aim of this section is to understand if it's possible to cluster
# the two genders thanks to the information included in the other variables

# Model PCA
PCA_fit_sex <- PCA_data %>%
  select(-matches("F|M")) %>%  #Do other variables include this info?
  prcomp(scale = TRUE)

# Explained variance
PCA_sex_explvar <- PCA_fit_sex %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) +
  theme_project()

# Variables contribution
PCA_sex_contribution <- fviz_contrib(PCA_fit_sex,
                                     "var",
                                     axes = 1,
                                     xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x() +
  theme_project()

ggtitle("Variables percentage contribution of first Principal Components")

# Plot rotation matrix
rotation_sex_1 <- PCA_fit_sex %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1,
             PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02, 
            color = "#904C2F") +
  xlim(-1.25, .5) +
  ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

# Plot PC1 vs PC2
PCA_sex <- PCA_fit_sex %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Sex)) + 
  geom_point(size = 1.5) +
  theme_project()

# PCA Distance----------------------------------------------------------------------
#The aim of this section is to understand if it's possible to cluster
#the two distance classes thanks to the information included in the other variables

# Model PCA
PCA_fit_distance <- PCA_data %>%
  select(-matches("distance")) %>%  #Do other variables include this info?
  prcomp(scale = TRUE)

# Explained variance
PCA_distance_explvar <- PCA_fit_distance %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) +
  theme_project()

# Variables contribution
PCA_distance_contribution <- fviz_contrib(PCA_fit_distance,
                                          "var",
                                          axes = 1,
                                          xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x() +
  theme_project()

ggtitle("Variables percentage contribution of first Principal Components")

# Plot rotation matrix
rotation_distance_1 <- PCA_fit_distance %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1,
             PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02, 
            color = "#904C2F") +
  xlim(-1.25, .5) +
  ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

# Plot PC1 vs PC2
PCA_distance <- PCA_fit_distance %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Sex)) + 
  geom_point(size = 1.5) +
  theme_project()
