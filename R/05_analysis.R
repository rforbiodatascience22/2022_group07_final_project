# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")
gene_expr_data <- read_tsv(file = "data/04_gene_expr_data.tsv")

# Gene Expression analysis -------------------------------------------------

# Wrangle data ------------------------------------------------------------
#creating tibble for gene expression analysis
gene_expr <- gene_expr_data %>%
  select(-matches("ID|Sex")) %>% 
  mutate(Population = case_when(Population == "east" ~ 0,
                                Population == "west" ~ 1)) %>% 
  group_by(Genes) %>%
  nest %>% 
  ungroup 

# Model data -------------------------------------------------------------
#logistic regression model for correlation of gene expression and population
gene_expr_model <- gene_expr %>% 
  mutate(mdl = map(data,
                   ~glm(Population ~ expr_lvl, 
                        data = .x, 
                        family = binomial(link = "logit")))) %>% 
  mutate(mdl_tidy = map(mdl,
                        ~tidy(.x,
                              conf.int = TRUE))) %>% 
  unnest(mdl_tidy) %>% 
  filter(str_detect(term, "expr_lvl"))

# Analysis -----------------------------------------------------------------
#creating labels based on significance of analysis
gene_expr_analysis <- gene_expr_model %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"), 
         gene_label = case_when(identified_as == "Significant" ~ gene,
                                identified_as == "Non-significant" ~ "")) %>% 
  mutate(neg_log10_p = -log10(p.value))

# Visualize ---------------------------------------------------------------
#plotting the significance values
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

ggsave("05_gene_expression.png",
       path = image_path,
       device = "png")

----------------------------------------------------------------------------

# PCA ----------------------------------------------------------------------

# Remove all but numeric variables and keep only necessary variables
PCA_data <- my_data_clean_aug %>% 
  select(-(matches("energy|Gene|eff|time.sec|power|maxvelocity|ID|distance_class"))) %>% 
  as_tibble()

# One hot encoding
final_data <- PCA_data %>%
  mutate(value = 1)  %>%
  spread(Sex,
         value,
         fill = 0 ) %>%
  mutate(value = 1) %>%
  spread(Population,
         value,
         fill = 0 )

# Model data---------------------------------------------------------------------

# Perform PCA
pca_fit <- final_data %>%
  prcomp(scale = TRUE)

# Perform K-nearest Neighbors
kmean <- pca_fit$x %>%
  kmeans(centers = 2,
         iter.max = 1000,
         nstart = 10) %>%
  augment(final_data)


# Visualize data ----------------------------------------------------------

# Eigenvalues percentage plot (data explained)
pl1 <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0,
                                                 0.01))) 
ggsave("05_data_exlained_PCs.png",
       path = image_path,
       device = "png")

# Contribution of each variable to PC
var_contr <- get_pca_var(pca_fit)
pl5 <- fviz_contrib(pca_fit,
                    "var",
                    axes = 1,
                    xtickslab.rt = 90) + 
  theme_minimal() +
  rotate_x()
  ggtitle("Variables percentage contribution of first Principal Components")

ggsave("05_data_contribution_PCs.png",
       path = image_path,
       device = "png")

# PC1 vs PC2 Plot - Population/Sex/distance_class
pl2 <- pca_fit %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Population)) + 
  geom_point(size = 1.5)
ggsave("05_PCA_population.png",
       path = image_path,
       device = "png")

pl3 <- pca_fit %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = Sex)) + 
  geom_point(size = 1.5)
ggsave("05_PCA_sex.png",
       path = image_path,
       device = "png")

pl4 <- pca_fit %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = distance_class)) + 
  geom_point(size = 1.5)

ggsave("05_PCA_distanceclass.png",
       path = image_path,
       device = "png")


# Plot rotation matrix

# Define arrow style for plotting
arrow_style <- arrow(angle = 20, 
                     ends = "first",
                     type = "closed",
                     length = grid::unit(8, "pt"))

# Plot rotation matrix
rotation_matrix <- pca_fit %>%
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

ggsave("05_PCA_rotationmatrix.png",
       path = image_path,
       device = "png")

# Find optimal number of clusters
pca_fit %>% 
  tidy() %>% 
  fviz_nbclust(FUNcluster = kmeans, 
               k.max = 8)
ggsave("05_optimal_clusters.png",
       path = image_path,
       device = "png")

# KNN - clusters plot
pl5 <- pca_fit %>%
  augment(kmean) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = .cluster)) + 
  geom_point(size = 1.5)

ggsave("05_PCA_KNN.png",
       path = image_path,
       device = "png")
