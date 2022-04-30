# Load libraries ----------------------------------------------------------
library("readr")
library("tibble")
library("tidyverse")
library("caret")
library("broom")
library("factoextra")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv", 
                              show_col_types = FALSE)


# Wrangle data ------------------------------------------------------------

#Removing all but numeric variables (keeping only necessary variables)
PCA_data <- my_data_clean_aug %>% 
  select(-(matches("energy|Log|eff|time.sec|power|maxvelocity|ID|distance_class"))) %>% 
  as_tibble()

#define one-hot encoding function
dummy <- dummyVars(" ~ .",
                   data = PCA_data)

#perform one-hot encoding on data frame
final_data <- data.frame(predict(dummy,
                                 newdata = PCA_data))


# Model data---------------------------------------------------------------------

#Perform PCA
pca_fit <- final_data %>%
  prcomp(scale = TRUE)

#PCs stats (std/percent/cummulative)
pca_fit %>% 
  tidy(matrix = "eigenvalues")

#Rotation matrix
pca_fit %>%
  tidy(matrix = "rotation")

#KNN
kmean <- pca_fit$x %>%
  kmeans(centers = 2,
         iter.max = 1000,
         nstart = 10) %>%
  augment(final_data)


# Visualise data ----------------------------------------------------------


#Eigenvalues percentage plot
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

#Contribution of each variable to PC(needs change to ggplot instead of plot)
var <- get_pca_var(pca_fit)
pl5 <- fviz_contrib(pca_fit,
                "var",
                axes = 1,
                xtickslab.rt = 90)+ 
  theme_minimal()+ 
  ggtitle("Variables percentage contribution of first Principal Components")


#PC1 VS PC2 Plot - Population/Sex/distance_class
pl2 <- pca_fit %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2, 
             color = Population)) + 
  geom_point(size = 1.5)

pl3 <- pca_fit %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = Sex)) + 
  geom_point(size = 1.5)

pl4 <- pca_fit %>%
  augment(my_data_clean_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = distance_class)) + 
  geom_point(size = 1.5)


#Plot rotation matrix(arrows)

# define arrow style for plotting
arrow_style <- arrow(angle = 20, 
                     ends = "first",
                     type = "closed",
                     length = grid::unit(8,
                                         "pt"))

# plot rotation matrix
pca_fit %>%
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

#Finding optiml number of clusters
pca_fit$x %>% 
  fviz_nbclust(FUNcluster=kmeans, k.max = 8)

#KNN - clusters plot
pl5 <- pca_fit %>%
  augment(kmean) %>% # add original dataset back in
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = .cluster)) + 
  geom_point(size = 1.5)

show(pl4)
# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)