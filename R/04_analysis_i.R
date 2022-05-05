# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Visualise data ----------------------------------------------------------


# Flight performance of eastern and western North American monarchs (Fig. 2 from the paper)
library(viridis)
library(patchwork)


#A. flight duration
time_mean <- my_data_clean_aug %>% 
  group_by(Sex,Population) %>% 
  summarise(average_time = mean(time.min))

flight_duration <- ggplot(data = my_data_clean_aug,
                          mapping = aes(y = time.min, 
                                        x = Sex,
                                        color = Sex)) +
  geom_jitter(width = 0.2) +
  facet_grid(cols = vars(Population), 
             switch = "x") +
  geom_crossbar(data = time_mean, mapping = aes(y = average_time,
                                                ymin = average_time, 
                                                ymax = average_time)) +
  theme_minimal()+
  theme(panel.spacing = unit(0, "cm"), # define the distance between the two panels
        strip.placement = "outside", # strips will remain outside the facet
        strip.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_colour_viridis_d() + #it is not possible to add scale_colour_viridis to a theme object
  labs(title="Flight duration",
       x="",
       y="time (min)")

#B. Flight distance
distance_mean <- my_data_clean_aug %>% 
  group_by(Sex,Population) %>% 
  summarise (average_distance = mean(distance))

flight_distance <- ggplot(data = my_data_clean_aug,
                          mapping = aes(y = distance, 
                                        x = Sex,
                                        color = Sex)) +
  geom_jitter(width = 0.2) +
  facet_grid(cols = vars(Population), 
             switch = "x") +
  geom_crossbar(data = distance_mean, mapping = aes(y = average_distance,
                                                    ymin = average_distance, 
                                                    ymax = average_distance)) +
  theme_minimal() +
  theme(panel.spacing = unit(0, "cm"), # define the distance between the two panels
        strip.placement = "outside", # strips will remain outside the facet
        strip.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_colour_viridis_d() + #it is not possible to add scale_colour_viridis to a theme object
  labs(title="Flight distance",
       x="",
       y=" Flight distance (km)")


#C.Flight power

power_mean <- my_data_clean_aug %>% 
  group_by(Sex,Population) %>% 
  summarise(average_power = mean (power))

flight_power <- ggplot(data = my_data_clean_aug,
                       mapping = aes(y = power, 
                                     x = Sex,
                                     color = Sex)) +
  geom_jitter(width = 0.2) +
  facet_grid(cols = vars(Population), 
             switch = "x") +
  geom_crossbar(data = power_mean, mapping = aes(y = average_power,
                                                 ymin = average_power,
                                                 ymax = average_power)) +
  theme_minimal() +
  theme(panel.spacing = unit(0, "cm"), # define the distance between the two panels
        strip.placement = "outside", # strips will remain outside the facet
        strip.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_viridis_d() + #it is not possible to add scale_colour_viridis to a theme object
  labs(title="Flight power",
       x="",
       y="Flight power (x 1000)")


FLIGHT_PERFORMANCE <- ((flight_distance/flight_duration +
                          plot_layout(guides = 'auto'))|flight_power ) +
  plot_layout(guides = 'collect')




# Write data --------------------------------------------------------------

ggsave("flight_performance.png",
       plot = FLIGHT_PERFORMANCE,
       device = "png",
       path = "results/",
       height = 12,
       width = 16,
       units = "cm"
       )

# Figure 8 from the paper ----------------------------------------------------------
my_data_clean_aug %>% ...

# Write data --------------------------------------------------------------



#Heatmap of gene expression between the two populations

subset_east <- my_data_clean_aug %>% 
  select(starts_with('Log'),ID,Population) %>% 
  drop_na() %>% 
  pivot_longer(cols = contains("Log"),
               names_to = "gene_names",
               values_to = "Log_expression") %>% 
  filter(Population == "east")


heat_map_east <-  ggplot (data = subset_east,
                          mapping = aes(x = gene_names,
                                        y = ID,
                                        fill = Log_expression)) +
  geom_tile() +
  facet_wrap( vars(Population), strip.position = "right" ) +
  scale_fill_viridis_c(option = "D", direction = -1) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size =6, hjust=1),
        legend.position= "none",
        strip.placement = "",
        axis.title = element_text(size = 8)) +
  labs(y = "Butterfly ID ",x= " ")

subset_west<- my_data_clean_aug %>% 
  select(starts_with('Log'),ID,Population) %>% 
  drop_na() %>% 
  pivot_longer(cols = contains("Log"),
               names_to = "gene_names",
               values_to = "Log_expression") %>% 
  filter(Population == "west")

heat_map_west <-  ggplot (data = subset_west,
                          mapping = aes(x = gene_names,
                                        y = ID,
                                        fill = Log_expression)) +
  geom_tile() +
  facet_wrap(vars(Population), 
              strip.position = "right" ) +
  scale_fill_viridis_c(option = "D", 
                       direction = -1) + 
  theme(axis.text.x = element_text(angle= 45),
        axis.text = element_text(size = 6, 
                                 hjust=1),
        legend.position= "bottom",
        strip.placement = "",
        axis.title = element_text(size = 8)) +
  labs(y = "Butterfly ID ",
       x = " ")

gene_expression <- (heat_map_east / heat_map_west) + 
  plot_annotation(title = "Gene expression between the two populations",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          size = 16)))

# Write data --------------------------------------------------------------

ggsave("gene_expression.png",
       plot = gene_expression,
       device = "png",
       path = "results/",
       height = 12,
       width = 16,
       units = "cm",
)