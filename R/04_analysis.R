
# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv",
                              show_col_types = FALSE)


# Wrangle data ------------------------------------------------------------

#Boxplot of the variable power for the different populations, divided by sex
power <- my_data_clean_aug %>% 
  ggplot(aes(x = Sex,
             y = power,
             fill = Sex)) +
  geom_boxplot()+
  facet_wrap(~ Population) +
  labs(x = "Sex",
       y = "Power",
       title = "Power of the different populations") +
  theme_project() +
  scale_fill_project()
     
ggsave("04_power_boxplot.png",
       path = image_path,
       device = "png")

#Create a new variable 'express' which consider ID, Sex and Population just for 
#the butterflies from which we have the gene expressions.
#Create only one column for the value of the expressions and one for indicating 
#which gene is referred to
express <- my_data_clean_aug %>% #
  select(ID,
         Sex,
         Population,
         Gene_379,
         Gene_203,
         Gene_C2,
         Gene_C4,
         Gene_C5,
         Gene_C7) %>%
  drop_na() %>% 
  pivot_longer(cols = starts_with("Gene_"),
               names_to = "Genes",
               names_prefix = "Gene_",
               values_to = "Expression")

#Boxplots for the all of the different gene expressions divided by populations
#and sex
gene_expr <- express %>%
  ggplot(aes(x = Population,
             y = Expression,
             fill = Sex)) +
  geom_boxplot() +
  facet_grid(~ Genes) +
  labs(y = "Gene Expression",
       x = "Population",
       title = "Gene expressions") +
  theme_project() +
  scale_fill_project()

ggsave("04_gene_expression.png",
       path = image_path,
       device = "png")

##Histogram plots:
#Comparison of the efficiency between butterflies of the two population and 
#divided by sex
effic<- ggplot(data = my_data_clean_aug) +
  geom_bar(aes(x = Population,
               y = efficiency,
               fill = Sex),
           width = .7,
           stat = "identity",
           position = position_dodge(0.7)) +
  labs(y = "Efficiency",
       x = "Population",
       title = "Efficiency") +
  theme_project() +
  scale_fill_project()

#Comparison of the distance flown by butterflies of the two population and 
#divided by sex
dist <- ggplot(data = my_data_clean_aug) +
  geom_bar(aes(x = Population,
               y = distance,
               fill = Sex),
           width = .7,
           stat = "identity",
           position = position_dodge(0.7)) +
  labs(y = "Distance (m)",
       x = "Population",
       title = "Distances flown") +
  theme_project() +
  scale_fill_project()

#Comparison of the average speed sustained by the butterflies of the two 
#population and divided by sex
vel <- ggplot(data = my_data_clean_aug) +
  geom_bar(aes(x = Population,
               y = averagevelocity,
               fill = Sex),
           width = .7,
           stat = "identity",
           position = position_dodge(0.7)) +
  labs(y = "Speed (m/s)",
       x = "Population",
       title = "Speed") +
  theme_project() +
  scale_fill_project()

#Comparison of the energy consumed by the butterflies of the two population 
#and divided by sex 
energy <- ggplot(data = my_data_clean_aug) +
  geom_bar(aes(x = Population,
               y = energy_consumed,
               fill = Sex),
           width = .7,
           stat = "identity",
           position = position_dodge(0.7)) +
  labs(y = "Energy consumed (J)",
       x = "Population",
       title = "Energy consumption") +
  theme_project() +
  scale_fill_project()

#Plot all the variables together 
plots <- ggarrange(effic,
                   dist,
                   vel,
                   energy,
                   ncol = 2,
                   nrow = 2)
annotate_figure(plots,
                top = text_grob("Fligth variables between populations and sex",
                                color = "black", 
                                face = "bold", 
                                size = 14))
ggsave("04_flight_var.png",
       path = image_path,
       device = "png")


#Create a new variable 'mass' which consider ID, Sex and Population for all the 
#butterflies of the dataset.
#Create only one column for the value of the mass registered and one for 
#indicating the state of mass: 'pre' or 'post' experiment.
mass <- my_data_clean_aug %>%
  select(ID,
         Sex,
         Population,
         premass,
         postmass) %>%
  pivot_longer(cols = ends_with("mass"),
               names_to = "State_of_mass",
               names_prefix = "mass",
               values_to = "Mass") 

#Histograms to compare premass and postmass of the butterflies of the two 
#population and divided by sex 
 mass %>%
  ggplot(aes(x=Population,
             y = Mass,
             fill = State_of_mass)) +
  geom_bar(width=.7,
           stat = "identity",
           position = position_dodge(0.7)) +
  labs(y = "Mass (g)",
       x = "Population",
       title = "Pre-mass and Post-mass copmarison between population",
       fill = "State of mass") +
  theme_project() +
  scale_fill_project()


#Heatmaps for Genes Expression related to the different IDs divided by 
#population

#Heatmap for east population
heat_east <- express %>%
  filter(Population == "east") %>%
  ggplot(aes(x = Genes,
             y = ID,
             fill = Expression)) +
  geom_tile(lwd = .9,
            linetype = 1) +
  labs(title = "East population",
       y = "ID",
       x = "Genes") +
  scale_fill_viridis_c(option = "B", direction = -1) +
  theme_project()

#Heatmap for west population
heat_west <- express %>% 
  filter(Population == "west") %>% 
  ggplot(aes(x = Genes,
             y = ID,
             fill = Expression)) +
  geom_tile(lwd = .9,
            linetype = 1) +
  labs(title = "West population",
       y = "ID",
       x = "Genes") +
  scale_fill_viridis_c(option = "B", direction = -1) +
  theme_project()

#Plot the two heatmaps together in order to compare them in one single figure
heat_maps <- ggarrange(heat_east,
                       heat_west,
                       ncol = 1,
                       nrow = 2) %>% 
  annotate_figure(plots, 
                  top = text_grob("Heatmaps for genes expression",
                                  color = "black",
                                  face = "bold",
                                  size = 14))

ggsave("04_heat_maps.png",
       path = image_path,
       device = "png")

##Density ridge for energy consumed by the butterflies of the two population
density_energy <- ggplot(my_data_clean_aug,
                         aes(x = energy_consumed,
                             y = Population,
                             fill= Population)) +
  geom_density_ridges() +
  labs(title = "Density chart of energy consumed",
       x = "Energy consumed",
       y = "Population Identified") +
  theme_project() +
  scale_fill_project()

ggsave("04_density_energy.png",
       path = image_path,
       device = "png")

##Density ridge for efficiency of the butterflies of the two population
density_effic <- ggplot(my_data_clean_aug,
                        aes(x = efficiency,
                            y = Population,
                            fill= Population)) +
  geom_density_ridges() +
  labs(title = "Density chart of efficiency",
       x = "Efficiency",
       y = "Population Identified") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_project() 

ggsave("04_density_efficiency.png",
       path = image_path,
       device = "png")

##Create plots from figure 2 in the paper

#flight duration plot: data distribution jitter and average value for the two populations, splitted by sex
#dataset with time means to use in the plot
time_mean <- my_data_clean_aug %>% 
  group_by(Sex, Population) %>% 
  summarise(average_time = mean(time.min))

flight_duration <- ggplot(data = my_data_clean_aug,
                          mapping = aes(y = time.min, 
                                        x = Sex,
                                        color = Population)) +
  geom_jitter(width = 0.2) +
  facet_grid(cols = vars(Population), 
             switch = "x") +
  geom_crossbar(data = time_mean, mapping = aes(y = average_time,
                                                ymin = average_time, 
                                                ymax = average_time)) +
  theme_minimal() +
  theme(panel.spacing = unit(0, "cm"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(title="Flight duration",
       x="",
       y="time (min)") + 
  scale_colour_brewer(palette = "Set2") 

#flight distance plot: data distribution jitter and average value for the two populations, splitted by sex
#dataset with time means to use in the plot
distance_mean <- my_data_clean_aug %>% 
  group_by(Sex,Population) %>% 
  summarise (average_distance = mean(distance))

flight_distance <- ggplot(data = my_data_clean_aug,
                          mapping = aes(y = distance, 
                                        x = Sex,
                                        color = Population)) +
  geom_jitter(width = 0.2) +
  facet_grid(cols = vars(Population), 
             switch = "x") +
  geom_crossbar(data = distance_mean, mapping = aes(y = average_distance,
                                                    ymin = average_distance, 
                                                    ymax = average_distance)) +
  theme_minimal() +
  theme(panel.spacing = unit(0, "cm"), 
        strip.placement = "outside", 
        strip.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(title="Flight distance",
       x="",
       y=" Flight distance (km)") + 
  scale_colour_brewer(palette = "Set2") 

#flight power plot: data distribution jitter and average value for the two populations, splitted by sex
#dataset with time means to use in the plot
power_mean <- my_data_clean_aug %>% 
  group_by(Sex, Population) %>% 
  summarise(average_power = mean(power))

flight_power <- ggplot(data = my_data_clean_aug,
                       mapping = aes(y = power, 
                                     x = Sex,
                                     color = Population)) +
  geom_jitter(width = 0.2) +
  facet_grid(cols = vars(Population), 
             switch = "x") +
  geom_crossbar(data = power_mean, mapping = aes(y = average_power,
                                                 ymin = average_power,
                                                 ymax = average_power)) +
  theme_minimal() +
  theme(panel.spacing = unit(0, "cm"), 
        strip.placement = "outside", 
        strip.text.x = element_text(size = 9),
        plot.title = element_text(hjust = 0.5)) +
  labs(title="Flight power",
       x="",
       y="power (W)") + 
  scale_colour_brewer(palette = "Set2") 

#patchwork of plots to represent "Flight performance":
flight_performance_plots <- flight_distance + 
  flight_duration +  
  flight_power +  
  density_effic

ggsave("04_flight_performance.png",
       path = image_path,
       device = "png",
       height = 7,
       width = 7,
       unit = "in")




# Write data --------------------------------------------------------------
write_tsv(x = express,
          file = "data/04_gene_expr_data.tsv")
