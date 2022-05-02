
# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------

#Boxplot of the variable power for the different populations, divided by sex
power <- my_data_clean_aug %>% 
            ggplot(aes(x = Sex,
                       y = power,
                       fill = Sex))+
            geom_boxplot()+
            facet_wrap(~ Population)+
            labs(x = "Sex",
                 y = "Power",
                 title = "Power of the different populations")+
            theme_project()+
            scale_fill_project()
     


##Boxplots for the different gene expressions
express <- my_data_clean_aug %>%            #subset to plot the expressions easier
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
##############
##should I do all in a %>% in order not to create the variable 'express'??

gene_expr <- express %>%
               ggplot(aes(x = Population,
                          y = Expression,
                          fill = Sex))+
               geom_boxplot()+
               facet_grid(~ Genes)+    
               labs(y = "Gene Expression",
                    x = "Population",
                    title = "Gene expressions")+
               theme_project()+
               scale_fill_project()


##Histograms
##Comparison of the efficiency between sex of populations

effic<- ggplot(data = my_data_clean_aug) +
           geom_bar(aes(x = Population,
                        y = efficiency,
                        fill = Sex),
                    width = .7,
                    stat = "identity",
                    position = position_dodge(0.7)) +
           labs(y = "Efficiency",
                x = "Population",
                title = "Efficiency")+
           theme_project()+
           scale_fill_project()

##Comparison of the distance flown by the butterflies splitted by sex and population
dist <- ggplot(data = my_data_clean_aug) +
          geom_bar(aes(x = Population,
                       y = distance,
                       fill = Sex),
                    width = .7,
                    stat = "identity",
                    position = position_dodge(0.7)) +
           labs(y = "Distance (m)",
                x = "Population",
                title = "Distances flown")+
           theme_project()+
           scale_fill_project()

##Comparison of the velocities of the butterflies splitted by sex and population
vel <- ggplot(data = my_data_clean_aug) +
          geom_bar(aes(x = Population,
                       y = averagevelocity,
                       fill = Sex),
                   width = .7,
                   stat = "identity",
                   position = position_dodge(0.7)) +
          labs(y = "Speed (m/s)",
               x = "Population",
               title = "Speed")+
          theme_project()+
          scale_fill_project()

##Comparison of the energy consumed by the butterflies   
energy <- ggplot(data = my_data_clean_aug) +
           geom_bar(aes(x = Population,
                        y = energy_consumed,
                        fill = Sex),
                     width = .7,
                     stat = "identity",
                     position = position_dodge(0.7)) +
          labs(y = "Energy consumed (J)",
               x = "Population",
               title = "Energy consumption")+
          theme_project()+
          scale_fill_project()

#Plot all the variable together 
plots <- ggarrange(effic,
                   dist,
                   vel,
                   energy,
                   ncol = 2,
                   nrow = 2)
annotate_figure(plots, 
                top = text_grob("Comparison between populations and sex", 
                                color = "black", 
                                face = "bold", 
                                size = 14))


#------------keep or not to keep?----------------------------------

##Comparison between premass and postmass
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

##############
##should I do all in a %>% in order not to create the variable 'mass'??
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
          fill = "State of mass")+
     theme_project()+
     scale_fill_project()

#------------------------------------------------------------------


##Heatmap - Genes Expression and ID (divided by population)

#Heatmap for east population
heat_east <- express %>% 
              filter(Population == "east") %>% 
               ggplot(aes(x = ID,
                          y = Genes,
                          fill = Expression)) +
                geom_tile(lwd = .9,
                          linetype = 1) +
                 labs(title = "East population",
                      y = "Genes",
                      x = "ID")+
                scale_fill_viridis_c(option = "B", direction = -1) +
                theme_project()

#Heatmap for west population
heat_west <- express %>% 
                filter(Population == "west") %>% 
                 ggplot(aes(x = ID,
                            y = Genes,
                            fill = Expression)) +
                 geom_tile(lwd = .9,
                           linetype = 1) +
                 labs(title = "West population",
                      y = "Genes",
                      x = "ID")+
                 scale_fill_viridis_c(option = "B", direction = -1) +
                 theme_project()


heat_maps <- ggarrange(heat_east,
                       heat_west,
                       ncol = 1,
                       nrow = 2)
annotate_figure(plots, 
                top = text_grob("Heatmaps for genes expression", 
                                color = "black", 
                                face = "bold", 
                                size = 14))


##Density ridge for energy consumed
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


##Density ridge for efficiency
density_effic <- ggplot(my_data_clean_aug,
                        aes(x = efficiency,
                            y = Population,
                            fill= Population)) +
                 geom_density_ridges() +
                 labs(title = "Density chart of efficiency",
                      x = "Efficiency",
                      y = "Population Identified") +
                 theme_project() +
                 scale_fill_project()


## plots to save

plots_analysis <- c(power,
                    gene_expr,
                    heat_maps,
                    density_energy,
                    density_effic)

#map(names(plots_analysis), ~save_plot_list("04_", plots_analysis, .x)) 


# Write data --------------------------------------------------------------
write_tsv(x = express,
          file = "data/04_gene_expr_data.tsv")
