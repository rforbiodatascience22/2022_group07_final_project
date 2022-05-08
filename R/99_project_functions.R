
# To check for and summarize NA values in a dataframe
na_count <- function(df) {
  df %>% 
    summarise_all(~ sum(is.na(.)))
}


# Common theme to be used in all figures in the project

theme_project <- function() {
  theme_gdocs(base_size = 15)
}

scale_colour_project <- function() {
  scale_colour_brewer(palette = "Set2")
}

scale_fill_project <- function() {
  scale_fill_brewer(palette = "Set2")
}

# To rotate x-axis labels by 45 deg
rotate_x <- function() {
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
}

# To rotate y-axis labels by 45 deg
rotate_y <- function() {
  theme(axis.text.y = element_text(angle = 45,
                                   hjust = 1))
}



#function for plotting distribution of variables
datadistribution_plot <- function(x, y, df){
  ggplot(df, aes(x = .data[[x]], 
                 y = .data[[y]],
                 fill = Population)) + 
    geom_violin() +
    geom_boxplot(aes(x = .data[[x]], 
                   y = .data[[y]]), 
               width = 0.3) +
    theme_project() +
    scale_fill_project()
}


#function to save plots in images to plots folder in results 

folder <- "results/plots"

if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder)
}

image_path = "results/plots"
  
save_plot_list <- function(prefix, plot_list, .x) {
  ggsave(filename = paste0(prefix, .x, ".png"),
         path = image_path,
         plot = plot_list[[.x]],
         height = 7,
         width = 7,
         unit = "in")
}