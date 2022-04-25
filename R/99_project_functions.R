library(ggthemes)

plot_distribution <- function(data, column) {
  
  if (is.numeric(data[[column]])){
    ggplot(data, aes_string(x = column)) +
      geom_density(fill = "#00AFBB") +
      theme(panel.background = element_blank(),
            panel.grid = ) +
      xlab(column) +
      theme_project()
      
  }
  
  else{
    ggplot(data, aes_string(x = column)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45,
                                       size = 5,
                                       hjust = 1)) +
      xlab(column) +
      theme_project() +
      scale_color_viridis(option = "D") +
      rotate_x()
  }
  
}

data_distributions <- function(data){
  my_plotlist <- lapply(colnames(data), plot_distribution, data = data)
  plots <- ggarrange(plotlist=my_plotlist, nrow = 3, ncol = 3)
  ggexport(plots, filename = "results/Distributions.pdf")
}


plot_boxplot <- function(data, column, x){
  if (is.numeric(data[[column]])){
    
    ggplot(data,
           mapping = aes_string(x = x,
                                y = column)) +
      geom_boxplot() +
      theme_project()
  }
}

data_boxplots <- function(data){
  my_plotlist <- lapply(colnames(data), plot_box_dot, data = data, x = "Sex")
  my_plotlist <- append(my_plotlist, lapply(colnames(data), plot_box_dot, data = data, x = "Population"))
  plots <- ggarrange(plotlist=my_plotlist, nrow = 3, ncol = 3)
  ggexport(plots, filename = "results/Boxplots.pdf")
}

# To check for and summarize NA values in a dataframe
na_count <- function(df){
  df %>% 
    summarise_all(~ sum(is.na(.)))
}

# Common theme to be used in all figures in the project
theme_project <- function(){
  theme_gdocs(base_size = 7)
}

# To rotate x-axis labels by 45 deg
rotate_x <- function(){
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
}

# To rotate y-axis labels by 45 deg
rotate_y <- function(){
  theme(axis.text.y = element_text(angle = 45,
                                   hjust = 1))
}
