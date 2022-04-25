library(ggthemes)

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

datadistribution_plot <- function(x, y, df){
  ggplot(df, aes(x = .data[[x]], y = .data[[y]])) + 
    geom_violin() +
    theme_project() +
    geom_boxplot(width=0.1)
}

