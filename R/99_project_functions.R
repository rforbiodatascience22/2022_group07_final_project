# Define project functions ------------------------------------------------
# Strings cannot be used directly in aes(), but can be used with the .data pronoun.
datadistribution_plot <- function(x,y){
  ggplot(my_data_clean, aes(x = .data[[x]], y = .data[[y]])) + 
    geom_violin() + #alternatively jitter
    geom_boxplot(width=0.1)
}

...