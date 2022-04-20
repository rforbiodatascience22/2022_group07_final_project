# Define project functions ------------------------------------------------
foo <- function(x){
  return(2*x)
}

bar <- function(x){
  return(x^2)
}


#, "Std", "min", "25%", "50%", "75%", "Max"
describe <- function(dataset){#NOTWORKING
  
  stats <- c("Count", "Mean" )
  sumstat <- data.frame()
  
  tempstat <- dataset %>%
    summarise(across(
      .cols = is.numeric,
      .funs = median, na.rm = TRUE,
      .names = "{col}"
    ))

  sumstat <- tempstat %>%
    bind_rows(sumstat)
  
  # tempstat <- dataset %>%
  #   summarise(across(
  #     .cols = is.numeric, 
  #     .fns = mean, na.rm = TRUE, 
  #     .names = "{col}"
  #   ))
  # 
  # sumstat <- tempstat %>%
  #   bind_rows(sumstat)
  
  # sumstat <- cbind(row.names = stats, sumstat)
  
  return(sumstat)
}


plot_distribution <- function(data, column) {
  
  if (is.numeric(data[[column]])){
    ggplot(data, aes_string(x = column)) +
    geom_density(fill = "lightgreen") +
    xlab(column)
  }
  
  else{
    ggplot(data, aes_string(x = column)) +
    geom_bar() +
    xlab(column)
  }
  
}

data_distributions <- function(data){
  my_plotlist <- lapply(colnames(data), plot_distribution, data = data)
  plots <- ggarrange(plotlist=my_plotlist, nrow = 3, ncol = 3)
  ggexport(plots, filename = "results/Distributions.pdf")
}
