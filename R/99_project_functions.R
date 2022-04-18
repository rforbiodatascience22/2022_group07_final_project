# Define project functions ------------------------------------------------

# To check for and summarize NA values in a dataframe
na_count <- function(df){
    df %>% 
    summarise_all(~ sum(is.na(.)))
}

bar <- function(x){
  return(x^2)
}