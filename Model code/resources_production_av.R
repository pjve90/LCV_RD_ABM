#' Average amount of resources produced.
#'
#' \code{produce_av} calculates the average amount of resources produced by an individual.
#' 
#' The average amount of resources produces by an individual throughout her whole lifespan is calculated by the average of produced resources in each iteration.

produce_av <- function(final_ind_data){
  final_ind_data$prod_av[i] <- mean(as.numeric(it_data[i,grep("prod_a",colnames(it_data))]),na.rm = T)
  return(final_ind_data$prod_av)
}
