#' Variability of resources produced.
#'
#' \code{produce_cv} calculates the variability of resources produced by an individual.
#' 
#' The variability of resources produced by an individual throughout her whole lifespan is calculated by the coefficient of variation of resources produced in each iteration.

produce_cv <- function(final_ind_data){
  final_ind_data$prod_cv[i] <- final_ind_data$prod_av[i]/sd(as.numeric(it_data[i,grep("prod_a",colnames(it_data))]),na.rm = T)
  return(final_ind_data$prod_cv)
}
