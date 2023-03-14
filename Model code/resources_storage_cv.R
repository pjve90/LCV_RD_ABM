#' Variability of stored resources.
#'
#' \code{storage_cv} calculates the variability of stored resources of an individual.
#' 
#' The variability of stored resources of an individual throughout her whole lifespan is calculated by the coefficient of variation of stored resources in each iteration.

storage_cv <- function(final_ind_data){
  final_ind_data$store_cv[i] <- final_ind_data$store_av[i]/sd(as.numeric(it_data[i,grep("store_a",colnames(it_data))]),na.rm = T)
  return(final_ind_data$store_cv)
}
