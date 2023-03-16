#' Variability of stored resources.
#'
#' \code{storage_cv} calculates the variability of stored resources of an individual.
#' 
#' The variability of stored resources of an individual throughout her whole lifespan is calculated by the coefficient of variation of stored resources in each iteration.

storage_cv <- function(final_ind_data){
  final_ind_data$store_cv[i] <- final_ind_data$store_av[i]/sd(it_dataf[which(it_dataf$id==i),"store_a"])
  return(final_ind_data$store_cv)
}
